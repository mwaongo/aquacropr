#' Generate daily output column names based on total columns
#' @keywords internal
#' @noRd
.get_daily_colnames <- function(ncol_data) {

  # Fixed columns (before soil layers)
  fixed_cols_before <- c(
    'day', 'month', 'year', 'dap', 'stage', 'wcxxx', 'raina', 'irri', 'surf', 'infilt',
    'ro', 'drain', 'cr', 'zgwta', 'ex', 'e', 'eex', 'trx', 'tr', 'trtrx',
    'etx', 'et', 'etetx', 'gd', 'za', 'stexp', 'ststo', 'stsen', 'stsalt', 'stweed',
    'cc', 'ccw', 'sttr', 'kctr', 'trx2', 'tr2', 'trw', 'trtrx2', 'wp', 'biomass',
    'hi', 'ydry', 'yfresh', 'brelative', 'wpet', 'bin', 'bout',
    'wcxxx2', 'wrxxx', 'zb', 'wr', 'wrsat', 'wrfc', 'wrexp', 'wrsto', 'wrsen', 'wrpwp',
    'saltin', 'saltout', 'saltup', 'saltxxx', 'saltz', 'zc', 'ece', 'ecsw',
    'stsalt2', 'zgwtb', 'ecgw'
  )  # 71 columns

  # Fixed columns (after soil layers)
  fixed_cols_after <- c('rainb', 'eto', 'tmin', 'tavg', 'tmax', 'co2')  # 6 columns

  # Calculate number of soil layers
  # Formula: ncol_data = 71 (before) + n_layers + n_layers + 6 (after)
  # Therefore: n_layers = (ncol_data - 77) / 2

  n_fixed <- length(fixed_cols_before) + length(fixed_cols_after)
  n_soil_layers <- (ncol_data - n_fixed) / 2

  # Validate
  if (n_soil_layers != round(n_soil_layers) || n_soil_layers < 1) {
    warning(
      "Cannot determine number of soil layers from ", ncol_data, " columns\n",
      "Expected structure: 71 fixed + 2*N soil layers + 6 climate = total\n",
      "Using generic column names",
      call. = FALSE
    )
    return(paste0("col", 1:ncol_data))
  }

  if (n_soil_layers > 20) {
    warning(
      "Detected ", n_soil_layers, " soil layers, which seems unusual\n",
      "Maximum typical layers is 12. Please verify file structure.",
      call. = FALSE
    )
  }

  # Generate soil layer column names
  soil_wc_cols <- paste0('wc', sprintf('%02d', 1:n_soil_layers))
  soil_ece_cols <- paste0('ece', sprintf('%02d', 1:n_soil_layers))

  # Combine all columns
  all_cols <- c(
    fixed_cols_before,
    soil_wc_cols,
    soil_ece_cols,
    fixed_cols_after
  )

  return(all_cols)
}

#' Detect runs in daily output file
#' @keywords internal
#' @noRd
.detect_daily_runs <- function(file) {
  # Read all lines
  all_lines <- readr::read_lines(file)

  # Find "Run:" markers
  run_indices <- which(stringr::str_detect(all_lines, "^\\s*Run:"))

  if (length(run_indices) == 0) {
    stop("No 'Run:' markers found in file. Is this a valid AquaCrop daily output?",
         call. = FALSE)
  }

  # Data starts 2 lines after Run: marker
  data_starts <- run_indices + 2

  # Calculate number of rows per run
  # Data ends 2 lines before next Run: marker (or at end of file)
  data_ends <- c(run_indices[-1] - 3, length(all_lines))

  # Calculate nrows for each run
  nrows <- data_ends - data_starts + 1

  # Filter out runs with no data
  valid_runs <- nrows > 0

  if (!any(valid_runs)) {
    stop("No valid data rows found in file", call. = FALSE)
  }

  return(list(
    n_runs = sum(valid_runs),
    skip = data_starts[valid_runs],
    nrows = nrows[valid_runs]
  ))
}

#' Read AquaCrop daily output file
#'
#' Reads and parses daily output files (*Day.OUT) from AquaCrop simulations.
#' Automatically detects the number of soil layers and handles multiple runs.
#' Works with any number of soil layers (typically 1-12, but can handle more).
#'
#' @param file Character string. Path to the *Day.OUT file
#' @param add_date Logical. If TRUE, creates a date column. Default is TRUE.
#'
#' @return A tibble with daily simulation results. The number of soil-related columns
#'   (wc01-wcNN and ece01-eceNN) depends on the soil profile used in the simulation.
#'
#' Key columns include:
#' \describe{
#'   \item{site}{Site name (extracted from filename)}
#'   \item{run}{Run number}
#'   \item{date}{Date (if add_date = TRUE)}
#'   \item{day, month, year}{Day, month, year}
#'   \item{dap}{Days after planting}
#'   \item{stage}{Growth stage}
#'   \item{raina}{Rainfall in mm}
#'   \item{irri}{Irrigation in mm}
#'   \item{eto}{Reference evapotranspiration in mm}
#'   \item{e}{Soil evaporation in mm}
#'   \item{tr}{Crop transpiration in mm}
#'   \item{et}{Total evapotranspiration in mm}
#'   \item{cc}{Canopy cover in percent}
#'   \item{biomass}{Biomass in ton/ha}
#'   \item{ydry}{Dry yield in ton/ha}
#'   \item{tmin, tavg, tmax}{Temperature in Celsius}
#'   \item{wc01-wcNN}{Water content by soil layer (N = number of layers)}
#'   \item{ece01-eceNN}{Electrical conductivity by soil layer}
#' }
#'
#' @examples
#' \dontrun{
#' # Read daily outputs
#' daily <- read_daily_out("OUTP/wakandaPRMDay.OUT")
#'
#' # Check number of soil layers
#' attr(daily, "n_soil_layers")  # e.g., 5, 10, 12, etc.
#'
#' # Analyze biomass accumulation
#' library(ggplot2)
#' daily %>%
#'   filter(run == 1) %>%
#'   ggplot(aes(x = date, y = biomass)) +
#'   geom_line() +
#'   labs(title = "Biomass accumulation")
#'
#' # Analyze soil water profile (works with any number of layers)
#' daily %>%
#'   filter(run == 1, dap == 50) %>%
#'   select(date, starts_with("wc")) %>%
#'   tidyr::pivot_longer(starts_with("wc"),
#'                       names_to = "layer",
#'                       values_to = "water_content") %>%
#'   mutate(layer_num = as.numeric(stringr::str_extract(layer, "\\d+"))) %>%
#'   ggplot(aes(x = water_content, y = -layer_num)) +
#'   geom_point() + geom_path() +
#'   labs(title = "Soil water profile", y = "Layer depth")
#' }
#'
#' @export
read_daily_out <- function(file, add_date = TRUE) {

  # Validate file
  .validate_file(file, "Day.OUT")

  # Extract site name
  site_name <- basename(file) %>%
    stringr::str_replace("(?i)PRMDay\\.OUT$", "") %>%
    stringr::str_replace("(?i)PRODay\\.OUT$", "") %>%
    stringr::str_replace("(?i)Day\\.OUT$", "")

  # Detect runs and their positions
  run_info <- .detect_daily_runs(file)

  message("Found ", run_info$n_runs, " run(s) in file")

  # Read each run
  all_data <- vector("list", run_info$n_runs)

  for (i in seq_along(run_info$skip)) {

    # Read data for this run
    run_data <- tryCatch({
      readr::read_table(
        file = file,
        skip = run_info$skip[i] - 1,
        n_max = run_info$nrows[i],
        col_names = FALSE,
        col_types = readr::cols(.default = readr::col_double()),
        show_col_types = FALSE,
        na = c("", "NA", "-9", "-9.9")
      )
    }, error = function(e) {
      warning("Failed to read run ", i, ": ", conditionMessage(e), call. = FALSE)
      return(NULL)
    })

    if (!is.null(run_data)) {
      # Add run number
      run_data$run <- i
      all_data[[i]] <- run_data
    }
  }

  # Remove NULL elements
  all_data <- all_data[!sapply(all_data, is.null)]

  if (length(all_data) == 0) {
    stop("No data could be read from file", call. = FALSE)
  }

  # Combine all runs
  data <- dplyr::bind_rows(all_data)

  # Get appropriate column names (dynamically based on ncol)
  ncol_data <- ncol(data) - 1  # -1 for run column we added
  col_names <- c(.get_daily_colnames(ncol_data), "run")

  # Calculate number of soil layers for metadata
  n_soil_layers <- (ncol_data - 77) / 2

  # Set column names
  names(data) <- col_names

  # Reorder: run first
  data <- data %>%
    dplyr::select(run, dplyr::everything())

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  # Add site column
  data <- data %>%
    dplyr::mutate(site = site_name, .before = 1)

  # Add date column if requested
  if (add_date && all(c("day", "month", "year") %in% names(data))) {
    data <- data %>%
      dplyr::mutate(
        date = as.Date(paste(year, month, day, sep = "-")),
        .after = "run"
      )
  }

  # Add metadata
  version_info <- .detect_aquacrop_version(file)
  attr(data, "source_file") <- normalizePath(file, mustWork = FALSE)
  attr(data, "read_time") <- Sys.time()
  attr(data, "n_runs") <- run_info$n_runs
  attr(data, "n_soil_layers") <- as.integer(n_soil_layers)
  if (!is.null(version_info)) {
    attr(data, "aquacrop_version") <- version_info$full
  }

  # Info message about soil layers
  message("Detected ", n_soil_layers, " soil layer(s)")

  return(data)
}
