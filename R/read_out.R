#' Read AquaCrop output file (generic)
#'
#' Automatically detects the type of AquaCrop output file (seasonal or daily)
#' and calls the appropriate reading function. Supports both *season.OUT and
#' *Day.OUT files.
#'
#' @param file Character string. Path to the output file (season.OUT or Day.OUT)
#' @param type Character string. Type of output file: "auto" (default), "season", or "daily".
#'   If "auto", type is detected from filename.
#' @param ... Additional arguments passed to read_season_out() or read_daily_out()
#'
#' @return A tibble with simulation results. Structure depends on file type:
#' \itemize{
#'   \item Seasonal output: One row per run with summary statistics
#'   \item Daily output: One row per day per run with detailed daily data
#' }
#'
#' @details
#' The function detects file type in the following order:
#' \enumerate{
#'   \item If `type` is specified, uses that type
#'   \item If filename contains "season.OUT" (case-insensitive), reads as seasonal
#'   \item If filename contains "Day.OUT" (case-insensitive), reads as daily
#'   \item If detection fails, examines file content
#' }
#'
#' @examples
#' \dontrun{
#' # Automatic detection
#' season <- read_out("OUTP/wakandaPRMseason.OUT")
#' daily <- read_out("OUTP/wakandaPRMDay.OUT")
#'
#' # Explicit type
#' season <- read_out("OUTP/myfile.OUT", type = "season")
#'
#' # With additional arguments
#' season <- read_out("OUTP/wakandaPRMseason.OUT", add_dates = FALSE)
#' daily <- read_out("OUTP/wakandaPRMDay.OUT", add_date = TRUE)
#'
#' # Read multiple files
#' files <- list.files("OUTP", pattern = "\\.OUT$", full.names = TRUE)
#' all_outputs <- purrr::map_dfr(files, read_out)
#' }
#'
#' @seealso \code{\link{read_season_out}}, \code{\link{read_daily_out}}
#'
#' @export
read_out <- function(file, type = c("auto", "season", "daily"), ...) {

  type <- match.arg(type)

  # Validate file exists
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  # Detect type
  detected_type <- if (type == "auto") {
    .detect_output_type(file)
  } else {
    type
  }

  # Call appropriate function
  result <- switch(
    detected_type,
    season = {
      message("Reading as seasonal output...")
      read_season_out(file, ...)
    },
    daily = {
      message("Reading as daily output...")
      read_daily_out(file, ...)
    },
    stop("Could not determine output type. Please specify type = 'season' or 'daily'",
         call. = FALSE)
  )

  # Add output type as attribute
  attr(result, "output_type") <- detected_type

  return(result)
}

#' Detect AquaCrop output file type
#' @keywords internal
#' @noRd
.detect_output_type <- function(file) {

  filename <- basename(file)

  # Check filename patterns
  if (grepl("season\\.OUT$", filename, ignore.case = TRUE)) {
    return("season")
  }

  if (grepl("Day\\.OUT$", filename, ignore.case = TRUE)) {
    return("daily")
  }

  # If filename doesn't help, examine content
  message("Filename ambiguous, examining file content...")

  # Read first few lines
  first_lines <- readr::read_lines(file, n_max = 10)

  # Daily files have "Run:" markers
  if (any(grepl("^\\s*Run:", first_lines))) {
    message("Detected daily output (found 'Run:' markers)")
    return("daily")
  }

  # Seasonal files have fewer lines before data (4 lines)
  # and no "Run:" markers
  # Try to count header lines
  if (length(first_lines) >= 4) {
    # Check if line 4 looks like a header
    line4 <- first_lines[4]
    if (grepl("mm|ton|%|ppm", line4, ignore.case = TRUE)) {
      message("Detected seasonal output (header structure)")
      return("season")
    }
  }

  # Last resort: try to read as both and see which works
  warning(
    "Could not confidently detect file type for: ", basename(file), "\n",
    "Attempting to read as seasonal output...",
    call. = FALSE
  )

  return("season")
}

#' Read multiple AquaCrop output files
#'
#' Reads multiple output files from a directory and combines them into
#' a single data frame. Automatically detects file types.
#'
#' @param path Character string. Path to directory containing output files
#' @param pattern Character string. Pattern to match files (default: "\\\\.OUT$")
#' @param type Character string. Type filter: "all" (default), "season", or "daily"
#' @param recursive Logical. Search subdirectories? Default is FALSE.
#' @param ... Additional arguments passed to read_out()
#'
#' @return A tibble combining all output files. If mixing seasonal and daily,
#'   returns a list with separate data frames.
#'
#' @examples
#' \dontrun{
#' # Read all outputs from OUTP directory
#' all_data <- read_out_batch("OUTP")
#'
#' # Read only seasonal outputs
#' seasons <- read_out_batch("OUTP", type = "season")
#'
#' # Read from multiple subdirectories
#' all_data <- read_out_batch("simulations", recursive = TRUE)
#' }
#'
#' @export
read_out_batch <- function(path = "OUTP",
                           pattern = "\\.OUT$",
                           type = c("all", "season", "daily"),
                           recursive = FALSE,
                           ...) {

  type <- match.arg(type)

  if (!dir.exists(path)) {
    stop("Directory not found: ", path, call. = FALSE)
  }

  # List files
  files <- list.files(
    path,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    stop("No output files found in: ", path, call. = FALSE)
  }

  message("Found ", length(files), " file(s)")

  # Filter by type if specified
  if (type != "all") {
    type_pattern <- if (type == "season") "season\\.OUT$" else "Day\\.OUT$"
    files <- files[grepl(type_pattern, files, ignore.case = TRUE)]

    if (length(files) == 0) {
      stop("No ", type, " output files found", call. = FALSE)
    }

    message("Reading ", length(files), " ", type, " file(s)...")
  }

  # Read all files
  all_data <- purrr::map(files, function(f) {
    tryCatch({
      read_out(f, type = if (type == "all") "auto" else type, ...)
    }, error = function(e) {
      warning("Failed to read ", basename(f), ": ", e$message, call. = FALSE)
      NULL
    })
  })

  # Remove failed reads
  all_data <- all_data[!sapply(all_data, is.null)]

  if (length(all_data) == 0) {
    stop("No files could be read successfully", call. = FALSE)
  }

  # Check if all same type
  types <- sapply(all_data, function(x) attr(x, "output_type"))

  if (length(unique(types)) == 1) {
    # All same type - combine
    message("Combining ", length(all_data), " ", unique(types), " file(s)...")
    result <- dplyr::bind_rows(all_data)
    attr(result, "output_type") <- unique(types)
    attr(result, "n_files") <- length(all_data)
    return(result)
  } else {
    # Mixed types - return list
    message("Mixed file types detected, returning list")
    result <- list(
      season = dplyr::bind_rows(all_data[types == "season"]),
      daily = dplyr::bind_rows(all_data[types == "daily"])
    )
    attr(result, "n_files_season") <- sum(types == "season")
    attr(result, "n_files_daily") <- sum(types == "daily")
    return(result)
  }
}

#' Quick summary of AquaCrop output
#'
#' Provides a quick summary of output data, with different summaries
#' for seasonal vs daily outputs
#'
#' @param data Output data from read_out() or read_season_out() or read_daily_out()
#'
#' @return Prints summary to console, invisibly returns summary statistics
#'
#' @examples
#' \dontrun{
#' season <- read_out("OUTP/wakandaPRMseason.OUT")
#' summary_out(season)
#'
#' daily <- read_out("OUTP/wakandaPRMDay.OUT")
#' summary_out(daily)
#' }
#'
#' @export
summary_out <- function(data) {

  output_type <- attr(data, "output_type")

  if (is.null(output_type)) {
    stop("Data does not have output_type attribute. Was it read with read_out()?",
         call. = FALSE)
  }

  cli::cli_h1("AquaCrop Output Summary")
  cli::cli_text("Type: {output_type}")
  cli::cli_text("Source: {.file {attr(data, 'source_file')}}")

  if (!is.null(attr(data, "aquacrop_version"))) {
    cli::cli_text("AquaCrop version: {attr(data, 'aquacrop_version')}")
  }

  cli::cli_text("")

  if (output_type == "season") {
    # Seasonal summary
    cli::cli_h2("Seasonal Statistics")

    n_runs <- nrow(data)
    cli::cli_text("Number of runs: {n_runs}")

    if ("site" %in% names(data)) {
      n_sites <- length(unique(data$site))
      cli::cli_text("Number of sites: {n_sites}")
    }

    if ("ydry" %in% names(data)) {
      cli::cli_text("")
      cli::cli_h3("Yield Statistics")
      yield_stats <- data %>%
        dplyr::summarise(
          mean = mean(ydry, na.rm = TRUE),
          sd = sd(ydry, na.rm = TRUE),
          min = min(ydry, na.rm = TRUE),
          max = max(ydry, na.rm = TRUE)
        )

      cli::cli_text("Mean yield: {round(yield_stats$mean, 2)} ton/ha")
      cli::cli_text("Std dev: {round(yield_stats$sd, 2)} ton/ha")
      cli::cli_text("Range: {round(yield_stats$min, 2)} - {round(yield_stats$max, 2)} ton/ha")
    }

  } else if (output_type == "daily") {
    # Daily summary
    cli::cli_h2("Daily Output Statistics")

    if (!is.null(attr(data, "n_runs"))) {
      cli::cli_text("Number of runs: {attr(data, 'n_runs')}")
    }

    if (!is.null(attr(data, "n_soil_layers"))) {
      cli::cli_text("Soil layers: {attr(data, 'n_soil_layers')}")
    }

    n_days <- nrow(data)
    cli::cli_text("Total daily records: {n_days}")

    if ("date" %in% names(data)) {
      date_range <- range(data$date, na.rm = TRUE)
      cli::cli_text("Date range: {date_range[1]} to {date_range[2]}")
    }
  }

  invisible(data)
}
