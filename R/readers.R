#' Standard AquaCrop seasonal output column names by version
#' @keywords internal
#' @noRd
.SEASON_COLS_BY_VERSION <- list(
  # Version 7.x (41 columns)
  v7 = c(
    "runnr", "day1", "month1", "year1", "rain", "eto", "gd", "co2",
    "irri", "infilt", "runoff", "drain", "upflow", "e", "eex", "tr",
    "trw", "trtrx", "saltin", "saltout", "saltup", "saltprof", "cycle",
    "saltstr", "fertstr", "weedstr", "tempstr", "expstr", "stostr", "biomass",
    "brelative", "hi", "ydry", "yfresh", "wpet", "bin", "bout",
    "dayn", "monthn", "yearn", "prmfile"
  ),
  # Version 6.x (36 columns) - sans les 5 dernières colonnes de stress
  v6 = c(
    "runnr", "day1", "month1", "year1", "rain", "eto", "gd", "co2",
    "irri", "infilt", "runoff", "drain", "upflow", "e", "eex", "tr",
    "trw", "trtrx", "saltin", "saltout", "saltup", "saltprof", "cycle",
    "biomass", "brelative", "hi", "ydry", "yfresh", "wpet", "bin", "bout",
    "dayn", "monthn", "yearn", "prmfile"
  )
)

#' Detect AquaCrop version from output file
#' @keywords internal
#' @noRd
.detect_aquacrop_version <- function(file) {
  # Read first line
  first_line <- readr::read_lines(file, n_max = 1)

  # Extract version
  version_match <- stringr::str_extract(first_line, "AquaCrop ([0-9.]+)")

  if (is.na(version_match)) {
    return(NULL)
  }

  # Extract major version
  version_num <- stringr::str_extract(version_match, "[0-9]+\\.[0-9]+")
  major_version <- as.numeric(stringr::str_extract(version_num, "^[0-9]+"))

  return(list(
    full = version_num,
    major = major_version
  ))
}

#' Get appropriate column names based on file structure
#' @keywords internal
#' @noRd
.get_season_colnames <- function(file, ncol_data) {
  # Detect version
  version_info <- .detect_aquacrop_version(file)

  if (is.null(version_info)) {
    # Fallback: guess based on column count
    if (ncol_data == 41) {
      return(.SEASON_COLS_BY_VERSION$v7)
    } else if (ncol_data == 36) {
      return(.SEASON_COLS_BY_VERSION$v6)
    } else {
      warning(
        "Unknown column count: ", ncol_data, "\n",
        "Using generic column names",
        call. = FALSE
      )
      return(paste0("col", 1:ncol_data))
    }
  }

  # Choose based on version
  if (version_info$major >= 7) {
    expected_cols <- .SEASON_COLS_BY_VERSION$v7
  } else if (version_info$major >= 6) {
    expected_cols <- .SEASON_COLS_BY_VERSION$v6
  } else {
    warning(
      "AquaCrop version ", version_info$full, " may have different format\n",
      "Proceeding with best guess",
      call. = FALSE
    )
    expected_cols <- .SEASON_COLS_BY_VERSION$v6
  }

  # Validate column count
  if (ncol_data != length(expected_cols)) {
    warning(
      "Column mismatch for AquaCrop ", version_info$full, ":\n",
      "  Expected ", length(expected_cols), " columns\n",
      "  Got ", ncol_data, " columns\n",
      "Using generic names for extra/missing columns",
      call. = FALSE
    )

    if (ncol_data > length(expected_cols)) {
      # More columns than expected - add generic names
      extra_cols <- paste0("col", (length(expected_cols) + 1):ncol_data)
      return(c(expected_cols, extra_cols))
    } else {
      # Fewer columns - truncate
      return(expected_cols[1:ncol_data])
    }
  }

  return(expected_cols)
}

#' Read AquaCrop seasonal output file
#' @export
read_season_out <- function(file, add_dates = TRUE) {

  # Validate file
  .validate_file(file, "PRMseason.OUT")

  # Extract site name
  site_name <- basename(file) %>%
    stringr::str_replace("(?i)PRMseason\\.OUT$", "") %>%
    stringr::str_replace("(?i)season\\.OUT$", "")

  # Read data WITHOUT header
  data <- tryCatch({
    readr::read_table(
      file = file,
      skip = 4,
      col_names = FALSE,
      col_types = readr::cols(.default = readr::col_guess()),
      show_col_types = FALSE,
      na = c("", "NA", "-9", "-9.9")
    )
  }, error = function(e) {
    stop("Failed to read data from: ", basename(file), "\n",
         "Error: ", conditionMessage(e), call. = FALSE)
  })

  # Get appropriate column names based on version and column count
  col_names <- .get_season_colnames(file, ncol(data))
  names(data) <- col_names

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  # Add site column
  data <- data %>%
    dplyr::mutate(site = site_name, .before = 1)

  # Add dates if requested
  if (add_dates) {
    if (all(c("day1", "month1", "year1") %in% names(data))) {
      data <- data %>%
        dplyr::mutate(
          plantingdate = as.Date(paste(year1, month1, day1, sep = "-")),
          .after = "runnr"
        )
    }

    if (all(c("dayn", "monthn", "yearn") %in% names(data))) {
      after_col <- if ("plantingdate" %in% names(data)) "plantingdate" else "runnr"

      data <- data %>%
        dplyr::mutate(
          harvestdate = as.Date(paste(yearn, monthn, dayn, sep = "-")),
          .after = dplyr::all_of(after_col)
        )
    }

    if (all(c("plantingdate", "harvestdate") %in% names(data))) {
      data <- data %>%
        dplyr::mutate(
          seasonlength = as.numeric(harvestdate - plantingdate),
          .after = "harvestdate"
        )
    }
  }

  # Add metadata
  version_info <- .detect_aquacrop_version(file)
  attr(data, "source_file") <- normalizePath(file, mustWork = FALSE)
  attr(data, "read_time") <- Sys.time()
  if (!is.null(version_info)) {
    attr(data, "aquacrop_version") <- version_info$full
  }

  return(data)
}

#' Validate file exists and has expected pattern
#' @keywords internal
#' @noRd
.validate_file <- function(file, pattern) {

  if (!is.character(file) || length(file) != 1) {
    stop("file must be a single character string", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }

  if (!grepl(pattern, file, ignore.case = TRUE)) {
    warning(
      "File name does not match expected pattern '", pattern, "'\n",
      "File: ", basename(file), "\n",
      "Proceeding anyway, but results may be incorrect.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Read an AquaCrop CLI File
#'
#' Reads and parses an AquaCrop CLI (climate) file, locates the referenced
#' weather files (.Tnx, .ETo, .PLU), reads them, and joins them into a single
#' tidy data frame.
#'
#' @param file Character. Path to the AquaCrop CLI file.
#'
#' @family AquaCrop readers
#' @return A tibble with columns:
#'   \itemize{
#'     \item station: Character, station/grid name (from temperature file)
#'     \item year: Integer, year
#'     \item month: Integer, month (1-12)
#'     \item day: Integer, day of month
#'     \item tmin: Numeric, minimum temperature (°C)
#'     \item tmax: Numeric, maximum temperature (°C)
#'     \item eto: Numeric, reference evapotranspiration (mm/day)
#'     \item rain: Numeric, rainfall (mm)
#'   }
#'
#' @details
#' The CLI file structure:
#' \itemize{
#'   \item Line 1: Description/name
#'   \item Line 2: AquaCrop version
#'   \item Line 3: Temperature file (.Tnx)
#'   \item Line 4: ETo file (.ETo)
#'   \item Line 5: Rainfall file (.PLU)
#'   \item Line 6: CO2 file (.CO2)
#' }
#'
#' File path resolution:
#' \itemize{
#'   \item If filename only (e.g., "grid_005.Tnx"), looks in the same directory
#'     as the CLI file
#'   \item If absolute path (e.g., "/data/weather/grid_005.Tnx"), uses that path
#'   \item If relative path (e.g., "../weather/grid_005.Tnx"), resolves relative
#'     to CLI file location
#' }
#'
#' The function reads all three weather files and joins them by date columns
#' (station, year, month, day) using left joins.
#'
#' @examples
#' \dontrun{
#' # Read CLI file and get complete weather data
#' climate <- read_cli("data/default.CLI")
#' head(climate)
#'
#' # Check for missing data
#' summary(climate)
#'
#' # Filter complete cases
#' complete_data <- climate[complete.cases(climate), ]
#' }
#'
#' @importFrom fs file_exists path_dir path is_absolute_path
#' @importFrom dplyr left_join
#' @export
read_cli <- function(file) {
  # Check file existence
  if (!fs::file_exists(file)) {
    stop("CLI file does not exist: ", file)
  }

  # Read CLI file
  lines <- readLines(file, warn = FALSE)

  if (length(lines) < 6) {
    stop("CLI file does not have expected structure (minimum 6 lines): ", file)
  }

  # Parse CLI content
  # Line 1: description (skip)
  # Line 2: version (skip)
  # Line 3: Tnx file
  # Line 4: ETo file
  # Line 5: PLU file
  # Line 6: CO2 file (skip for now)

  tnx_file <- trimws(lines[3])
  eto_file <- trimws(lines[4])
  plu_file <- trimws(lines[5])

  # Get base directory of CLI file
  cli_dir <- fs::path_dir(file)

  # Resolve file paths
  tnx_path <- .resolve_path(tnx_file, cli_dir)
  eto_path <- .resolve_path(eto_file, cli_dir)
  plu_path <- .resolve_path(plu_file, cli_dir)

  # Check all files exist
  if (!fs::file_exists(tnx_path)) {
    stop("Temperature file not found: ", tnx_path)
  }
  if (!fs::file_exists(eto_path)) {
    stop("ETo file not found: ", eto_path)
  }
  if (!fs::file_exists(plu_path)) {
    stop("Rainfall file not found: ", plu_path)
  }

  # Read all weather files
  tnx_data <- read_tnx(tnx_path)
  eto_data <- read_eto(eto_path)
  plu_data <- read_plu(plu_path)

  # Join datasets by date columns
  # Start with temperature (usually the primary dataset)
  climate <- tnx_data

  # Join ETo data
  climate <- dplyr::left_join(
    climate,
    eto_data[, c("year", "month", "day", "eto")],
    by = c("year", "month", "day")
  )

  # Join rainfall data
  climate <- dplyr::left_join(
    climate,
    plu_data[, c("year", "month", "day", "rain")],
    by = c("year", "month", "day")
  )

  return(climate)
}

#' @rdname read_cli
#' @family AquaCrop readers
#' @export
read_climate <- read_cli


#' Check if a File is an AquaCrop Tnx File
#'
#' Validates whether a file appears to be a valid AquaCrop Tnx (temperature)
#' file based on file extension, structure, and business rules.
#'
#' @param file Character. Path to the file to check.
#'
#' @return Logical. TRUE if the file appears to be a valid AquaCrop Tnx file,
#'   FALSE otherwise.
#'
#' @details
#' The function checks:
#' \itemize{
#'   \item File existence
#'   \item .Tnx file extension (preferred but not required)
#'   \item Presence of colon-separated header lines
#'   \item Presence of separator line (====)
#'   \item Presence of numeric data after separator
#'   \item Consistency of first_day with record_type
#' }
#'
#' @examples
#' \dontrun{
#' is_tnx("temp_data.Tnx")
#' is_tnx("weather/grid_005.TNX")
#' }
#'
#' @importFrom fs file_exists path_ext
#' @noRd
#' @keywords internal
#'
is_tnx <- function(file) .is_climate_file(file, "tnx")


#' Read an AquaCrop Tnx File
#'
#' Reads and parses an AquaCrop Tnx (temperature) file into a tidy data frame.
#'
#' @param file Character. Path to the AquaCrop Tnx file.
#'
#' @family AquaCrop readers
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item station: Character, station/grid name
#'     \item year: Integer, year
#'     \item month: Integer, month (1-12)
#'     \item day: Integer, day of month
#'     \item tmin: Numeric, minimum temperature (°C). -9 values are converted to NA.
#'     \item tmax: Numeric, maximum temperature (°C). -9 values are converted to NA.
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates the file using \code{is_tnx()}
#'   \item Parses header information (station, record type, start date)
#'   \item Reads Tmin and Tmax values
#'   \item Generates dates based on record type (daily, 10-day, or monthly)
#'   \item Converts -9 (missing data indicator) to NA
#' }
#'
#' Record types:
#' \itemize{
#'   \item 1: Daily data
#'   \item 2: 10-day (dekadal) data
#'   \item 3: Monthly data
#' }
#'
#' @examples
#' \dontrun{
#' temp_data <- read_tnx("data/grid_005.Tnx")
#' head(temp_data)
#'
#' # Filter complete cases
#' complete_data <- temp_data[!is.na(temp_data$tmin) & !is.na(temp_data$tmax), ]
#'
#' # Calculate daily temperature range
#' temp_data$temp_range <- temp_data$tmax - temp_data$tmin
#'
#' # Summary statistics
#' summary(temp_data[, c("tmin", "tmax")])
#' }
#'
#' @importFrom readr read_table
#' @importFrom tibble tibble
#' @family AquaCrop readers
#' @export
read_tnx <- function(file) {
  .read_climate_file(file, "tnx", c("tmin", "tmax"), "Tnx")
}

#' Check if a File is an AquaCrop ETo File
#'
#' Validates whether a file appears to be a valid AquaCrop ETo (reference
#' evapotranspiration) file based on file extension, structure, and business rules.
#'
#' @param file Character. Path to the file to check.
#'
#' @return Logical. TRUE if the file appears to be a valid AquaCrop ETo file,
#'   FALSE otherwise.
#'
#' @details
#' The function checks:
#' \itemize{
#'   \item File existence
#'   \item .ETo file extension (preferred but not required)
#'   \item Presence of colon-separated header lines
#'   \item Presence of separator line (====)
#'   \item Presence of numeric data after separator
#'   \item Consistency of first_day with record_type
#' }
#'
#' @examples
#' \dontrun{
#' is_eto("eto_data.ETo")
#' is_eto("weather/grid_003.ETO")
#' }
#'
#' @importFrom fs file_exists path_ext
#' @noRd
#' @keywords internal
#'
is_eto <- function(file) .is_climate_file(file, "eto")


#' Read an AquaCrop ETo File
#'
#' Reads and parses an AquaCrop ETo (reference evapotranspiration) file into
#' a tidy data frame.
#'
#' @param file Character. Path to the AquaCrop ETo file.
#' @family AquaCrop readers
#' @return A tibble with columns:
#'   \itemize{
#'     \item station: Character, station/grid name
#'     \item year: Integer, year
#'     \item month: Integer, month (1-12)
#'     \item day: Integer, day of month
#'     \item eto: Numeric, reference evapotranspiration (mm/day). -9 values are
#'       converted to NA.
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates the file using \code{is_eto()}
#'   \item Parses header information (station, record type, start date)
#'   \item Reads ETo values
#'   \item Generates dates based on record type (daily, 10-day, or monthly)
#'   \item Converts -9 (missing data indicator) to NA
#' }
#'
#' Record types:
#' \itemize{
#'   \item 1: Daily data
#'   \item 2: 10-day (dekadal) data
#'   \item 3: Monthly data
#' }
#'
#' @examples
#' \dontrun{
#' eto_data <- read_eto("data/grid_003.ETo")
#' head(eto_data)
#'
#' # Filter complete cases
#' complete_data <- eto_data[!is.na(eto_data$eto), ]
#'
#' # Summary statistics
#' summary(eto_data$eto)
#' }
#'
#' @importFrom readr read_fwf
#' @importFrom tibble tibble
#' @family AquaCrop readers
#' @export
read_eto <- function(file) {
  .read_climate_file(file, "eto", "eto", "ETo")
}


#' Check if a File is an AquaCrop PLU File
#'
#' Validates whether a file appears to be a valid AquaCrop PLU (rainfall) file
#' based on file extension, structure, and business rules.
#'
#' @param file Character. Path to the file to check.
#' @return Logical. TRUE if the file appears to be a valid AquaCrop PLU file,
#'   FALSE otherwise.
#'
#' @details
#' The function checks:
#' \itemize{
#'   \item File existence
#'   \item .plu file extension (preferred but not required)
#'   \item Presence of colon-separated header lines
#'   \item Presence of separator line (====)
#'   \item Presence of numeric data after separator
#'   \item Consistency of first_day with record_type
#' }
#'
#' @examples
#' \dontrun{
#' is_plu("rainfall_data.plu")
#' is_plu("weather/station1.PLU")
#' }
#'
#' @export
is_plu <- function(file) .is_climate_file(file, "plu")


#' Read an AquaCrop PLU File
#'
#' Reads and parses an AquaCrop PLU (rainfall) file into a tidy data frame.
#'
#' @param file Character. Path to the AquaCrop PLU file.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item station: Character, station name
#'     \item year: Integer, year
#'     \item month: Integer, month (1-12)
#'     \item day: Integer, day of month
#'     \item rain: Numeric, rainfall value (mm). -9 values are converted to NA.
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates the file using \code{is_plu()}
#'   \item Parses header information (station, record type, start date)
#'   \item Reads rainfall values
#'   \item Generates dates based on record type (daily, 10-day, or monthly)
#'   \item Converts -9 (missing data indicator) to NA
#' }
#'
#' Record types:
#' \itemize{
#'   \item 1: Daily data
#'   \item 2: 10-day (dekadal) data
#'   \item 3: Monthly data
#' }
#'
#' @examples
#' \dontrun{
#' rainfall <- read_plu("data/station1.plu")
#' head(rainfall)
#' }
#' @family AquaCrop readers
#' @export
read_plu <- function(file) {
  .read_climate_file(file, "plu", "rain", "PLU")
}


#' @rdname read_eto
#' @family AquaCrop readers
#' @export
read_et0 <- read_eto
