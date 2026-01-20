#' Read AquaCrop Season Output Files
#'
#' Reads seasonal output files from AquaCrop simulations (PRMSeason.OUT) and
#' returns a tibble with formatted data and cleaned column names.
#'
#' @param file Path to the AquaCrop output file (fooPRMSeason.OUT)
#'
#' @return A tibble containing the output file data with:
#'   \itemize{
#'     \item Columns formatted with camelCase names
#'     \item Automatically detected data types
#'   }
#'
#' @details
#' AquaCrop season output files follow a fixed-width format with:
#' - Lines 1-2: Headers/metadata
#' - Line 3: Column names
#' - Line 4+: Data
#' @family AquaCrop readers
#' @examples
#' \dontrun{
#' data <- read_season_out("C1PRMSeason.OUT")
#' }
#'
#' @export
read_season_out <- function(file) {
  .validate_file(file, "PRMseason.OUT")

  header <- .read_header(file)
  data <- readr::read_fwf(file = file, skip = 4)
  names(data) <- header

  return(data)
}



#' @keywords internal
.validate_file <- function(file, expected_suffix) {
  if (!file.exists(file)) {
    stop("file does not exist: ", file, call. = FALSE)
  }

  if (!endsWith(basename(file), expected_suffix)) {
    stop("file should be ended with ", expected_suffix,
      ", got: ", basename(file),
      call. = FALSE
    )
  }
}

#' @keywords internal
.read_header <- function(file) {
  readr::read_fwf(file = file, skip = 2, n_max = 1) %>%
    c() %>%
    unlist() %>%
    c("PRMName") %>%
    janitor::make_clean_names(case = "snake") %>%
    gsub(pattern = "_", replacement = "", x = .)
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


#' Resolve File Path Relative to Base Directory
#'
#' Helper function to resolve file paths that may be absolute, relative, or
#' just filenames.
#'
#' @param file_path Character. Path from CLI file (may be filename, relative, or absolute).
#' @param base_dir Character. Base directory (directory containing CLI file).
#'
#' @return Character. Resolved absolute path.
#'
#' @keywords internal
#' @noRd
.resolve_path <- function(file_path, base_dir) {
  # If absolute path, return as is
  if (fs::is_absolute_path(file_path)) {
    return(file_path)
  }

  # Otherwise, treat as relative to base_dir
  return(fs::path(base_dir, file_path))
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
is_tnx <- function(file) {
  # Check file existence
  if (!fs::file_exists(file)) {
    return(FALSE)
  }

  # Check file extension
  ext_ok <- tolower(fs::path_ext(file)) == "tnx"

  # Read first lines
  lines <- tryCatch(
    readLines(file, n = 20, warn = FALSE),
    error = function(e) character()
  )
  if (length(lines) == 0) {
    return(FALSE)
  }

  # Check for AquaCrop file structure
  has_colon <- any(grepl(":", lines, fixed = TRUE))
  has_separator <- any(grepl("^=+", lines))

  # Check for data after separator
  sep_idx <- grep("^=+", lines)
  has_data <- FALSE
  if (length(sep_idx) > 0 && sep_idx[1] < length(lines)) {
    has_data <- any(grepl("^\\s*-?\\d", lines[(sep_idx[1] + 1):length(lines)]))
  }

  # Basic structure check
  basic_ok <- ext_ok || (has_colon && has_separator && has_data)
  if (!basic_ok) {
    return(FALSE)
  }

  # Check business rules for first_day
  header_lines <- lines[1:(sep_idx[1] - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  if (length(parts) >= 5) {
    record_type <- as.integer(trimws(parts[[2]][1]))
    first_day <- as.integer(trimws(parts[[3]][1]))

    # Return FALSE if date rules are not met
    if (!tryCatch(check_dates_rule(record_type, first_day),
      error = function(e) FALSE
    )) {
      return(FALSE)
    }
  }

  TRUE
}


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
  # Validate file
  if (!is_tnx(file)) {
    stop("File is not a valid AquaCrop Tnx file: ", file)
  }

  # Read file
  lines <- readLines(file, warn = FALSE)

  # Locate separator (====)
  sep_idx <- grep("^=+", lines)[1]
  if (is.na(sep_idx)) {
    stop("Cannot find separator line (====) in file: ", file)
  }

  # Parse header
  header_lines <- lines[1:(sep_idx - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  station <- trimws(parts[[1]][1])
  record_type <- as.integer(trimws(parts[[2]][1]))
  first_day <- as.integer(trimws(parts[[3]][1]))
  first_month <- as.integer(trimws(parts[[4]][1]))
  first_year <- as.integer(trimws(parts[[5]][1]))

  # Check business rules for dates
  check_dates_rule(record_type, first_day)

  # Read temperature values (2 columns: Tmin and Tmax)
  temp_data <- readr::read_table(
    file,
    skip = sep_idx,
    col_names = c("tmin", "tmax"),
    show_col_types = FALSE
  )

  # Convert -9 to NA
  temp_data$tmin <- replace(temp_data$tmin, temp_data$tmin == -9, NA_real_)
  temp_data$tmax <- replace(temp_data$tmax, temp_data$tmax == -9, NA_real_)

  # Generate dates based on record type
  by_type <- c("day", "10 days", "month")
  start_date <- as.Date(sprintf("%d-%02d-%02d", first_year, first_month, first_day))
  dates <- seq.Date(
    from = start_date,
    by = by_type[record_type],
    length.out = nrow(temp_data)
  )

  # Return tidy tibble
  tibble::tibble(
    station = station,
    year    = as.integer(format(dates, "%Y")),
    month   = as.integer(format(dates, "%m")),
    day     = as.integer(format(dates, "%d")),
    tmin    = temp_data$tmin,
    tmax    = temp_data$tmax
  )
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
is_eto <- function(file) {
  # Check file existence
  if (!fs::file_exists(file)) {
    return(FALSE)
  }

  # Check file extension
  ext_ok <- tolower(fs::path_ext(file)) == "eto"

  # Read first lines
  lines <- tryCatch(
    readLines(file, n = 20, warn = FALSE),
    error = function(e) character()
  )
  if (length(lines) == 0) {
    return(FALSE)
  }

  # Check for AquaCrop file structure
  has_colon <- any(grepl(":", lines, fixed = TRUE))
  has_separator <- any(grepl("^=+", lines))

  # Check for data after separator
  sep_idx <- grep("^=+", lines)
  has_data <- FALSE
  if (length(sep_idx) > 0 && sep_idx[1] < length(lines)) {
    has_data <- any(grepl("^\\s*-?\\d", lines[(sep_idx[1] + 1):length(lines)]))
  }

  # Basic structure check
  basic_ok <- ext_ok || (has_colon && has_separator && has_data)
  if (!basic_ok) {
    return(FALSE)
  }

  # Check business rules for first_day
  header_lines <- lines[1:(sep_idx[1] - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  if (length(parts) >= 5) {
    record_type <- as.integer(trimws(parts[[2]][1]))
    first_day <- as.integer(trimws(parts[[3]][1]))

    # Return FALSE if date rules are not met
    if (!tryCatch(check_dates_rule(record_type, first_day),
      error = function(e) FALSE
    )) {
      return(FALSE)
    }
  }

  TRUE
}


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
  # Validate file
  if (!is_eto(file)) {
    stop("File is not a valid AquaCrop ETo file: ", file)
  }

  # Read file, just 100 lines for header parsing
  lines <- readLines(file, n = 100, warn = FALSE)

  # Locate separator (====)
  sep_idx <- grep("^=+", lines)[1]
  if (is.na(sep_idx)) {
    stop("Cannot find separator line (====) in file: ", file)
  }

  # Parse header
  header_lines <- lines[1:(sep_idx - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  station <- trimws(parts[[1]][1])
  record_type <- as.integer(trimws(parts[[2]][1]))
  first_day <- as.integer(trimws(parts[[3]][1]))
  first_month <- as.integer(trimws(parts[[4]][1]))
  first_year <- as.integer(trimws(parts[[5]][1]))

  # Check business rules for dates
  check_dates_rule(record_type, first_day)

  # Read ETo values
  value <- readr::read_fwf(file, skip = sep_idx, show_col_types = FALSE)[[1]]
  value <- replace(value, value == -9, NA_real_)

  # Generate dates based on record type
  by_type <- c("day", "10 days", "month")
  start_date <- as.Date(sprintf("%d-%02d-%02d", first_year, first_month, first_day))
  dates <- seq.Date(
    from = start_date,
    by = by_type[record_type],
    length.out = length(value)
  )

  # Return tidy tibble
  tibble::tibble(
    station = station,
    year    = as.integer(format(dates, "%Y")),
    month   = as.integer(format(dates, "%m")),
    day     = as.integer(format(dates, "%d")),
    eto     = value
  )
}

#' Check Consistency of first_day for record_type
#'
#' Validates that the first_day value is consistent with the record_type
#' according to AquaCrop PLU file specifications.
#'
#' @param record_type Integer. Record type: 1 = daily, 2 = 10-day, 3 = monthly.
#' @param first_day Integer. First day of the record.
#'
#' @return Logical. Returns TRUE if consistent, otherwise stops with an error.
#'
#' @details
#' Business rules:
#' \itemize{
#'   \item Monthly (record_type = 3): first_day must be 1
#'   \item 10-day (record_type = 2): first_day must be 1, 11, or 21
#'   \item Daily (record_type = 1): first_day can be any valid day (1-31)
#' }
#'
#' @keywords internal
#' @noRd
check_dates_rule <- function(record_type, first_day) {
  if (record_type == 3 && first_day != 1) {
    stop("Monthly record_type requires first_day = 1")
  }
  if (record_type == 2 && !(first_day %in% c(1, 11, 21))) {
    stop("10-day record_type requires first_day = 1, 11, or 21")
  }
  TRUE
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
is_plu <- function(file) {
  # Check file existence
  if (!fs::file_exists(file)) {
    return(FALSE)
  }

  # Check file extension
  ext_ok <- tolower(fs::path_ext(file)) == "plu"

  # Read first lines
  lines <- tryCatch(
    readLines(file, n = 20, warn = FALSE),
    error = function(e) character()
  )
  if (length(lines) == 0) {
    return(FALSE)
  }

  # Check for AquaCrop file structure
  has_colon <- any(grepl(":", lines, fixed = TRUE))
  has_separator <- any(grepl("^=+", lines))

  # Check for data after separator
  sep_idx <- grep("^=+", lines)
  has_data <- FALSE
  if (length(sep_idx) > 0 && sep_idx[1] < length(lines)) {
    has_data <- any(grepl("^\\s*-?\\d", lines[(sep_idx[1] + 1):length(lines)]))
  }

  # Basic structure check
  basic_ok <- ext_ok || (has_colon && has_separator && has_data)
  if (!basic_ok) {
    return(FALSE)
  }

  # Check business rules for first_day
  header_lines <- lines[1:(sep_idx[1] - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  if (length(parts) >= 5) {
    record_type <- as.integer(trimws(parts[[2]][1]))
    first_day <- as.integer(trimws(parts[[3]][1]))

    # Return FALSE if date rules are not met
    if (!tryCatch(check_dates_rule(record_type, first_day),
      error = function(e) FALSE
    )) {
      return(FALSE)
    }
  }

  TRUE
}


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
  # Validate file
  if (!is_plu(file)) {
    stop("File is not a valid AquaCrop PLU file: ", file)
  }

  # Read file
  lines <- readLines(file, n = 100, warn = FALSE)

  # Locate separator (====)
  sep_idx <- grep("^=+", lines)[1]
  if (is.na(sep_idx)) {
    stop("Cannot find separator line (====) in file: ", file)
  }

  # Parse header
  header_lines <- lines[1:(sep_idx - 3)]
  parts <- strsplit(header_lines, ":", fixed = TRUE)

  station <- trimws(parts[[1]][1])
  record_type <- as.integer(trimws(parts[[2]][1]))
  first_day <- as.integer(trimws(parts[[3]][1]))
  first_month <- as.integer(trimws(parts[[4]][1]))
  first_year <- as.integer(trimws(parts[[5]][1]))

  # Check business rules for dates
  check_dates_rule(record_type, first_day)

  # Read rainfall values
  value <- readr::read_fwf(file, skip = sep_idx, show_col_types = FALSE)[[1]]
  value <- replace(value, value == -9, NA_real_)

  # Generate dates based on record type
  by_type <- c("day", "10 days", "month")
  start_date <- as.Date(sprintf("%d-%02d-%02d", first_year, first_month, first_day))
  dates <- seq.Date(
    from = start_date,
    by = by_type[record_type],
    length.out = length(value)
  )

  # Return tidy tibble
  tibble::tibble(
    station = station,
    year    = as.integer(format(dates, "%Y")),
    month   = as.integer(format(dates, "%m")),
    day     = as.integer(format(dates, "%d")),
    rain    = value
  )
}


#' @rdname read_eto
#' @family AquaCrop readers
#' @export
read_et0 <- read_eto
