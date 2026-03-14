#' Validate AquaCrop Climate File Structure
#'
#' Generic validator for AquaCrop climate files (Tnx, ETo, PLU).
#'
#' @param file Character. Path to the file to check.
#' @param expected_ext Character. Expected file extension (e.g., "tnx", "eto", "plu").
#'
#' @return Logical. TRUE if the file appears to be a valid AquaCrop climate file.
#'
#' @keywords internal
#' @noRd
.is_climate_file <- function(file, expected_ext) {
  # Check file existence
  if (!fs::file_exists(file)) {
    return(FALSE)
  }

  # Check file extension
  ext_ok <- tolower(fs::path_ext(file)) == expected_ext

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
  tryCatch({
    if (length(sep_idx) == 0 || sep_idx[1] < 4) {
      return(FALSE)
    }

    header_lines <- lines[1:(sep_idx[1] - 3)]
    parts <- strsplit(header_lines, ":", fixed = TRUE)

    if (length(parts) >= 5) {
      record_type <- as.integer(trimws(parts[[2]][1]))
      first_day <- as.integer(trimws(parts[[3]][1]))

      if (!.check_dates_rule(record_type, first_day)) {
        return(FALSE)
      }
    }

    TRUE

  }, error = function(e) {
    FALSE
  })
}


#' Check column header pattern in climate file
#'
#' @param file Path to file
#' @param pattern Regex pattern to search for in header line
#' @keywords internal
#' @noRd
.check_climate_header <- function(file, pattern) {
  tryCatch({
    lines <- readLines(file, n = 20, warn = FALSE)
    sep_idx <- grep("^=+", lines)

    if (length(sep_idx) == 0 || sep_idx[1] <= 1) {
      return(FALSE)
    }

    header_line <- lines[sep_idx[1] - 1]
    has_pattern <- grepl(pattern, header_line, ignore.case = TRUE)

    return(has_pattern)

  }, error = function(e) {
    FALSE
  })
}


#' Check if file is a valid AquaCrop temperature file
#'
#' @param file Path to the file to check
#' @return Logical indicating if file is a valid .Tnx file
#' @export
is_tnx <- function(file) {

  # Check basic climate file structure
  is_climate <- .is_climate_file(file, "tnx")

  if (!is_climate) {
    return(FALSE)
  }

  # Check for Tmin and Tmax in header
  has_temp_header <- .check_climate_header(file, "Tmin.*Tmax")

  return(has_temp_header)
}


#' Check if file is a valid AquaCrop rainfall file
#'
#' @param file Path to the file to check
#' @return Logical indicating if file is a valid .PLU file
#' @export
is_plu <- function(file) {

  # Check basic climate file structure
  is_climate <- .is_climate_file(file, "plu")

  if (!is_climate) {
    return(FALSE)
  }

  # Check for "Total rain" in header
  has_rain_header <- .check_climate_header(file, "Total rain")

  return(has_rain_header)
}


#' Check if file is a valid AquaCrop reference evapotranspiration file
#'
#' @param file Path to the file to check
#' @return Logical indicating if file is a valid .ETo file
#' @export
is_eto <- function(file) {

  # Check basic climate file structure
  is_climate <- .is_climate_file(file, "eto")

  if (!is_climate) {
    return(FALSE)
  }

  # Check for "Average ETo" in header
  has_eto_header <- .check_climate_header(file, "Average ETo")

  return(has_eto_header)
}


#' Check if file is a valid AquaCrop climate file
#'
#' @param file Path to the file to check
#' @return Logical indicating if file is a valid .CLI file
#' @export
is_cli <- function(file) {

  # Check file existence
  if (!fs::file_exists(file)) {
    return(FALSE)
  }

  # Check file extension
  ext_ok <- tolower(fs::path_ext(file)) == "cli"

  if (!ext_ok) {
    return(FALSE)
  }

  # Read file
  tryCatch({
    lines <- readLines(file, n = 10, warn = FALSE)

    if (length(lines) < 6) {
      return(FALSE)
    }

    # Check structure:
    # Line 1: climate name
    # Line 2: version (contains "AquaCrop Version")
    # Line 3: .Tnx file
    # Line 4: .ETo file
    # Line 5: .PLU file
    # Line 6: .CO2 file

    has_version <- grepl("AquaCrop Version", lines[2], ignore.case = TRUE)
    has_tnx <- grepl("\\.Tnx$", lines[3], ignore.case = TRUE)
    has_eto <- grepl("\\.ETo$", lines[4], ignore.case = TRUE)
    has_plu <- grepl("\\.PLU$", lines[5], ignore.case = TRUE)
    has_co2 <- grepl("\\.CO2$", lines[6], ignore.case = TRUE)

    is_valid <- has_version && has_tnx && has_eto && has_plu && has_co2

    return(is_valid)

  }, error = function(e) {
    FALSE
  })
}
