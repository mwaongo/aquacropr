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
#' @examples
#' \dontrun{
#' # Read CLI file and get complete weather data
#' climate <- read_cli("data/default.CLI")
#' head(climate)
#' }
#'
#' @importFrom fs file_exists path_dir path is_absolute_path
#' @importFrom dplyr left_join
#' @export
read_cli <- function(file) {

  # Validate file
  if (!is_cli(file)) {
    stop("File is not a valid AquaCrop CLI file: ", file)
  }

  # Read CLI file
  lines <- readLines(file, warn = FALSE)

  # Parse CLI content
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
#' @examples
#' \dontrun{
#' temp_data <- read_tnx("data/grid_005.Tnx")
#' head(temp_data)
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

  .read_climate_file(file, "tnx", c("tmin", "tmax"), "Tnx")
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
#' @examples
#' \dontrun{
#' eto_data <- read_eto("data/grid_003.ETo")
#' head(eto_data)
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

  .read_climate_file(file, "eto", "eto", "ETo")
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

  .read_climate_file(file, "plu", "rain", "PLU")
}


#' @rdname read_eto
#' @family AquaCrop readers
#' @export
read_et0 <- read_eto


#' Parse an AquaCrop Calendar (.CAL) File
#'
#' Reads a .CAL file and returns a structured list of onset parameters.
#'
#' @param file Character. Full path to the .CAL file.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{onset}{Character. "fixed", "rainfall", or "thermal".}
#'     \item{fixed_day}{Integer or NA. Fixed calendar day (onset "fixed" only).}
#'     \item{window_start}{Integer or NA.}
#'     \item{window_length}{Integer or NA.}
#'     \item{criterion_internal}{Integer or NA. AquaCrop internal criterion number.}
#'     \item{criterion}{Integer or NA. User-facing criterion number (1-4).}
#'     \item{preset_value}{Numeric or NA.}
#'     \item{successive_days}{Integer or NA. NA when not applicable.}
#'     \item{occurrences}{Integer or NA.}
#'   }
#'
#' @examples
#' \dontrun{
#' cal <- read_cal("CAL/station_01.CAL")
#' cal$onset        # "rainfall"
#' cal$criterion    # 2
#' cal$preset_value # 30
#' }
#' @family AquaCrop readers
#'
#' @export
read_cal <- function(file) {
  
  if (!fs::file_exists(file)) {
    stop("CAL file not found: ", file, call. = FALSE)
  }
  
  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  
  # Extract the numeric value at the start of a line ("  value  : description")
  .parse_value <- function(line) {
    suppressWarnings(as.numeric(trimws(strsplit(line, ":")[[1]][1])))
  }
  
  onset_code <- as.integer(.parse_value(lines[3]))
  
  if (onset_code == 0L) {
    return(list(
      onset              = "fixed",
      fixed_day          = as.integer(.parse_value(lines[6])),
      window_start       = NA_integer_,
      window_length      = NA_integer_,
      criterion_internal = NA_integer_,
      criterion          = NA_integer_,
      preset_value       = NA_real_,
      successive_days    = NA_integer_,
      occurrences        = NA_integer_
    ))
  }
  
  window_start       <- as.integer(.parse_value(lines[4]))
  window_length      <- as.integer(.parse_value(lines[5]))
  criterion_internal <- as.integer(.parse_value(lines[6]))
  preset_value       <- as.numeric(.parse_value(lines[7]))
  successive_days    <- as.integer(.parse_value(lines[8]))
  occurrences        <- as.integer(.parse_value(lines[9]))
  
  # -9 means not applicable
  if (!is.na(successive_days) && successive_days == -9L) {
    successive_days <- NA_integer_
  }
  
  if (criterion_internal %in% 1L:4L) {
    onset     <- "rainfall"
    criterion <- criterion_internal
  } else if (criterion_internal %in% 11L:14L) {
    onset     <- "thermal"
    criterion <- criterion_internal - 10L
  } else {
    stop("Unknown criterion number: ", criterion_internal, call. = FALSE)
  }
  
  list(
    onset              = onset,
    fixed_day          = NA_integer_,
    window_start       = window_start,
    window_length      = window_length,
    criterion_internal = criterion_internal,
    criterion          = criterion,
    preset_value       = preset_value,
    successive_days    = successive_days,
    occurrences        = occurrences
  )
}
