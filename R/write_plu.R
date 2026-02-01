#' Write AquaCrop Rainfall File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) rainfall (.PLU) file containing daily
#' precipitation data for use in crop simulations.
#'
#' @param path Directory path where the output .PLU file will be written. Default = "CLIMATE/"
#' @param stn Station name or identifier for the weather station. Default = NULL (extracted from data if available)
#' @param data Data frame containing daily rainfall data. Can have either:
#'   - Columns: station, year, month, day, rain (legacy format), or
#'   - Columns: year, month, day, rain (new format)
#' @param var_name Name of the rainfall column in the data frame. Default = "rain"
#' @param syear Start year of the data period (extracted from data if not provided). Default = NULL
#' @param eyear End year of the data period (extracted from data if not provided). Default = NULL
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#'   Options: "windows", "unix", "linux", or "macOS". Default = "windows"
#' @param record_type Type of temporal aggregation. Options: 1 = daily, 2 = 10-daily, 3 = monthly. Default = 1
#' @param first_day First day of record. For 10-daily: 1, 11, or 21; For monthly: 1. Default = 1
#' @param first_month First month of record (1-12). Default = 1
#'
#' @family AquaCrop file writers
#' @return
#' Invisibly returns the full path to the created .PLU file. Creates a formatted .PLU file
#' in the specified directory with the format: <<stn>>.PLU containing daily rainfall data
#' as required by AquaCrop v7.0.
#'
#' @examples
#' \dontrun{
#' # Load example weather data
#' data("weather")
#'
#' # Write rainfall file with default column name
#' write_plu(
#'   path = "weather/",
#'   stn = "Wakanda_Station",
#'   data = weather
#' )
#'
#' # Legacy format (backward compatibility)
#' write_plu(data = weather_with_station, path = "climate/")
#' }
#'
#' @seealso
#' \code{\link{weather}} for example weather data,
#' \code{\link{write_eto}} for writing ETo files,
#' \code{\link{write_tnx}} for writing temperature files
#'
#' @export
write_plu <- function(
    path = "CLIMATE/",
    stn = NULL,
    data = NULL,
    var_name = "rain",
    syear = NULL,
    eyear = NULL,
    eol = NULL,
    record_type = 1,
    first_day = 1,
    first_month = 1) {
  # Handle NULL or invalid data
  if (is.null(data) || !is.data.frame(data)) {
    stop(
      "data must be a data frame with columns: year, month, day, ", var_name,
      "\nReceived: ", class(data)[1]
    )
  }

  # Extract station from data if available and stn not provided
  if (is.null(stn) && "station" %in% names(data)) {
    stn <- utils::head(data, 1) %>%
      dplyr::pull("station")
  }

  # Set default station if still NULL
  if (is.null(stn)) {
    stn <- "station"
  }

  # Validate required columns
  required_cols <- c("year", "month", "day", var_name)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      "\nRequired columns are: year, month, day, ", var_name
    )
  }

  # Extract or validate years
  if (is.null(syear)) {
    syear <- utils::head(data, 1) %>%
      dplyr::pull("year")
  }

  if (is.null(eyear)) {
    eyear <- utils::tail(data, 1) %>%
      dplyr::pull("year")
  }

  # Ensure trailing slash on path
  path <- .add_trailing_slash(path)

  # Create directory if it doesn't exist
  fs::dir_create(path, recurse = TRUE)

  # Format station name for filename
  stn_formatted <- snakecase::to_any_case(stn, case = "snake", sep_out = "_")

  # Get header
  header <- .get_header(
    var_name = "rain",
    stn = stn,
    syear = syear,
    eyear = eyear,
    eol = eol,
    record_type = record_type,
    first_day = first_day,
    first_month = first_month
  )

  # Prepare data for output (select only the rainfall column)
  rain_data <- data %>%
    dplyr::select(!!rlang::sym(var_name))

  # Write file
  output_file <- paste0(path, stn_formatted, ".PLU")

  readr::write_file(x = header, file = output_file)

  write_fwf(rain_data, file = output_file, width = 10, justify = "r")

  # Return invisibly with file path
  invisible(output_file)
}
