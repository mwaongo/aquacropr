#' Write AquaCrop Reference Evapotranspiration File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) reference evapotranspiration (.ETo) file
#' containing daily ETo data for use in crop simulations.
#'
#' @param path Directory path where the output .ETo file will be written. Default = "weather/"
#' @param stn Station name or identifier for the weather station. Default = NULL (extracted from data if available)
#' @param data Data frame containing daily ETo data. Can have either:
#'   - Columns: station, year, month, day, et0 (legacy format), or
#'   - Columns: year, month, day, et0 (new format)
#' @param var_name Name of the ETo column in the data frame. Default = "et0"
#' @param syear Start year of the data period (extracted from data if not provided). Default = NULL
#' @param eyear End year of the data period (extracted from data if not provided). Default = NULL
#' @param eol End-of-line character style for the output file.
#'   Options: "windows", "unix", "linux", or "macOS". Default = "windows"
#' @param record_type Type of temporal aggregation. Options: 1 = daily, 2 = 10-daily, 3 = monthly. Default = 1
#' @param first_day First day of record. For 10-daily: 1, 11, or 21; For monthly: 1. Default = 1
#' @param first_month First month of record (1-12). Default = 1
#'
#' @return
#' Invisibly returns the full path to the created .ETo file. Creates a formatted .ETo file
#' in the specified directory with the format: <<stn>>.ETo containing daily reference
#' evapotranspiration data as required by AquaCrop v7.0.
#'
#' @examples
#' \dontrun{
#' # Load example weather data
#' data("weather")
#'
#' # Write ETo file with default column name
#' write_eto(
#'   path = "weather/",
#'   stn = "Wakanda_Station",
#'   data = weather
#' )
#'
#' # Write ETo file with custom column name
#' write_eto(
#'   path = "weather/",
#'   stn = "Wakanda_Station",
#'   data = weather,
#'   var_name = "eto_calculated"
#' )
#'
#' # Legacy format (backward compatibility)
#' write_eto(data = weather_with_station, path = "climate/")
#' }
#'
#' @seealso
#' \code{\link{weather}} for example weather data,
#' \code{\link{write_plu}} for writing rainfall files,
#' \code{\link{write_tnx}} for writing temperature files
#'
#' @export
write_eto <- function(
    path = "weather/",
    stn = NULL,
    data = NULL,
    var_name = "et0",
    syear = NULL,
    eyear = NULL,
    eol = "windows",
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
    var_name = "et0",
    stn = stn,
    syear = syear,
    eyear = eyear,
    eol = eol,
    record_type = record_type,
    first_day = first_day,
    first_month = first_month
  )

  # Prepare data for output (select only the ETo column)
  eto_data <- data %>%
    dplyr::select(!!rlang::sym(var_name))

  # Write file
  output_file <- paste0(path, stn_formatted, ".ETo")

  readr::write_file(x = header, file = output_file)

  write_fwf(eto_data, file = output_file, width = 10, justify = "r")

  # Return invisibly with file path
  invisible(output_file)
}


#' @rdname write_eto
#' @export
write_et0 <- write_eto
