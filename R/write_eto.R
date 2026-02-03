#' Write AquaCrop Reference Evapotranspiration File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) reference evapotranspiration (.ETo) file
#' containing daily ETo data for use in crop simulations.
#'
#' @param path Directory path where the output .ETo file will be written. Default = "CLIMATE/"
#' @param stn Station name or identifier for the weather station. Default = NULL (extracted from data if available)
#' @param data Data frame containing daily ETo data. Can have either:
#'   - Columns: station, year, month, day, et0 (legacy format), or
#'   - Columns: year, month, day, et0 (new format)
#' @param var_name Name of the ETo column in the data frame. Default = "et0"
#' @param syear Start year of the data period (extracted from data if not provided). Default = NULL
#' @param eyear End year of the data period (extracted from data if not provided). Default = NULL
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
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
#' @family AquaCrop file writers
#' @export
#'
write_eto <- function(
    path = "CLIMATE/",
    stn = NULL,
    data = NULL,
    var_name = "et0",
    syear = NULL,
    eyear = NULL,
    eol = NULL,
    record_type = 1,
    first_day = 1,
    first_month = 1) {

  .write_climate_file(
    path = path,
    stn = stn,
    data = data,
    var_cols = var_name,
    header_var_name = "et0",
    file_ext = ".ETo",
    syear = syear,
    eyear = eyear,
    eol = eol,
    record_type = record_type,
    first_day = first_day,
    first_month = first_month,
    col_widths = 10
  )
}


#' @rdname write_eto
#' @export
write_et0 <- write_eto
