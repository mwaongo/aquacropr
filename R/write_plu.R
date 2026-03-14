#' Write AquaCrop Rainfall File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) rainfall (.PLU) file containing daily
#' precipitation data for use in crop simulations.
#'
#' @param path Directory path where the output .PLU file will be written. Default = "CLIMATE/"
#' @param site_name Station name or identifier for the weather station. Default = NULL (extracted from data if available)
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
#' in the specified directory with the format: <<site_name>>.PLU containing daily rainfall data
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
#'   site_name = "Wakanda_Station",
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
    site_name = NULL,
    data = NULL,
    var_name = "rain",
    syear = NULL,
    eyear = NULL,
    eol = NULL,
    record_type = 1,
    first_day = 1,
    first_month = 1) {

  .write_climate_file(
    path = path,
    site_name = site_name,
    data = data,
    var_cols = var_name,
    header_var_name = "rain",
    file_ext = ".PLU",
    syear = syear,
    eyear = eyear,
    eol = eol,
    record_type = record_type,
    first_day = first_day,
    first_month = first_month,
    col_widths = 10
  )
}
