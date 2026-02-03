#' Write AquaCrop Temperature File
#'
#' @description
#' Write an AquaCrop v7.0 (August 2022) temperature (.Tnx) file containing daily
#' minimum and maximum temperature data for use in crop simulations.
#'
#' @param path Directory path where the output .Tnx file will be written. Default = "CLIMATE/"
#' @param stn Station name or identifier for the weather station. Default = NULL (extracted from data if available)
#' @param data Data frame containing daily temperature data. Can have either:
#'   - Columns: station, year, month, day, tmin, tmax (legacy format), or
#'   - Columns: year, month, day, tmin, tmax (new format)
#' @param var_name_min Name of the minimum temperature column in the data frame. Default = "tmin"
#' @param var_name_max Name of the maximum temperature column in the data frame. Default = "tmax"
#' @param syear Start year of the data period (extracted from data if not provided). Default = NULL
#' @param eyear End year of the data period (extracted from data if not provided). Default = NULL
#' @param eol End-of-line character style for the output file.
#'   Options: "windows", "unix", "linux", or "macOS". Default = "windows"
#' @param record_type Type of temporal aggregation. Options: 1 = daily, 2 = 10-daily, 3 = monthly. Default = 1
#' @param first_day First day of record. For 10-daily: 1, 11, or 21; For monthly: 1. Default = 1
#' @param first_month First month of record (1-12). Default = 1
#'
#' @family AquaCrop file writers
#' @return
#' Invisibly returns the full path to the created .Tnx file. Creates a formatted .Tnx file
#' in the specified directory with the format: <<stn>>.Tnx containing daily minimum and
#' maximum temperature data as required by AquaCrop v7.0.
#'
#' @examples
#' \dontrun{
#' # Load example weather data
#' data("weather")
#'
#' # Write temperature file with default column names
#' write_tnx(
#'   path = "weather/",
#'   stn = "Wakanda_Station",
#'   data = weather
#' )
#'
#' # Legacy format (backward compatibility)
#' write_tnx(data = weather_with_station, path = "climate/")
#' }
#'
#' @seealso
#' \code{\link{weather}} for example weather data,
#' \code{\link{write_plu}} for writing rainfall files,
#' \code{\link{write_eto}} for writing ETo files
#'
#' @export
write_tnx <- function(
    path = "CLIMATE/",
    stn = NULL,
    data = NULL,
    var_name_min = "tmin",
    var_name_max = "tmax",
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
    var_cols = c(var_name_min, var_name_max),
    header_var_name = "temperature",
    file_ext = ".Tnx",
    syear = syear,
    eyear = eyear,
    eol = eol,
    record_type = record_type,
    first_day = first_day,
    first_month = first_month,
    col_widths = c(10, 10)
  )
}
