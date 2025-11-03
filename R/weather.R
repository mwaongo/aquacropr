#' Example Weather Data for AquaCrop
#'
#' @description
#' Daily weather data from Wakanda (fictional location) Automatic Weather Station.
#' Contains 30 years of climate records (1976-2005) suitable for testing and
#' demonstrating aquacroptools functions.
#'
#' @format A tibble with 10,958 rows and 8 variables:
#' \describe{
#'   \item{year}{Year of observation (1976-2005)}
#'   \item{month}{Month of observation (1-12, January-December)}
#'   \item{day}{Day of observation (1-31, depending on month)}
#'   \item{tmin}{Minimum daily temperature (°C)}
#'   \item{tmax}{Maximum daily temperature (°C)}
#'   \item{rain}{Daily rainfall amount (mm)}
#'   \item{et0}{Daily reference evapotranspiration - ETo (mm/day)}
#' }
#'
#' @details
#' This dataset provides complete daily weather records for AquaCrop simulations,
#' including:
#' - Temperature data for calculating growing degree days and thermal stress
#' - Rainfall data for water balance calculations
#' - Reference evapotranspiration (ETo) for crop water requirements
#'
#' The data covers a full 30-year period, allowing for:
#' - Long-term climate analysis
#' - Multiple growing season simulations
#' - Climate variability assessments
#'
#' @note
#' - All temperature values are in degrees Celsius (°C)
#' - Rainfall and ETo values are in millimeters (mm or mm/day)
#' - ETo represents reference evapotranspiration calculated using standardized methods
#' - Data is continuous with no missing values
#'
#' @examples
#' # Load the weather data
#' data("weather")
#' head(weather)
#'
#' # Summary statistics
#' summary(weather[, c("tmin", "tmax", "rain", "et0")])
#'
#' # Filter data for a specific year
#' weather_2000 <- weather[weather$year == 2000, ]
#'
#' # Calculate annual rainfall
#' annual_rain <- aggregate(rain ~ year, data = weather, FUN = sum)
#'
#' @seealso
#' \code{\link{write_plu}} for writing rainfall files,
#' \code{\link{write_eto}} for writing ETo files,
#' \code{\link{write_tnx}} for writing temperature files
#'
"weather"
