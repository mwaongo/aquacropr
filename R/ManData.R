#' AquaCrop Management Parameters
#'
#' @description
#' Reference dataset containing all management parameters for AquaCrop v7.0 (August 2022).
#' This dataset defines the structure and default values for management (.MAN) files,
#' which specify field and crop management practices.
#'
#' @format A data frame with 20 rows and 5 columns:
#' \describe{
#'   \item{name}{Parameter name (var_02 to var_21)}
#'   \item{value}{Default parameter value}
#'   \item{description}{Description of the parameter}
#'   \item{fmt}{Format string for output (printf-style)}
#'   \item{width}{Column width for formatted output}
#' }
#'
#' @details
#' ## Parameter Categories
#'
#' **General Settings (var_02)**
#' - var_02: AquaCrop Version (7.0)
#'
#' **Mulch Management (var_03-var_04)**
#' - var_03: Ground surface covered by mulches (0-100%)
#' - var_04: Effect of mulches on reducing soil evaporation (0-100%)
#'
#' **Soil Fertility (var_05)**
#' - var_05: Degree of soil fertility stress (0-100%, 0 = no stress, crop-specific effect)
#'
#' **Field Surface Practices (var_06-var_08)**
#' - var_06: Height of soil bunds (m, 0+ for no bunds)
#' - var_07: Surface runoff not affected by practices (0-100%)
#' - var_08: Surface runoff completely prevented (0-100%)
#'
#' **Weed Management (var_09-var_12)**
#' - var_09: Relative cover of weeds at canopy closure (0-100%)
#' - var_10: Increase of relative weed cover in mid-season (0+ %)
#' - var_11: Shape factor of CC expansion function in weed-infested field
#' - var_12: Replacement by weeds of self-thinned CC part for perennials (0-100%)
#'
#' **Multiple Cuttings (var_13-var_19)**
#' - var_13: Multiple cuttings consideration (0 = not considered, 1 = considered)
#' - var_14: Canopy cover after cutting (0-100%)
#' - var_15: Increase of Canopy Growth Coefficient after cutting (0+ %)
#' - var_16: First day of cutting window (1+ days, 1 = start of growth cycle)
#' - var_17: Number of days in cutting window (-9 = total cycle)
#' - var_18: Timing of multiple cuttings
#' - var_19: Time criterion for cuttings
#'
#' **Harvest Management (var_20-var_21)**
#' - var_20: Final harvest at maturity (0 = not considered, 1 = considered)
#' - var_21: Start of growing cycle in cuttings list (1+ or -9)
#'
#' ## Default Values:
#' The default values represent a baseline management scenario with:
#' \itemize{
#'   \item No mulching (var_03 = 0%)
#'   \item Moderate soil fertility stress (var_05 = 42%)
#'   \item No soil bunds (var_06 = 0 m)
#'   \item Moderate weed pressure (var_09 = 25%)
#'   \item No multiple cuttings (var_13 = 0)
#'   \item No special harvest timing (var_20 = 0)
#' }
#'
#' ## Special Values:
#' - \code{-9}: Indicates "not applicable" or "use default" for certain parameters
#' - \code{0}: Typically means "not considered" or "no effect" for management practices
#' - \code{1}: Start of growth cycle for timing parameters
#'
#' @examples
#' # View available management parameters
#' data("ManData")
#' head(ManData)
#'
#' # Find specific parameters
#' ManData[ManData$name == "var_05", ] # Soil fertility stress
#' ManData[ManData$name == "var_03", ] # Mulch coverage
#'
#' # See all mulch-related parameters
#' ManData[3:4, ]
#'
#' # See all weed-related parameters
#' ManData[8:11, ]
#'
#' # See all cutting-related parameters
#' ManData[13:19, ]
#'
#' @seealso
#' \code{\link{write_man}} for writing management files using these parameters,
#' \code{\link{CropData}} for crop parameter metadata
#' @source AquacropV7.0 (Août 2022) .FAO 2022. Rome, Italy
"ManData"
