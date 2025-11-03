#' Calculate Harvest Index Building Parameters
#'
#' @description
#' Calculate the timing parameters for harvest index (HI) building in AquaCrop.
#' The harvest index represents the ratio of harvested yield to total above-ground biomass.
#'
#' @param flowering_date Integer. Number of days from sowing/planting to flowering.
#'   Must be positive. Default = 35 days
#'
#' @details
#' This function calculates two key AquaCrop parameters:
#' \itemize{
#'   \item \strong{var_60}: Start of harvest index building (days from sowing)
#'     Set equal to flowering date
#'   \item \strong{var_64}: Start of linear harvest index increase (days from sowing)
#'     Set 5 days before flowering (flowering_date - 5)
#' }
#'
#' The harvest index typically starts building around flowering and increases
#' linearly during grain filling until maturity.
#'
#' @return Named list with two elements:
#' \describe{
#'   \item{var_60}{Days to start of HI building}
#'   \item{var_64}{Days to start of linear HI increase}
#' }
#'
#' @examples
#' # Default maize flowering (35 days)
#' calculate_harvest_index_timing(35)
#' # Returns: list(var_60 = 35, var_64 = 30)
#'
#' # Wheat flowering (90 days)
#' calculate_harvest_index_timing(90)
#' # Returns: list(var_60 = 90, var_64 = 85)
#'
#' @export

calculate_harvest_index_timing <- function(flowering_date = 35) {

  # Validate flowering_date
  if (!is.numeric(flowering_date)) {
    stop(
      "flowering_date must be numeric (days). Received: ",
      class(flowering_date)[1]
    )
  }

  if (length(flowering_date) != 1) {
    stop(
      "flowering_date must be a single value. Received vector of length: ",
      length(flowering_date)
    )
  }

  if (flowering_date <= 0) {
    stop(
      "flowering_date must be positive (> 0 days). Received: ",
      flowering_date
    )
  }

  if (flowering_date < 20) {
    warning(
      "flowering_date (", flowering_date, " days) is unusually early.",
      "\nTypical values are 30-120 days depending on crop."
    )
  }

  if (flowering_date > 200) {
    warning(
      "flowering_date (", flowering_date, " days) is unusually late.",
      "\nTypical values are 30-120 days for annual crops."
    )
  }

  # Calculate HI timing parameters
  var_60 <- as.integer(flowering_date)
  var_64 <- as.integer(flowering_date - 5)

  params <- list(var_60 = var_60, var_64 = var_64)

  return(params)
}


#' Build a Crop Parameter Set
#'
#' @description
#' Generate a complete set of crop parameters for AquaCrop by combining crop
#' phenology, harvest index timing, plant density, and other key parameters.
#' This is a convenience function for creating custom crop files.
#'
#' @param crop_cycle Integer. Total crop cycle length in days. Default = 90
#' @param flowering_date Integer. Days from sowing to flowering. Default = 35
#' @param plant_spacing Numeric. Distance between plants within rows (m). Default = 0.5
#' @param row_spacing Numeric. Distance between rows (m). Default = 0.8
#' @param var_28 Numeric. Minimum canopy cover (%) below which yield formation stops. Default = 40
#' @param var_38 Numeric. Soil fertility stress coefficient (0-1). Default = 1 (no stress)
#' @param var_08 Numeric. Base temperature (deg C) below which no growth occurs. Default = 8.0
#' @param var_09 Numeric. Upper temperature (deg C) above which growth is optimal. Default = 30
#'
#' @details
#' This function combines outputs from:
#' \itemize{
#'   \item \code{\link{calculate_crop_stages}}: Phenological stages (var_52-var_57)
#'   \item \code{\link{calculate_harvest_index_timing}}: HI timing (var_60, var_64)
#'   \item \code{\link{calculate_plant_density}}: Plant population (var_45)
#'   \item Additional key parameters provided as arguments
#' }
#'
#' All parameters are sorted by variable name (var_XX) for consistency with
#' AquaCrop file format.
#'
#' @return Named list of crop parameters suitable for passing to \code{\link{write_cro}}
#'
#' @examples
#' # Default maize parameters
#' build_crop_parameters()
#'
#' # Custom wheat parameters
#' build_crop_parameters(
#'   crop_cycle = 150,
#'   flowering_date = 90,
#'   plant_spacing = 0.05,
#'   row_spacing = 0.20,
#'   var_08 = 0,
#'   var_09 = 26
#' )
#'
#' # Use with write_cro
#' \dontrun{
#'   params <- build_crop_parameters(
#'     crop_cycle = 120,
#'     flowering_date = 60
#'   )
#'
#'   write_cro(
#'     crop_name = "custom-maize",
#'     params = params
#'   )
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{calculate_crop_stages}} for phenology parameters
#'   \item \code{\link{calculate_harvest_index_timing}} for HI parameters
#'   \item \code{\link{calculate_plant_density}} for plant population
#'   \item \code{\link{write_cro}} for writing crop files
#' }
#'
#' @export

build_crop_parameters <- function(
    crop_cycle = 90,
    flowering_date = 35,
    plant_spacing = 0.5,
    row_spacing = 0.8,
    var_28 = 40,
    var_38 = 1,
    var_08 = 8.0,
    var_09 = 30
) {

  # Validate additional parameters
  if (var_28 < 0 || var_28 > 100) {
    stop("var_28 (minimum CC for yield) must be between 0 and 100%. Received: ", var_28)
  }

  if (var_38 < 0 || var_38 > 1) {
    stop("var_38 (fertility stress) must be between 0 and 1. Received: ", var_38)
  }

  if (var_08 < -10 || var_08 > 30) {
    warning(
      "var_08 (base temperature = ", var_08, "deg C) is unusual.",
      "\nTypical range is 0-15 deg C for most crops."
    )
  }

  if (var_09 < 15 || var_09 > 45) {
    warning(
      "var_09 (upper temperature = ", var_09, "deg C) is unusual.",
      "\nTypical range is 25-35 deg C for most crops."
    )
  }

  if (var_08 >= var_09) {
    stop(
      "var_08 (base temp = ", var_08, "deg C) must be less than ",
      "var_09 (upper temp = ", var_09, "deg C)"
    )
  }

  # Calculate harvest index timing
  hi <- calculate_harvest_index_timing(flowering_date = flowering_date)

  # Calculate crop stages
  cs <- calculate_crop_stages(crop_cycle = crop_cycle)

  # Calculate plant density
  var_45 <- calculate_plant_density(
    plant_spacing = plant_spacing,
    row_spacing = row_spacing
  )

  # Combine all parameters
  params <- c(cs, hi)
  params$var_28 <- var_28
  params$var_38 <- var_38
  params$var_08 <- var_08
  params$var_09 <- var_09
  params$var_45 <- var_45

  # Sort by variable name for consistency
  params <- params[order(names(params))]

  return(params)
}


#' @rdname build_crop_parameters
#' @export
crop_params <- build_crop_parameters

