#' Get Crop Stages Lengths and Calendar
#'
#' @description
#' Compute crop stage lengths and calendar from crop cycle length. This function
#' calculates the duration of different phenological stages based on the total
#' crop cycle length, scaled from a reference 90-day cycle.
#'
#' @param crop_cycle Positive integer representing the total length of the
#'   crop cycle in days. Must be a whole number > 0. Default = 90 days.
#'
#' @details
#' The function calculates six crop stage variables based on proportions from a
#' reference 90-day crop cycle:
#' \itemize{
#'   \item \strong{var_52}: Days from sowing to emergence (8/90 of cycle = ~9%)
#'   \item \strong{var_53}: Days from sowing to maximum canopy cover (100% of cycle)
#'   \item \strong{var_54}: Days from sowing to start of canopy senescence (65/90 of cycle = ~72%)
#'   \item \strong{var_55}: Days from sowing to maturity (100% of cycle)
#'   \item \strong{var_56}: Days from sowing to start of yield formation (52/90 of cycle = ~58%)
#'   \item \strong{var_57}: Duration of yield formation period (20/90 of cycle = ~22%)
#' }
#'
#' All values are rounded to the nearest integer day.
#'
#' @return
#' A named list of six integer values (var_52 through var_57) representing crop
#' stage durations in days. This list can be passed directly to \code{write_cro()}
#' via the \code{params} argument to create a crop file with custom phenology.
#'
#' @examples
#' # Default 90-day cycle
#' calculate_crop_stages()
#'
#' # 120-day maize cycle
#' maize_stages <- calculate_crop_stages(crop_cycle = 120)
#'
#' # Use with write_cro to create a crop file
#' \dontrun{
#' stages <- calculate_crop_stages(crop_cycle = 130)
#' write_cro(
#'   path = "crop/",
#'   crop_name = "maize-130d",
#'   params = stages
#' )
#' }
#'
#' @export

calculate_crop_stages <- function(crop_cycle = 90) {
  # Input validation
  if (!is.numeric(crop_cycle)) {
    stop(
      "crop_cycle must be a positive integer. Received: ", class(crop_cycle)[1],
      "\nProvide the crop cycle length in days (e.g., 90, 120, 150)"
    )
  }

  if (length(crop_cycle) != 1) {
    stop(
      "crop_cycle must be a single value. Received vector of length: ",
      length(crop_cycle)
    )
  }

  if (crop_cycle <= 0) {
    stop(
      "crop_cycle must be positive (> 0). Received: ", crop_cycle,
      "\nProvide a valid crop cycle length in days"
    )
  }

  if (crop_cycle != as.integer(crop_cycle)) {
    stop(
      "crop_cycle must be an integer. Received: ", crop_cycle,
      "\nCrop cycle length must be a whole number of days (e.g., 90, not 90.5)"
    )
  }

  if (crop_cycle < 30) {
    warning(
      "crop_cycle (", crop_cycle, " days) is unusually short.",
      "\nTypical crop cycles are 60-180 days. Verify this is correct."
    )
  }

  if (crop_cycle > 365) {
    warning(
      "crop_cycle (", crop_cycle, " days) exceeds one year.",
      "\nThis is unusual for annual crops. Verify this is correct."
    )
  }

  # Reference cycle length (90 days)
  reference_cycle <- 90

  # Calculate stage durations based on reference proportions
  # var_52: Days to emergence (8/90 ≈ 8.9% of cycle)
  var_52 <- round(8 * crop_cycle / reference_cycle)

  # var_53: Days to maximum canopy cover (100% of cycle)
  var_53 <- round(crop_cycle)

  # var_54: Days to start of senescence (65/90 ≈ 72.2% of cycle)
  var_54 <- round(65 * crop_cycle / reference_cycle)

  # var_55: Days to maturity (100% of cycle)
  var_55 <- round(crop_cycle)

  # var_56: Days to start of yield formation (52/90 ≈ 57.8% of cycle)
  var_56 <- round(52 * crop_cycle / reference_cycle)

  # var_57: Duration of yield formation (20/90 ≈ 22.2% of cycle)
  var_57 <- round(20 * crop_cycle / reference_cycle)

  # Create named list of parameters
  params <- list(
    var_52 = as.integer(var_52),
    var_53 = as.integer(var_53),
    var_54 = as.integer(var_54),
    var_55 = as.integer(var_55),
    var_56 = as.integer(var_56),
    var_57 = as.integer(var_57)
  )

  # Add informative names as attributes for user reference
  attr(params, "description") <- c(
    var_52 = "Days to emergence",
    var_53 = "Days to maximum canopy",
    var_54 = "Days to senescence",
    var_55 = "Days to maturity",
    var_56 = "Days to yield formation",
    var_57 = "Yield formation duration"
  )

  return(params)
}
