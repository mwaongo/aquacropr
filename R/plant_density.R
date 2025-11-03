#' Calculate Plant Population Density
#'
#' @description
#' Calculate the number of plants per hectare based on plant spacing and row spacing.
#' This is useful for determining planting density in AquaCrop simulations and field planning.
#'
#' @param plant_spacing Numeric. Distance between plants within a row (in meters).
#'   Must be positive (> 0). Default = 0.3 m
#' @param row_spacing Numeric. Distance between rows (in meters).
#'   Must be positive (> 0). Default = 0.8 m
#'
#' @details
#' The calculation uses the formula:
#' \deqn{plants/ha = \frac{10000}{plant\_spacing \times row\_spacing}}
#'
#' Where 10,000 m² = 1 hectare.
#'
#' The result is rounded up (ceiling) to ensure you have at least the calculated
#' number of plants per hectare.
#'
#' ## Common Planting Densities:
#'
#' **Maize:**
#' \itemize{
#'   \item Low density: 30,000-40,000 plants/ha (0.75m × 0.40m)
#'   \item Medium density: 50,000-70,000 plants/ha (0.75m × 0.20m)
#'   \item High density: 80,000-100,000 plants/ha (0.50m × 0.20m)
#' }
#'
#' **Wheat:**
#' \itemize{
#'   \item Typical: 250-400 plants/m² = 2,500,000-4,000,000 plants/ha
#' }
#'
#' **Soybean:**
#' \itemize{
#'   \item Low: 250,000-350,000 plants/ha (0.75m × 0.05m)
#'   \item High: 400,000-500,000 plants/ha (0.50m × 0.05m)
#' }
#'
#' @return Integer representing the number of plants per hectare (plants/ha)
#'
#' @examples
#' # Default maize spacing (0.3m × 0.8m)
#' calculate_plant_density()
#' # Returns: 41,667 plants/ha
#'
#' # Using alias
#' plant_density(plant_spacing = 0.40, row_spacing = 0.75)
#' # Returns: 33,334 plants/ha
#'
#' # Dense maize planting
#' calculate_plant_density(plant_spacing = 0.20, row_spacing = 0.60)
#' # Returns: 83,334 plants/ha
#'
#' # Cotton spacing
#' plant_density(0.15, 1.0)
#' # Returns: 66,667 plants/ha
#'
#' # Calculate for multiple scenarios
#' spacings <- data.frame(
#'   plant_spacing = c(0.20, 0.30, 0.40),
#'   row_spacing = c(0.75, 0.75, 0.75)
#' )
#' spacings$plants_per_ha <- mapply(
#'   calculate_plant_density,
#'   spacings$plant_spacing,
#'   spacings$row_spacing
#' )
#'
#' @export

calculate_plant_density <- function(plant_spacing = 0.3, row_spacing = 0.8) {

  # Validate plant_spacing
  if (!is.numeric(plant_spacing)) {
    stop(
      "plant_spacing must be numeric (in meters). Received: ",
      class(plant_spacing)[1]
    )
  }

  if (length(plant_spacing) != 1) {
    stop(
      "plant_spacing must be a single value. Received vector of length: ",
      length(plant_spacing)
    )
  }

  if (plant_spacing <= 0) {
    stop(
      "plant_spacing must be positive (> 0 meters). Received: ",
      plant_spacing
    )
  }

  if (plant_spacing > 5) {
    warning(
      "plant_spacing (", plant_spacing, " m) is unusually large.",
      "\nTypical values are 0.1-2.0 m. Verify this is correct."
    )
  }

  # Validate row_spacing
  if (!is.numeric(row_spacing)) {
    stop(
      "row_spacing must be numeric (in meters). Received: ",
      class(row_spacing)[1]
    )
  }

  if (length(row_spacing) != 1) {
    stop(
      "row_spacing must be a single value. Received vector of length: ",
      length(row_spacing)
    )
  }

  if (row_spacing <= 0) {
    stop(
      "row_spacing must be positive (> 0 meters). Received: ",
      row_spacing
    )
  }

  if (row_spacing > 5) {
    warning(
      "row_spacing (", row_spacing, " m) is unusually large.",
      "\nTypical values are 0.3-2.0 m. Verify this is correct."
    )
  }

  # Calculate plants per hectare
  # 1 hectare = 10,000 m²
  # Area per plant = plant_spacing × row_spacing (m²)
  # Plants per hectare = 10,000 / area_per_plant
  n <- ceiling(10000 / (plant_spacing * row_spacing))

  return(as.integer(n))
}


#' @rdname calculate_plant_density
#' @export
plant_density <- calculate_plant_density


