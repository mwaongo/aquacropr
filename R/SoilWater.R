#' Soil Water Characteristics Based on Soil Texture
#'
#' @description
#' Soil hydraulic properties and water retention characteristics for 13 soil texture
#' classes. These values are used by \code{\link{write_sol}} to create AquaCrop soil
#' profile (.SOL) files with appropriate hydraulic parameters for each texture class.
#'
#' @format A tibble with 13 rows and 9 variables:
#' \describe{
#'   \item{sat}{Numeric. Saturation point (volumetric water content at saturation, vol%)}
#'   \item{fc}{Numeric. Field capacity (volumetric water content at -33 kPa, vol%)}
#'   \item{wp}{Numeric. Wilting point (volumetric water content at -1500 kPa, vol%)}
#'   \item{ksat}{Numeric. Saturated hydraulic conductivity (mm/day)}
#'   \item{penetrability}{Numeric. Root zone expansion rate (% per day, typically 100)}
#'   \item{gravel}{Numeric. Soil gravel fraction (vol%, typically 0 for standard textures)}
#'   \item{cra}{Numeric. Coefficient a for capillary rise calculation (dimensionless)}
#'   \item{crb}{Numeric. Coefficient b for capillary rise calculation (dimensionless)}
#'   \item{description}{Character. USDA soil texture class name (lowercase)}
#' }
#'
#' @details
#' ## Soil Texture Classes:
#'
#' The 12 standard USDA soil texture classes plus one special class:
#'
#' **Coarse Textures (High drainage, low water retention):**
#' \itemize{
#'   \item sand: SAT=36%, FC=13%, WP=6%, Ksat=3000 mm/day
#'   \item loamy sand: SAT=38%, FC=16%, WP=8%, Ksat=2200 mm/day
#'   \item sandy loam: SAT=41%, FC=22%, WP=10%, Ksat=1200 mm/day
#' }
#'
#' **Medium Textures (Moderate drainage and retention):**
#' \itemize{
#'   \item loam: SAT=46%, FC=31%, WP=15%, Ksat=500 mm/day
#'   \item silt loam: SAT=46%, FC=33%, WP=13%, Ksat=575 mm/day
#'   \item silt: SAT=43%, FC=33%, WP=9%, Ksat=500 mm/day
#' }
#'
#' **Fine Textures (Low drainage, high water retention):**
#' \itemize{
#'   \item sandy clay loam: SAT=47%, FC=32%, WP=20%, Ksat=225 mm/day
#'   \item clay loam: SAT=50%, FC=39%, WP=23%, Ksat=125 mm/day
#'   \item silty clay loam: SAT=52%, FC=44%, WP=23%, Ksat=150 mm/day
#'   \item sandy clay: SAT=50%, FC=39%, WP=27%, Ksat=35 mm/day
#'   \item silty clay: SAT=54%, FC=50%, WP=32%, Ksat=100 mm/day
#'   \item clay: SAT=55%, FC=54%, WP=39%, Ksat=35 mm/day
#' }
#'
#' **Special Class:**
#' \itemize{
#'   \item impermeable: SAT=0.5%, FC=0.3%, WP=0.1%, Ksat=0 mm/day (for restrictive layers)
#' }
#'
#' ## Capillary Rise Coefficients:
#'
#' The cra and crb coefficients are used in AquaCrop's capillary rise calculations
#' to estimate water movement from groundwater tables to the root zone. These are
#' texture-specific empirical parameters.
#'
#' @source
#' AquaCrop v7.0 (August 2022). FAO, Rome, Italy.
#'
#' Values based on standard USDA soil texture classification and typical hydraulic
#' properties for each texture class.
#'
#' @examples
#' # View all soil texture data
#' data(SoilWater)
#' SoilWater
#'
#' # Get properties for loam soil
#' SoilWater[SoilWater$description == "loam", ]
#'
#' # Calculate available water capacity (FC - WP)
#' SoilWater$awc <- SoilWater$fc - SoilWater$wp
#'
#' # Find textures with high Ksat (> 500 mm/day)
#' SoilWater[SoilWater$ksat > 500, c("description", "ksat")]
#'
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{SWOData}} for soil initial water content defaults
#'   \item \code{\link{write_sol}} for creating soil profile files using this data
#'   \item \code{\link{write_swo}} for creating soil initial water content files
#' }
#'
"SoilWater"
