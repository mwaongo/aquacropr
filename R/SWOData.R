#' Soil Initial Water Content Default Values
#'
#' @description
#' Default wilting point (WP) and electrical conductivity (ECe) values for 12 standard
#' USDA soil texture classes. These values are used by \code{\link{write_swo}} to
#' automatically set initial soil water conditions based on soil texture when creating
#' soil initial water content (.SW0) files.
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{texture}{Character. USDA soil texture class name (lowercase)}
#'   \item{wp}{Numeric. Wilting point water content (volumetric %, vol%)}
#'   \item{ece}{Numeric. Electrical conductivity (dS/m, default 0 for non-saline soils)}
#' }
#'
#' @details
#' ## Soil Texture Classes and Wilting Points:
#'
#' The 12 standard USDA soil texture classes with their respective wilting points:
#'
#' **Coarse Textures (Low water retention at WP):**
#' \itemize{
#'   \item sand: WP = 6%
#'   \item loamy sand: WP = 8%
#'   \item sandy loam: WP = 10%
#' }
#'
#' **Medium Textures (Moderate water retention at WP):**
#' \itemize{
#'   \item loam: WP = 15%
#'   \item silt loam: WP = 13%
#'   \item silt: WP = 9%
#' }
#'
#' **Fine Textures (High water retention at WP):**
#' \itemize{
#'   \item sandy clay loam: WP = 20%
#'   \item clay loam: WP = 23%
#'   \item silty clay loam: WP = 23%
#'   \item sandy clay: WP = 27%
#'   \item silty clay: WP = 32%
#'   \item clay: WP = 39%
#' }
#'
#' ## Water Content Relationships:
#'
#' From the wilting point (WP), other soil moisture levels can be estimated:
#' \itemize{
#'   \item \strong{Wilting Point (WP)}: Minimum water for plant survival (-1500 kPa)
#'   \item \strong{Field Capacity (FC)}: WP × 1.5 (approximate, -33 kPa)
#'   \item \strong{Saturation (SAT)}: WP × 2.5 (approximate, 0 kPa)
#' }
#'
#' ## Electrical Conductivity:
#'
#' Default ECe values are 0 dS/m (non-saline). Typical ECe ranges:
#' \itemize{
#'   \item 0-2 dS/m: Non-saline (most crops unaffected)
#'   \item 2-4 dS/m: Slightly saline (sensitive crops affected)
#'   \item 4-8 dS/m: Moderately saline (many crops affected)
#'   \item 8-16 dS/m: Strongly saline (only tolerant crops)
#'   \item >16 dS/m: Very strongly saline (few crops tolerate)
#' }
#'
#' @source
#' AquaCrop v7.0 (August 2022). FAO, Rome, Italy.
#'
#' Wilting point values represent typical volumetric water content at -1500 kPa
#' soil water potential for each USDA soil texture class.
#'
#' @examples
#' # View all soil texture data
#' data(SWOData)
#' SWOData
#'
#' # Get wilting point for loam
#' SWOData[SWOData$texture == "loam", "wp"]
#'
#' # Calculate field capacity for all textures
#' SWOData$fc_approx <- SWOData$wp * 1.5
#'
#' # Find textures with WP > 20%
#' SWOData[SWOData$wp > 20, ]
#'
#' # Compare with SoilWater data
#' data(SoilWater)
#' merge(SWOData, SoilWater[, c("description", "fc", "sat")],
#'       by.x = "texture", by.y = "description")
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{SoilWater}} for complete soil hydraulic properties
#'   \item \code{\link{write_swo}} for creating soil initial water content files using this data
#'   \item \code{\link{salinity_to_ece}} for converting salinity descriptions to ECe values
#' }
#'
"SWOData"
