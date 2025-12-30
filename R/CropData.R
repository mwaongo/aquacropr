#' AquaCrop Crop Parameters
#'
#' @description
#' Reference dataset containing all crop parameters for AquaCrop v7.0 (August 2022).
#' This dataset defines the structure and default values for crop (.CRO) files.
#'
#' @format A data frame with 83 rows and 5 columns:
#' \describe{
#'   \item{name}{Parameter name (var_02 to var_84)}
#'   \item{value}{Default parameter value}
#'   \item{description}{Description of the parameter}
#'   \item{fmt}{Format string for output (printf-style)}
#'   \item{width}{Column width for formatted output}
#' }
#'
#' @details
#' ## Parameter Categories
#'
#' **General Crop Settings (var_02-var_07)**
#' - var_02: AquaCrop Version (7.0)
#' - var_03: File protection (0 = protected, 1 = not protected)
#' - var_04: Crop type (1 = leafy, 2 = fruit/grain, 3 = root/tuber)
#' - var_05: Establishment method (0 = regrowth, 1 = sown, 2 = transplanted)
#' - var_06: Crop cycle determination (0 = by GDD, 1 = by calendar days)
#' - var_07: Soil water depletion method
#'
#' **Temperature Thresholds (var_08-var_09, var_27-var_28)**
#' - var_08: Base temperature (°C) below which crop development stops
#' - var_09: Upper temperature (°C) above which crop development stops
#' - var_27: Minimum air temperature for pollination (°C)
#' - var_28: Maximum air temperature for pollination (°C)
#' - var_29: Minimum growing degrees required (°C-day)
#'
#' **Water Stress Parameters (var_11-var_19)**
#' - var_11: Soil water depletion for canopy expansion (p_exp upper threshold)
#' - var_12: Soil water depletion for canopy expansion (p_exp lower threshold)
#' - var_13: Shape factor for canopy expansion water stress
#' - var_14: Soil water depletion for stomatal closure (p_sto)
#' - var_15: Shape factor for stomatal stress
#' - var_16: Soil water depletion for canopy senescence (p_sen)
#' - var_17: Shape factor for senescence water stress
#' - var_18: Sum of ETo during dormant period (mm)
#' - var_19: Soil water depletion for pollination failure (p_pol)
#'
#' **Aeration and Fertility (var_20-var_21)**
#' - var_20: Anaerobiotic point (Vol% above saturation)
#' - var_21: Soil fertility stress (0-100%, 0 = no stress)
#'
#' **Stress Response Shapes (var_22-var_26)**
#' - var_22: Shape factor for canopy cover response to fertility
#' - var_23: Shape factor for maximum canopy cover response to fertility
#' - var_24: Shape factor for crop water productivity response to fertility
#' - var_25: Shape factor for decline of canopy cover response to fertility
#'
#' **Salinity Tolerance (var_30-var_34)**
#' - var_30: ECe threshold for no yield reduction (dS/m)
#' - var_31: ECe at 100% yield reduction (dS/m)
#' - var_33: Calibrated CC distortion due to salinity (%)
#' - var_34: Calibrated stomatal stress response to salinity (%)
#'
#' **Evapotranspiration (var_35-var_36)**
#' - var_35: Crop coefficient at full canopy (KcTr,x)
#' - var_36: Decline of crop coefficient during senescence (%/day)
#'
#' **Root Development (var_37-var_41)**
#' - var_37: Minimum effective rooting depth (m)
#' - var_38: Maximum effective rooting depth (m)
#' - var_39: Shape factor for root zone expansion
#' - var_40: Maximum root water extraction in top quarter (m³/m³/day)
#' - var_41: Maximum root water extraction in bottom quarter (m³/m³/day)
#'
#' **Canopy Development (var_42-var_51)**
#' - var_42: Effect of canopy on reducing soil evaporation (%)
#' - var_43: Soil surface covered by individual seedling (cm²)
#' - var_44: Canopy size at planting for transplanted crops (cm²)
#' - var_45: Plant density (plants/ha)
#' - var_46: Canopy Growth Coefficient (CGC, fraction per day)
#' - var_47-var_49: Perennial crop decline parameters
#' - var_50: Maximum canopy cover (CCx, 0-1 fraction)
#' - var_51: Canopy Decline Coefficient (CDC, fraction per day)
#'
#' **Phenology - Calendar Days (var_52-var_57)**
#' - var_52: Days from sowing to emergence
#' - var_53: Days from sowing to maximum rooting depth
#' - var_54: Days from sowing to start of senescence
#' - var_55: Days from sowing to maturity
#' - var_56: Days from sowing to flowering
#' - var_57: Length of flowering stage (days)
#'
#' **Harvest Index Development (var_58-var_68)**
#' - var_58: Crop determinancy (0 = indeterminate, 1 = determinate)
#' - var_59: Excess of potential fruits (%)
#' - var_60: Building up of HI starts at (% of cycle)
#' - var_61: Water Productivity normalized for ETo and CO2 (g/m²)
#' - var_62: Water Productivity for reference CO2 concentration (%)
#' - var_63: Sink strength - biomass to storage organs (%)
#' - var_64: Reference Harvest Index (HIo, %)
#' - var_65: Possible increase of HI due to water stress before flowering (%)
#' - var_66: Coefficient for positive impact of restricted vegetative growth (%)
#' - var_67: Coefficient for negative impact of stomatal closure (%)
#' - var_68: Allowable maximum increase of specified HI (%)
#'
#' **Phenology - Growing Degree Days (var_69-var_77)**
#' - var_69-var_74: GDD equivalents of calendar day parameters (-9 if not used)
#' - var_75: CGC for GDD (fraction per °C-day)
#' - var_76: CDC for GDD (fraction per °C-day)
#' - var_77: GDD for building up Harvest Index
#'
#' **Yield and Perennials (var_78-var_84)**
#' - var_78: Dry matter content of fresh yield (%)
#' - var_79: Minimum rooting depth in first year for perennials (m)
#' - var_80: Crop transplanted in first year (0 = no, 1 = yes)
#' - var_81: Transfer of assimilates from above ground parts (0 = no, 1 = yes)
#' - var_82: Number of days at end of season for assimilate buildup
#' - var_83: Percentage of assimilates transferred to root system (%)
#' - var_84: Percentage of stored assimilates transferred to above ground (%)
#'
#' @examples
#' # View available crop parameters
#' data("CropData")
#' head(CropData)
#'
#' # Find specific parameters
#' CropData[CropData$name == "var_35", ] # Crop coefficient
#' CropData[CropData$name == "var_50", ] # Maximum canopy cover
#'
#' # See all phenology parameters
#' CropData[52:57, ]
#'
#' @seealso \code{\link{write_cro}} for writing crop files using these parameters
#' @source AquacropV7.0 (Août 2022) .FAO 2022. Rome, Italy
"CropData"
