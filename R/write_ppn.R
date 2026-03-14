#' Write an AquaCrop Program Parameter (.PPn) File
#'
#' Creates an AquaCrop program parameter file with default values that can be
#' selectively overridden via the \code{args} list.
#'
#' @param site_name Character. Station name (used to build the output filename).
#' @param args Named list of parameter values to override. Any parameter not
#'   supplied keeps its AquaCrop default value. Supported names (with defaults):
#'   \code{evap_decline} (4), \code{ke_x} (1.10), \code{cc_threshold_hi} (5),
#'   \code{root_expansion_start} (70), \code{max_root_expansion} (5.00),
#'   \code{shape_root_stress} (-6), \code{germination_swc} (20),
#'   \code{p_adjustment} (1.0), \code{aeration_days} (3),
#'   \code{senescence_exponent} (1.00), \code{p_sen_decrease} (12),
#'   \code{topsoil_thickness} (10), \code{evap_depth} (30),
#'   \code{cn_depth} (0.30), \code{cn_amc} (1), \code{salt_diffusion} (20),
#'   \code{salt_solubility} (100), \code{capillary_shape} (16),
#'   \code{tmin_default} (12.0), \code{tmax_default} (28.0),
#'   \code{gdd_method} (3), \code{rainfall_scs} (1),
#'   \code{eff_rainfall_pct} (70), \code{showers_per_decade} (2),
#'   \code{evap_reduction} (5).
#' @param path Character. Output directory. Default: \code{"SIMUL/"}.
#' @param eol Character. End-of-line style: \code{"windows"}, \code{"linux"},
#'   or \code{"macos"}. If \code{NULL} (default), auto-detected.
#'
#' @return Invisibly returns the output file path.
#'
#' @examples
#' \dontrun{
#' # Default parameters
#' write_ppn("grid_001")
#'
#' # Override temperature defaults
#' write_ppn("grid_001", args = list(tmin_default = 15.0, tmax_default = 32.0))
#' }
#'
#' @family AquaCrop file writers
#' @export
write_ppn <- function(site_name, args = list(), path = "SIMUL/", eol = NULL) {

  stopifnot(
    is.character(site_name) && length(site_name) == 1L,
    is.list(args),
    is.character(path)      && length(path)      == 1L
  )

  sep <- .get_eol(eol)

  # ---- Default parameter values -------------------------------------------
  defaults <- list(
    evap_decline         =   4L,
    ke_x                 =   1.10,
    cc_threshold_hi      =   5L,
    root_expansion_start =  70L,
    max_root_expansion   =   5.00,
    shape_root_stress    =  -6L,
    germination_swc      =  20L,
    p_adjustment         =   1.0,
    aeration_days        =   3L,
    senescence_exponent  =   1.00,
    p_sen_decrease       =  12L,
    topsoil_thickness    =  10L,
    evap_depth           =  30L,
    cn_depth             =   0.30,
    cn_amc               =   1L,
    salt_diffusion       =  20L,
    salt_solubility      = 100L,
    capillary_shape      =  16L,
    tmin_default         =  12.0,
    tmax_default         =  28.0,
    gdd_method           =   3L,
    rainfall_scs         =   1L,
    eff_rainfall_pct     =  70L,
    showers_per_decade   =   2L,
    evap_reduction       =   5L
  )

  # ---- Validate override names ---------------------------------------------
  bad <- setdiff(names(args), names(defaults))
  if (length(bad) > 0L) {
    stop("Unknown parameter(s) in args: ", paste(bad, collapse = ", "),
         call. = FALSE)
  }

  # ---- Merge overrides -----------------------------------------------------
  p <- utils::modifyList(defaults, args)

  # ---- Format helpers ------------------------------------------------------
  # Integer: right-justified, width = nchar(value) + 1
  fi <- function(x) {
    s <- as.character(as.integer(x))
    .format_string_fmt(s, "%s", nchar(s) + 1L, "right")
  }
  # Float: right-justified
  ff <- function(x, d = 2) {
    s <- sprintf(paste0("%.", d, "f"), as.numeric(x))
    .format_string_fmt(s, "%s", nchar(s) + 1L, "right")
  }

  # ---- Build lines ---------------------------------------------------------
  lines <- c(
    paste0(fi(p$evap_decline),         " : Evaporation decline factor for stage II"),
    paste0(ff(p$ke_x),                 " : Ke(x) Soil evaporation coefficient for fully wet and non-shaded soil surface cover"),
    paste0(fi(p$cc_threshold_hi),      " : Threshold for green CC below which HI can no longer increase (% cover)"),
    paste0(fi(p$root_expansion_start), " : Starting depth of root zone expansion curve (% of Zmin)"),
    paste0(ff(p$max_root_expansion),   " : Maximum allowable root zone expansion (fixed at 5 cm/day)"),
    paste0(fi(p$shape_root_stress),    " : Shape factor for effect water stress on root zone expansion"),
    paste0(fi(p$germination_swc),      " : Required soil water content in top soil for germination (% TAW)"),
    paste0(ff(p$p_adjustment, 1),      " : Adjustment factor for FAO-adjustment soil water depletion (p) by ETo"),
    paste0(fi(p$aeration_days),        " : Number of days after which deficient aeration is fully effective"),
    paste0(ff(p$senescence_exponent),  " : Exponent of senescence factor adjusting drop in photosynthetic activity of dying crop"),
    paste0(fi(p$p_sen_decrease),       " : Decrease of p(sen) once early canopy senescence is triggered (% of p(sen))"),
    paste0(fi(p$topsoil_thickness),    " : Thickness top soil (cm) in which soil water depletion has to be determined"),
    paste0(fi(p$evap_depth),           " : Depth [cm] of soil profile affected by water extraction by soil evaporation"),
    paste0(ff(p$cn_depth),             " : Considered depth (m) of soil profile for calculation of mean soil water content for CN adjustment"),
    paste0(fi(p$cn_amc),               " : CN is adjusted to Antecedent Moisture Class"),
    paste0(fi(p$salt_diffusion),       " : Salt diffusion factor (capacity for salt diffusion in micro pores) [%]"),
    paste0(fi(p$salt_solubility),      " : Salt solubility [g/liter]"),
    paste0(fi(p$capillary_shape),      " : Shape factor for effect of soil water content gradient on capillary rise"),
    paste0(ff(p$tmin_default, 1),      " : Default minimum temperature (degC) if no temperature file is specified"),
    paste0(ff(p$tmax_default, 1),      " : Default maximum temperature (degC) if no temperature file is specified"),
    paste0(fi(p$gdd_method),           " : Default method for the calculation of growing degree days"),
    paste0(fi(p$rainfall_scs),         " : Daily rainfall is estimated by USDA-SCS procedure (when input is 10-day/monthly rainfall)"),
    paste0(fi(p$eff_rainfall_pct),     " : Percentage of effective rainfall (when input is 10-day/monthly rainfall)"),
    paste0(fi(p$showers_per_decade),   " : Number of showers in a decade for run-off estimate (when input is 10-day/monthly rainfall)"),
    paste0(fi(p$evap_reduction),       " : Parameter for reduction of soil evaporation (when input is 10-day/monthly rainfall)")
  )

  # ---- Write file ----------------------------------------------------------
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  file <- file.path(path, paste0(site_name, ".PPn"))

  readr::write_file(
    x    = paste(paste0(lines, sep), collapse = ""),
    file = file
  )

  invisible(file)
}
