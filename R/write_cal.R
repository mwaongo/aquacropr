#' Write AquaCrop Calendar (.CAL) File
#'
#' Creates an AquaCrop calendar file (.CAL) defining the onset of the growing
#' period. Three onset types are supported: a fixed calendar day, a
#' rainfall-based criterion, or a thermal criterion.
#'
#' User-facing criterion numbers run from 1 to 4 within each onset group
#' (1 to 5 for rainfall). They are converted internally to AquaCrop's
#' numbering scheme (rainfall passes through as-is; thermal criterion c
#' becomes c + 10).
#'
#' @param site_name Character. Name of the CAL file (without extension).
#'   Typically the station name.
#' @param onset Character. Onset type: "fixed", "rainfall", or "thermal".
#' @param fixed_day Integer. Calendar day (1-366) for the onset of the
#'   growing period. Required when onset is "fixed".
#' @param window_start Integer. Day-number (1-366) of the start of the time
#'   window. Required when onset is "rainfall" or "thermal".
#' @param window_length Integer. Length of the time window in days. Required
#'   when onset is "rainfall" or "thermal".
#' @param criterion Integer. Criterion number within the onset group (1-4
#'   for thermal; 1-5 for rainfall).
#'   Rainfall criteria (onset = "rainfall"):
#'   \describe{
#'     \item{1}{Cumulative rainfall since start of window >= preset (mm).}
#'     \item{2}{Observed rainfall in N successive days >= preset (mm).
#'       Requires \code{successive_days}.}
#'     \item{3}{10-day rainfall >= preset (mm).}
#'     \item{4}{10-day rainfall >= preset fraction of 10-day ETo (percent).}
#'     \item{5}{Fuzzy logic index: cumulative rainfall x wet days x dry spell
#'       >= \code{fuzzy_threshold}. Requires \code{preset_value}
#'       (cum_rain_lower), \code{successive_days} (accum_days), and all
#'       \code{cum_rain_upper}, \code{wet_days_lower}, \code{wet_days_upper},
#'       \code{dry_spell_lower}, \code{dry_spell_upper},
#'       \code{fuzzy_threshold}.}
#'   }
#'   Thermal criteria (onset = "thermal"):
#'   \describe{
#'     \item{1}{Daily Tmin in each of N successive days >= preset (degC).
#'       Requires \code{successive_days}.}
#'     \item{2}{Daily Tmean in each of N successive days >= preset (degC).
#'       Requires \code{successive_days}.}
#'     \item{3}{Cumulative GDD in N successive days >= preset (degC).
#'       Requires \code{successive_days}.}
#'     \item{4}{Cumulative GDD since start of window >= preset (degC).}
#'   }
#' @param preset_value Numeric. Threshold value for the criterion.
#'   Unit: mm for rainfall criteria 1-3, percent for criterion 4,
#'   degC for all thermal criteria. For criterion 5 (fuzzy): lower bound
#'   of cumulative rainfall (mm), i.e. \code{cum_rain_lower}.
#' @param successive_days Integer. Number of consecutive days. Required for
#'   rainfall criteria 2 and 5 (where it represents \code{accum_days}) and
#'   thermal criteria 1, 2, and 3. Ignored for all other criteria.
#'   Default: NULL.
#' @param occurrences Integer. Number of times the criterion must be met
#'   before onset is triggered (1-3). Ignored for criterion 5.
#'   Default: 1.
#' @param cum_rain_upper Numeric. Upper bound of cumulative rainfall (mm)
#'   for the fuzzy gamma_1 function. Required for criterion 5.
#'   Must be > \code{preset_value}.
#' @param wet_days_lower Integer. Lower bound of wet-day count for the fuzzy
#'   gamma_2 function. Required for criterion 5.
#' @param wet_days_upper Integer. Upper bound of wet-day count for the fuzzy
#'   gamma_2 function. Required for criterion 5.
#'   Must be > \code{wet_days_lower}.
#' @param dry_spell_lower Integer. Lower bound of longest dry spell (days)
#'   for the fuzzy gamma_3 function. Required for criterion 5.
#' @param dry_spell_upper Integer. Upper bound of longest dry spell (days)
#'   for the fuzzy gamma_3 function. Required for criterion 5.
#'   Must be > \code{dry_spell_lower}.
#' @param fuzzy_threshold Numeric between 0 and 1. Defuzzification threshold.
#'   Required for criterion 5.
#' @param path Output directory path for CAL files. Default: "CAL/".
#' @param description Character. Description on the first line. Default:
#'   auto-generated based on onset type and criterion.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#'
#' @return Invisibly returns the output file path.
#'
#' @examples
#' \dontrun{
#' # Fixed onset on day 212
#' write_cal(
#'   site_name = "station_01",
#'   onset     = "fixed",
#'   fixed_day = 212
#' )
#'
#' # Rainfall criterion 1: cumulative rainfall >= 50 mm
#' write_cal(
#'   site_name     = "station_01",
#'   onset         = "rainfall",
#'   window_start  = 121,
#'   window_length = 92,
#'   criterion     = 1,
#'   preset_value  = 50,
#'   occurrences   = 1
#' )
#'
#' # Rainfall criterion 2: observed rainfall >= 30 mm in 3 successive days
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 2,
#'   preset_value    = 30,
#'   successive_days = 3,
#'   occurrences     = 1
#' )
#'
#' # Rainfall criterion 3: 10-day rainfall >= 40 mm
#' write_cal(
#'   site_name     = "station_01",
#'   onset         = "rainfall",
#'   window_start  = 121,
#'   window_length = 92,
#'   criterion     = 3,
#'   preset_value  = 40,
#'   occurrences   = 1
#' )
#'
#' # Rainfall criterion 4: 10-day rainfall >= 50 percent of ETo
#' write_cal(
#'   site_name     = "station_01",
#'   onset         = "rainfall",
#'   window_start  = 121,
#'   window_length = 92,
#'   criterion     = 4,
#'   preset_value  = 50,
#'   occurrences   = 1
#' )
#'
#' # Rainfall criterion 5: fuzzy logic onset (Waongo et al. 2014)
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 5,
#'   preset_value    = 20,   # cum_rain_lower
#'   successive_days = 3,    # accum_days
#'   cum_rain_upper  = 40,
#'   wet_days_lower  = 1,
#'   wet_days_upper  = 3,
#'   dry_spell_lower = 7,
#'   dry_spell_upper = 15,
#'   fuzzy_threshold = 0.5
#' )
#'
#' # Thermal criterion 1: daily Tmin >= 5 degC for 4 successive days
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "thermal",
#'   window_start    = 1,
#'   window_length   = 60,
#'   criterion       = 1,
#'   preset_value    = 5.0,
#'   successive_days = 4,
#'   occurrences     = 1
#' )
#'
#' # Thermal criterion 2: daily Tmean >= 10 degC for 3 successive days
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "thermal",
#'   window_start    = 1,
#'   window_length   = 60,
#'   criterion       = 2,
#'   preset_value    = 10.0,
#'   successive_days = 3,
#'   occurrences     = 1
#' )
#'
#' # Thermal criterion 3: cumulative GDD >= 20 degC in 8 successive days
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "thermal",
#'   window_start    = 1,
#'   window_length   = 60,
#'   criterion       = 3,
#'   preset_value    = 20.0,
#'   successive_days = 8,
#'   occurrences     = 1
#' )
#'
#' # Thermal criterion 4: cumulative GDD since start >= 600 degC
#' write_cal(
#'   site_name     = "station_01",
#'   onset         = "thermal",
#'   window_start  = 1,
#'   window_length = 60,
#'   criterion     = 4,
#'   preset_value  = 600.0,
#'   occurrences   = 1
#' )
#' }
#'
#' @references
#' Waongo, M., Laux, P., Traoré, S.B., Sanon, M., & Kunstmann, H. (2014).
#' A crop model and fuzzy rule based approach for optimizing maize planting
#' dates in Burkina Faso, West Africa.
#' \emph{Journal of Applied Meteorology and Climatology}, \strong{53}(3),
#' 598--613. \doi{10.1175/JAMC-D-13-0116.1}
#'
#' @family AquaCrop file writers
#' @export
write_cal <- function(
    site_name,
    onset,
    fixed_day       = NULL,
    window_start    = NULL,
    window_length   = NULL,
    criterion       = NULL,
    preset_value    = NULL,
    successive_days = NULL,
    occurrences     = 1L,
    # --- fuzzy extras (criterion 5 only) ---
    cum_rain_upper  = NULL,
    wet_days_lower  = NULL,
    wet_days_upper  = NULL,
    dry_spell_lower = NULL,
    dry_spell_upper = NULL,
    fuzzy_threshold = NULL,
    # ----------------------------------------
    path            = "CAL/",
    description     = NULL,
    version         = 7.1,
    eol             = NULL
) {

  onset <- match.arg(tolower(onset), choices = c("fixed", "rainfall", "thermal"))

  if (onset == "fixed") {
    if (is.null(fixed_day))
      stop("fixed_day is required when onset is 'fixed'.", call. = FALSE)
    if (fixed_day < 1L || fixed_day > 366L)
      stop("fixed_day must be between 1 and 366.", call. = FALSE)
    onset_code         <- 0L
    criterion_internal <- NULL

  } else {
    if (is.null(window_start) || is.null(window_length))
      stop("window_start and window_length are required for onset '",
           onset, "'.", call. = FALSE)

    valid_criteria <- if (onset == "rainfall") 1L:5L else 1L:4L
    if (is.null(criterion) || !criterion %in% valid_criteria)
      stop("criterion must be one of ",
           paste(valid_criteria, collapse = ", "),
           " for onset '", onset, "'.", call. = FALSE)

    if (onset == "thermal" && criterion == 5L)
      stop("criterion 5 (fuzzy logic) is only available for onset 'rainfall'.",
           call. = FALSE)

    criterion_internal <- if (onset == "thermal") criterion + 10L else criterion

    if (criterion == 5L) {
      # --- fuzzy validation ---
      fuzzy_args <- list(
        preset_value    = preset_value,    # cum_rain_lower
        successive_days = successive_days, # accum_days
        cum_rain_upper  = cum_rain_upper,
        wet_days_lower  = wet_days_lower,
        wet_days_upper  = wet_days_upper,
        dry_spell_lower = dry_spell_lower,
        dry_spell_upper = dry_spell_upper,
        fuzzy_threshold = fuzzy_threshold
      )
      null_args <- names(Filter(is.null, fuzzy_args))
      if (length(null_args) > 0L)
        stop("Missing fuzzy parameters: ",
             paste(null_args, collapse = ", "), ".", call. = FALSE)

      if (cum_rain_upper  <= preset_value)
        stop("cum_rain_upper must be > preset_value (cum_rain_lower).",
             call. = FALSE)
      if (wet_days_upper  <= wet_days_lower)
        stop("wet_days_upper must be > wet_days_lower.",   call. = FALSE)
      if (dry_spell_upper <= dry_spell_lower)
        stop("dry_spell_upper must be > dry_spell_lower.", call. = FALSE)
      if (fuzzy_threshold < 0 || fuzzy_threshold > 1)
        stop("fuzzy_threshold must be in [0, 1].", call. = FALSE)

      successive_days <- as.integer(successive_days)  # accum_days
      occurrences     <- 1L

    } else {
      # --- standard criteria 1-4 validation ---
      if (is.null(preset_value))
        stop("preset_value is required for onset '", onset, "'.", call. = FALSE)
      if (occurrences < 1L || occurrences > 3L)
        stop("occurrences must be between 1 and 3.", call. = FALSE)

      meta <- .cal_criterion_meta[[as.character(criterion_internal)]]
      if (meta$needs_succ && is.null(successive_days))
        stop("successive_days is required for ", onset,
             " criterion ", criterion, ".", call. = FALSE)
      if (!meta$needs_succ) successive_days <- -9L
    }

    onset_code <- 1L
  }

  if (is.null(description)) {
    description <- if (onset == "fixed") {
      paste0("Onset is specified by a fixed calendar day (day ", fixed_day, ")")
    } else if (criterion == 5L) {
      paste0("Fuzzy logic onset (cum_rain >= ", preset_value,
             " mm, wet_days >= ", wet_days_lower,
             ", dry_spell <= ", dry_spell_lower, " days)")
    } else {
      meta        <- .cal_criterion_meta[[as.character(criterion_internal)]]
      label_clean <- sub("^Criterion Nr \\((.+)\\)$", "\\1", meta$label)
      unit        <- if (onset == "thermal") "degC" else if (criterion == 4L) "%" else "mm"
      paste0(label_clean, " (here ", preset_value, " ", unit, ")")
    }
  }

  content <- .get_cal_header(
    description        = description,
    onset_code         = onset_code,
    version            = version,
    fixed_day          = fixed_day,
    window_start       = window_start,
    window_length      = window_length,
    criterion_internal = criterion_internal,
    preset_value       = preset_value,
    successive_days    = successive_days,
    occurrences        = occurrences,
    eol                = eol,
    # fuzzy extras
    cum_rain_upper  = cum_rain_upper,
    wet_days_lower  = wet_days_lower,
    wet_days_upper  = wet_days_upper,
    dry_spell_lower = dry_spell_lower,
    dry_spell_upper = dry_spell_upper,
    fuzzy_threshold = fuzzy_threshold
  )

  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  output_file <- paste0(path, site_name, ".CAL")
  readr::write_file(x = content, file = output_file)
  invisible(output_file)
}
