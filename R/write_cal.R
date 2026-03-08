#' Write AquaCrop Calendar (.CAL) File
#'
#' Creates an AquaCrop calendar file (.CAL) defining the onset of the growing
#' period. Three onset types are supported: a fixed calendar day, a
#' rainfall-based criterion, or a thermal criterion.
#'
#' User-facing criterion numbers run from 1 to 4 for standard AquaCrop
#' rainfall criteria, 5 for the generalised Sivakumar method, 6 for the
#' Marteau (2009) method, and 7 for the fuzzy logic method (Waongo et al.,
#' 2014). Thermal criteria run from 1 to 4 (converted internally to 11-14).
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
#' @param criterion Integer. Criterion number within the onset group.
#'   Rainfall criteria (onset = "rainfall"):
#'   \describe{
#'     \item{1}{Cumulative rainfall since start of window >= preset (mm).}
#'     \item{2}{Observed rainfall in N successive days >= preset (mm).
#'       Requires successive_days.}
#'     \item{3}{10-day rainfall >= preset (mm).}
#'     \item{4}{10-day rainfall >= preset fraction of 10-day ETo (percent).}
#'     \item{5}{Generalised Sivakumar: cumulative rainfall >= preset (mm) in
#'       successive_days days, with at least rday rainy days, and no dry spell
#'       (consecutive dry days) >= dspell within the following lookahead_days
#'       days. Requires successive_days, rday, dspell, lookahead_days.}
#'     \item{6}{Marteau (2009): first wet day (> 1 mm) in 1 or 2 consecutive
#'       days receiving >= preset mm total, followed by no rolling spell_days-day
#'       period with total rainfall < min_weekly_rain mm within the next
#'       lookahead_days days. Requires successive_days (1 or 2),
#'       min_weekly_rain, spell_days, lookahead_days.}
#'     \item{7}{Fuzzy logic (Waongo et al., 2014): three-component index
#'       (cumulative rainfall x wet-day count x dry spell) >=
#'       fuzzy_threshold. Requires preset_value (cum_rain_lower),
#'       successive_days (accum_days), cum_rain_upper, wet_days_lower,
#'       wet_days_upper, dry_spell_lower, dry_spell_upper, fuzzy_threshold.}
#'   }
#'   Thermal criteria (onset = "thermal"):
#'   \describe{
#'     \item{1}{Daily Tmin in each of N successive days >= preset (degC).
#'       Requires successive_days.}
#'     \item{2}{Daily Tmean in each of N successive days >= preset (degC).
#'       Requires successive_days.}
#'     \item{3}{Cumulative GDD in N successive days >= preset (degC).
#'       Requires successive_days.}
#'     \item{4}{Cumulative GDD since start of window >= preset (degC).}
#'   }
#' @param preset_value Numeric. Threshold value for the criterion.
#'   Unit: mm for rainfall criteria 1-3, 5, 6, 7; percent for criterion 4;
#'   degC for all thermal criteria. For criterion 7: lower bound of
#'   cumulative rainfall (mm), i.e. cum_rain_lower.
#' @param successive_days Integer. Number of consecutive days. Required for
#'   rainfall criteria 2, 5, 6 (1 or 2 for criterion 6), and 7 (accum_days),
#'   and thermal criteria 1, 2, and 3. Default: NULL.
#' @param occurrences Integer. Number of times the criterion must be met
#'   before onset is triggered (1-3). Ignored for criterion 7. Default: 1.
#' @param rday Integer. Minimum number of rainy days (> 0.1 mm) within the
#'   successive_days window. Required for criterion 5. Default: NULL.
#' @param dspell Integer. Maximum dry spell length (consecutive dry days)
#'   allowed in the look-ahead window. Required for criterion 5. Default: NULL.
#' @param lookahead_days Integer. Length (days) of the look-ahead window.
#'   Required for criteria 5 and 6. Classical values: 30 (Sivakumar),
#'   20 (Marteau). Default: NULL.
#' @param min_weekly_rain Numeric. Minimum total rainfall (mm) required in a
#'   rolling spell_days-day window within the look-ahead; windows with totals
#'   below this value are considered dry spells. Required for criterion 6.
#'   Classical Marteau value: 5 mm. Default: NULL.
#' @param spell_days Integer. Length (days) of the rolling window used to
#'   evaluate the dry spell condition in the look-ahead. Required for
#'   criterion 6. Classical Marteau value: 7 days. Default: NULL.
#' @param cum_rain_upper Numeric. Upper bound of cumulative rainfall (mm)
#'   for the fuzzy gamma_1 function. Required for criterion 7.
#'   Must be > preset_value.
#' @param wet_days_lower Integer. Lower bound of wet-day count for the fuzzy
#'   gamma_2 function. Required for criterion 7.
#' @param wet_days_upper Integer. Upper bound of wet-day count for the fuzzy
#'   gamma_2 function. Required for criterion 7.
#'   Must be > wet_days_lower.
#' @param dry_spell_lower Integer. Lower bound of longest dry spell (days)
#'   for the fuzzy gamma_3 function. Required for criterion 7.
#' @param dry_spell_upper Integer. Upper bound of longest dry spell (days)
#'   for the fuzzy gamma_3 function. Required for criterion 7.
#'   Must be > dry_spell_lower.
#' @param fuzzy_threshold Numeric between 0 and 1. Defuzzification threshold.
#'   Required for criterion 7.
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
#' # Rainfall criterion 5: generalised Sivakumar
#' # >= 20 mm in 3 days, >= 1 rainy day, max 7 consecutive dry days in 30
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 5,
#'   preset_value    = 20,
#'   successive_days = 3,
#'   rday            = 1,
#'   dspell          = 7,
#'   lookahead_days  = 30,
#'   occurrences     = 1
#' )
#'
#' # Rainfall criterion 6: Marteau (2009)
#' # >= 20 mm in 1 or 2 days, no 7-day period with < 5 mm in next 20 days
#' write_cal(
#'   site_name        = "station_01",
#'   onset            = "rainfall",
#'   window_start     = 121,
#'   window_length    = 92,
#'   criterion        = 6,
#'   preset_value     = 20,
#'   successive_days  = 2,
#'   min_weekly_rain  = 5,
#'   spell_days       = 7,
#'   lookahead_days   = 20,
#'   occurrences      = 1
#' )
#'
#' # Rainfall criterion 7: fuzzy logic (Waongo et al., 2014)
#' write_cal(
#'   site_name       = "station_01",
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 7,
#'   preset_value    = 20,   # cum_rain_lower
#'   successive_days = 3,    # accum_days
#'   cum_rain_upper  = 40,
#'   wet_days_lower  = 1,
#'   wet_days_upper  = 3,
#'   dry_spell_lower = 7,
#'   dry_spell_upper = 15,
#'   fuzzy_threshold = 0.5
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
    rday            = NULL,
    dspell          = NULL,
    lookahead_days  = NULL,
    min_weekly_rain = NULL,
    spell_days      = NULL,
    # --- fuzzy extras (criterion 7 only) ---
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

  # ---- Fixed onset ----------------------------------------------------------
  if (onset == "fixed") {
    if (is.null(fixed_day)) {
      stop("fixed_day is required when onset is 'fixed'.", call. = FALSE)
    }
    if (fixed_day < 1L || fixed_day > 366L) {
      stop("fixed_day must be between 1 and 366.", call. = FALSE)
    }
    onset_code         <- 0L
    criterion_internal <- NULL

    # ---- Rainfall / thermal onset ---------------------------------------------
  } else {
    if (is.null(window_start) || is.null(window_length)) {
      stop("window_start and window_length are required for onset '",
           onset, "'.", call. = FALSE)
    }

    valid_criteria <- if (onset == "rainfall") 1L:7L else 1L:4L
    if (is.null(criterion) || !criterion %in% valid_criteria) {
      stop("criterion must be one of ",
           paste(valid_criteria, collapse = ", "),
           " for onset '", onset, "'.", call. = FALSE)
    }
    if (is.null(preset_value)) {
      stop("preset_value is required for onset '", onset, "'.", call. = FALSE)
    }
    if (occurrences < 1L || occurrences > 3L) {
      stop("occurrences must be between 1 and 3.", call. = FALSE)
    }

    criterion_internal <- if (onset == "thermal") criterion + 10L else criterion

    # ---- Criterion 5: generalised Sivakumar ---------------------------------
    if (criterion == 5L) {
      if (is.null(successive_days)) {
        stop("successive_days is required for rainfall criterion 5 (Sivakumar).",
             call. = FALSE)
      }
      if (is.null(rday)) {
        stop("rday is required for rainfall criterion 5 (Sivakumar).",
             call. = FALSE)
      }
      if (is.null(dspell)) {
        stop("dspell is required for rainfall criterion 5 (Sivakumar).",
             call. = FALSE)
      }
      if (is.null(lookahead_days)) {
        stop("lookahead_days is required for rainfall criterion 5 (Sivakumar).",
             call. = FALSE)
      }
      if (rday > successive_days) {
        stop("rday cannot exceed successive_days.", call. = FALSE)
      }
      if (dspell >= lookahead_days) {
        stop("dspell must be strictly less than lookahead_days.", call. = FALSE)
      }

      # ---- Criterion 6: Marteau (2009) ----------------------------------------
    } else if (criterion == 6L) {
      if (is.null(successive_days) || !successive_days %in% 1L:2L) {
        stop("successive_days must be 1 or 2 for rainfall criterion 6 (Marteau).",
             call. = FALSE)
      }
      if (is.null(min_weekly_rain)) {
        stop("min_weekly_rain is required for rainfall criterion 6 (Marteau).",
             call. = FALSE)
      }
      if (is.null(spell_days)) {
        stop("spell_days is required for rainfall criterion 6 (Marteau).",
             call. = FALSE)
      }
      if (is.null(lookahead_days)) {
        stop("lookahead_days is required for rainfall criterion 6 (Marteau).",
             call. = FALSE)
      }
      if (spell_days >= lookahead_days) {
        stop("spell_days must be strictly less than lookahead_days.", call. = FALSE)
      }

      # ---- Criterion 7: fuzzy logic (Waongo et al., 2014) ---------------------
    } else if (criterion == 7L) {
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
        stop("cum_rain_upper must be > preset_value (cum_rain_lower).", call. = FALSE)
      if (wet_days_upper  <= wet_days_lower)
        stop("wet_days_upper must be > wet_days_lower.", call. = FALSE)
      if (dry_spell_upper <= dry_spell_lower)
        stop("dry_spell_upper must be > dry_spell_lower.", call. = FALSE)
      if (fuzzy_threshold < 0 || fuzzy_threshold > 1)
        stop("fuzzy_threshold must be between 0 and 1.", call. = FALSE)
      successive_days <- as.integer(successive_days)  # accum_days
      occurrences     <- 1L

      # ---- Standard AquaCrop criteria (1-4) -----------------------------------
    } else {
      meta <- .cal_criterion_meta[[as.character(criterion_internal)]]
      if (meta$needs_succ && is.null(successive_days)) {
        stop(
          "successive_days is required for ", onset, " criterion ", criterion, ".",
          call. = FALSE
        )
      }
      if (!meta$needs_succ) successive_days <- -9L
    }

    onset_code <- 1L
  }

  # ---- Build description ----------------------------------------------------
  if (is.null(description)) {
    description <- if (onset == "fixed") {
      paste0("Onset is specified by a fixed calendar day (day ", fixed_day, ")")

    } else if (criterion == 5L) {
      paste0(
        "Generalised Sivakumar: >= ", preset_value, " mm in ", successive_days,
        " day(s), >= ", rday, " rainy day(s), max ", dspell,
        " consecutive dry day(s) in ", lookahead_days, "-day look-ahead"
      )

    } else if (criterion == 6L) {
      paste0(
        "Marteau (2009): >= ", preset_value, " mm in ", successive_days,
        " day(s); no ", spell_days, "-day period < ", min_weekly_rain,
        " mm within ", lookahead_days, "-day look-ahead"
      )

    } else if (criterion == 7L) {
      paste0(
        "Fuzzy logic (Waongo et al. 2014): cum_rain >= ", preset_value,
        " mm, wet_days >= ", wet_days_lower,
        ", dry_spell <= ", dry_spell_lower, " days"
      )

    } else {
      meta        <- .cal_criterion_meta[[as.character(criterion_internal)]]
      label_clean <- sub("^Criterion Nr \\((.+)\\)$", "\\1", meta$label)
      unit        <- if (onset == "thermal") "degC" else if (criterion == 4L) "%" else "mm"
      paste0(label_clean, " (here ", preset_value, " ", unit, ")")
    }
  }

  # ---- Write standard lines (1-9) via existing helper ----------------------
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
    eol                = eol
  )

  # ---- Append criterion-specific extra lines --------------------------------
  if (!is.null(criterion) && criterion %in% c(5L, 6L, 7L)) {
    eol_char <- .get_eol(eol)

    if (criterion == 5L) {
      extra <- paste0(
        .fmt_cal_line(rday,
                      "Minimum number of rainy days (> 0.1 mm) in the successive-days window"),
        eol_char,
        .fmt_cal_line(dspell,
                      "Maximum consecutive dry days allowed in the look-ahead window"),
        eol_char,
        .fmt_cal_line(lookahead_days,
                      "Length (days) of the look-ahead window for the dry spell condition"),
        eol_char
      )

    } else if (criterion == 6L) {
      extra <- paste0(
        .fmt_cal_line(min_weekly_rain,
                      "Minimum total rainfall (mm) in the rolling spell window (dry spell threshold)"),
        eol_char,
        .fmt_cal_line(spell_days,
                      "Length (days) of the rolling window for the dry spell check"),
        eol_char,
        .fmt_cal_line(lookahead_days,
                      "Length (days) of the look-ahead window for the dry spell condition"),
        eol_char
      )

    } else {   # criterion == 7L
      extra <- paste0(
        .fmt_cal_int(as.integer(cum_rain_upper)),  "Fuzzy: cumulative rainfall upper bound (mm) [cum_rain_upper]",  eol_char,
        .fmt_cal_int(as.integer(wet_days_lower)),  "Fuzzy: wet days lower bound [wet_days_lower]",                  eol_char,
        .fmt_cal_int(as.integer(wet_days_upper)),  "Fuzzy: wet days upper bound [wet_days_upper]",                  eol_char,
        .fmt_cal_int(as.integer(dry_spell_lower)), "Fuzzy: dry spell lower bound (days) [dry_spell_lower]",         eol_char,
        .fmt_cal_int(as.integer(dry_spell_upper)), "Fuzzy: dry spell upper bound (days) [dry_spell_upper]",         eol_char,
        .fmt_cal_float(fuzzy_threshold),           "Fuzzy: defuzzification threshold [fuzzy_threshold]",            eol_char
      )
    }

    content <- paste0(content, extra)
  }

  # ---- Save file ------------------------------------------------------------
  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  output_file <- paste0(path, site_name, ".CAL")
  readr::write_file(x = content, file = output_file)

  invisible(output_file)
}
