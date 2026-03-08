#' Format Date String for Comments
#'
#' @description
#' Internal helper function to format year and day-of-year into a readable date string.
#'
#' @param year Integer. Year value
#' @param doy Integer. Day of year (1-366)
#'
#' @return Character string with formatted date (e.g., "15 March 2024")
#' @keywords internal
#' @noRd
.format_date_string <- function(year, doy) {
  date <- as.Date(paste0(year, "-01-01")) + (doy - 1)
  day <- as.integer(format(date, "%d"))

  # Get English month name (handle locale)
  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  on.exit(Sys.setlocale("LC_TIME", old_locale))

  month <- format(date, "%B")

  paste0(day, " ", month, " ", year)
}

# ---------------------------------------------------------------------------
# Criterion metadata keyed by AquaCrop internal criterion number.
#
# User-facing criterion numbers:
#   onset = "rainfall": 1, 2, 3, 4, 5, 6, 7  → AquaCrop internal: 1, 2, 3, 4, 5, 6, 7
#   onset = "thermal" : 1, 2, 3, 4            → AquaCrop internal: 11, 12, 13, 14
#
# Conversion:
#   rainfall → pass-through (user == internal)
#   thermal  → internal = user + 10
#
# Criteria 5-7 are aquacroptools extensions (not AquaCrop standalone):
#   5 = Generalised Sivakumar (1988)
#   6 = Marteau et al. (2009)
#   7 = Fuzzy logic — Waongo et al. (2014)
# ---------------------------------------------------------------------------
.cal_criterion_meta <- list(
  # Rainfall criteria (AquaCrop internal: 1-4)
  "1" = list(
    label        = "Criterion Nr (Cumulative rainfall since start of time window is equal to or exceeds a preset value)",
    preset_label = "Preset value of Cumulative rainfall (mm)",
    preset_fmt   = "int",
    needs_succ   = FALSE
  ),
  "2" = list(
    label        = "Criterion Nr (Observed rainfall during a number of successive days is equal to or exceeds a preset value)",
    preset_label = "Preset value of Observed rainfall (mm)",
    preset_fmt   = "int",
    needs_succ   = TRUE
  ),
  "3" = list(
    label        = "Criterion Nr (10-day rainfall is equal to or exceeds a preset value)",
    preset_label = "Preset value of 10-day rainfall (mm)",
    preset_fmt   = "int",
    needs_succ   = FALSE
  ),
  "4" = list(
    label        = "Criterion Nr (10-day rainfall exceeds a preset fraction of the 10-day ETo)",
    preset_label = "Preset 10-day Rain percentage of 10-day ETo",
    preset_fmt   = "int",
    needs_succ   = FALSE
  ),
  # Generalised Sivakumar (aquacroptools extension, internal: 5)
  "5" = list(
    label        = "Criterion Nr (Generalised Sivakumar: cumulative rainfall x rainy days x dry spell check)",
    preset_label = "Preset value of cumulative rainfall (mm) [preset_value]",
    preset_fmt   = "int",
    needs_succ   = TRUE   # successive_days = accumulation window length
  ),
  # Marteau et al. (2009) (aquacroptools extension, internal: 6)
  "6" = list(
    label        = "Criterion Nr (Marteau 2009: wet-day trigger x rolling dry spell check in look-ahead)",
    preset_label = "Preset value of total rainfall over trigger window (mm) [preset_value]",
    preset_fmt   = "int",
    needs_succ   = TRUE   # successive_days = 1 or 2 (trigger window)
  ),
  # Fuzzy logic — Waongo et al. (2014) (aquacroptools extension, internal: 7)
  "7" = list(
    label        = "Criterion Nr (Fuzzy logic: cumulative rainfall x wet days x dry spell index)",
    preset_label = "Preset value of cumulative rainfall lower bound (mm) [cum_rain_lower]",
    preset_fmt   = "int",
    needs_succ   = TRUE   # successive_days = accum_days
  ),
  # Thermal criteria (AquaCrop internal: 11-14)
  "11" = list(
    label        = "Criterion Nr (Daily minimum air temperature in each day of a number of successive days is equal to or exceeds a preset value)",
    preset_label = "Preset value of Minimum air temperature (degC)",
    preset_fmt   = "float",
    needs_succ   = TRUE
  ),
  "12" = list(
    label        = "Criterion Nr (Daily average air temperature in each day of a number of successive days is equal to or exceeds a preset value)",
    preset_label = "Preset value of Average air temperature (degC)",
    preset_fmt   = "float",
    needs_succ   = TRUE
  ),
  "13" = list(
    label        = "Criterion Nr (Cumulative Growing-degrees in a given number of successive days is equal to or exceeds a preset value)",
    preset_label = "Preset value of Cumulative Growing-degrees (degC)",
    preset_fmt   = "float",
    needs_succ   = TRUE
  ),
  "14" = list(
    label        = "Criterion Nr (Cumulative Growing-degrees since start of the time window is equal to or exceed a preset value)",
    preset_label = "Preset value of Cumulative Growing-degrees (degC)",
    preset_fmt   = "float",
    needs_succ   = FALSE
  )
)


# ---------------------------------------------------------------------------
# Formatting helpers
# Value column right-justified, colon placed at a consistent column.
# Integers: width 10 + "    : " → colon at column 15
# Floats  : width 12 + "  : "   → colon at column 15
# ---------------------------------------------------------------------------
.fmt_cal_int <- function(x) {
  paste0(format(as.character(x), width = 10, justify = "right"), "    : ")
}

.fmt_cal_float <- function(x) {
  paste0(format(sprintf("%.1f", as.numeric(x)), width = 12, justify = "right"), "  : ")
}


#' Build Fixed Onset CAL Data Lines
#' @noRd
.build_cal_fixed <- function(fixed_day, sep) {
  paste0(
    .fmt_cal_int(-9),        "Day-number (1 ... 366) of the Start of the time window for the onset criterion: Not applicable", sep,
    .fmt_cal_int(-9),        "Length (days) of the time window for the onset criterion: Not applicable", sep,
    .fmt_cal_int(fixed_day), "Day-number (1 ... 366) for the onset of the growing period", sep,
    .fmt_cal_int(-9),        "preset value for generation of the onset: Not applicable", sep,
    .fmt_cal_int(-9),        "Number of successive days: Not applicable", sep,
    .fmt_cal_int(-9),        "Number of occurrences: Not applicable", sep
  )
}


#' Build Criterion-Based Onset CAL Data Lines (lines 4-9, plus fuzzy extras)
#'
#' Generates the six standard data lines (lines 4-9) for any criterion-based
#' onset type. For criterion 7 (fuzzy logic, Waongo et al. 2014), six
#' additional lines (10-15) are appended; these are ignored by AquaCrop
#' standalone but parsed by \code{\link{read_cal}}.
#'
#' Uses the AquaCrop internal criterion number (already converted from the
#' user-facing number before this function is called).
#'
#' @noRd
.build_cal_criterion <- function(window_start, window_length, criterion_internal,
                                 preset_value, successive_days,
                                 occurrences, sep,
                                 cum_rain_upper  = NULL,
                                 wet_days_lower  = NULL,
                                 wet_days_upper  = NULL,
                                 dry_spell_lower = NULL,
                                 dry_spell_upper = NULL,
                                 fuzzy_threshold = NULL) {
  meta <- .cal_criterion_meta[[as.character(criterion_internal)]]

  line_7 <- if (meta$preset_fmt == "int") {
    paste0(.fmt_cal_int(as.integer(preset_value)), meta$preset_label)
  } else {
    paste0(.fmt_cal_float(preset_value), meta$preset_label)
  }

  line_8 <- if (meta$needs_succ) {
    paste0(.fmt_cal_int(successive_days), "Number of successive days for the onset criterion")
  } else {
    paste0(.fmt_cal_int(-9), "Number of successive days for the onset criterion: Not applicable")
  }

  standard <- paste0(
    .fmt_cal_int(window_start),       "Day-number (1 ... 366) of the Start of the time window for the onset criterion", sep,
    .fmt_cal_int(window_length),      "Length (days) of the time window for the onset criterion", sep,
    .fmt_cal_int(criterion_internal), meta$label, sep,
    line_7, sep,
    line_8, sep,
    .fmt_cal_int(occurrences),        "Number of occurrences before the onset criterion applies (max = 3)", sep
  )

  # Criteria 5 (Sivakumar) and 6 (Marteau): extra lines appended by
  # write_cal() after the header — nothing extra to add here.
  if (criterion_internal != 7L) return(standard)

  # --- Criterion 7: fuzzy extras (lines 10-15, ignored by AquaCrop) --------
  fuzzy <- paste0(
    .fmt_cal_int(as.integer(cum_rain_upper)),  "Fuzzy: cumulative rainfall upper bound (mm) [cum_rain_upper]",  sep,
    .fmt_cal_int(as.integer(wet_days_lower)),  "Fuzzy: wet days lower bound [wet_days_lower]",                  sep,
    .fmt_cal_int(as.integer(wet_days_upper)),  "Fuzzy: wet days upper bound [wet_days_upper]",                  sep,
    .fmt_cal_int(as.integer(dry_spell_lower)), "Fuzzy: dry spell lower bound (days) [dry_spell_lower]",         sep,
    .fmt_cal_int(as.integer(dry_spell_upper)), "Fuzzy: dry spell upper bound (days) [dry_spell_upper]",         sep,
    .fmt_cal_float(fuzzy_threshold),           "Fuzzy: defuzzification threshold [fuzzy_threshold]",            sep
  )

  paste0(standard, fuzzy)
}
