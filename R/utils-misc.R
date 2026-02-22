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
#   onset = "rainfall": 1, 2, 3, 4  → AquaCrop internal: 1, 2, 3, 4
#   onset = "thermal" : 1, 2, 3, 4  → AquaCrop internal: 11, 12, 13, 14
#
# Conversion:
#   rainfall → pass-through (user == internal)
#   thermal  → internal = user + 10
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


#' Build Criterion-Based Onset CAL Data Lines
#'
#' Uses the AquaCrop internal criterion number (already converted from
#' user-facing number before this function is called).
#'
#' @noRd
.build_cal_criterion <- function(window_start, window_length, criterion_internal,
                                 preset_value, successive_days,
                                 occurrences, sep) {
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

  paste0(
    .fmt_cal_int(window_start),         "Day-number (1 ... 366) of the Start of the time window for the onset criterion", sep,
    .fmt_cal_int(window_length),        "Length (days) of the time window for the onset criterion", sep,
    .fmt_cal_int(criterion_internal),   meta$label, sep,
    line_7, sep,
    line_8, sep,
    .fmt_cal_int(occurrences),          "Number of occurrences before the onset criterion applies (max = 3)", sep
  )
}

