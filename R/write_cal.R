#' Write AquaCrop Calendar (.CAL) File
#'
#' Creates an AquaCrop calendar file (.CAL) defining the onset of the growing
#' period. Three onset types are supported: a fixed calendar day, a
#' rainfall-based criterion, or a thermal criterion.
#'
#' User-facing criterion numbers run from 1 to 4 within each onset group.
#' They are converted internally to AquaCrop's numbering scheme (rainfall
#' passes through as-is; thermal criterion c becomes c + 10).
#'
#' @param cal_name Character. Name of the CAL file (without extension).
#'   Typically the station name.
#' @param onset Character. Onset type: "fixed", "rainfall", or "thermal".
#' @param fixed_day Integer. Calendar day (1-366) for the onset of the
#'   growing period. Required when onset is "fixed".
#' @param window_start Integer. Day-number (1-366) of the start of the time
#'   window. Required when onset is "rainfall" or "thermal".
#' @param window_length Integer. Length of the time window in days. Required
#'   when onset is "rainfall" or "thermal".
#' @param criterion Integer. Criterion number within the onset group (1-4).
#'   Required when onset is "rainfall" or "thermal".
#'   Rainfall criteria (onset = "rainfall"):
#'   \describe{
#'     \item{1}{Cumulative rainfall since start of window >= preset (mm).}
#'     \item{2}{Observed rainfall in N successive days >= preset (mm).
#'       Requires successive_days.}
#'     \item{3}{10-day rainfall >= preset (mm).}
#'     \item{4}{10-day rainfall >= preset fraction of 10-day ETo (percent).}
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
#'   Unit: mm for rainfall criteria 1-3, percent for criterion 4,
#'   degC for all thermal criteria.
#' @param successive_days Integer. Number of consecutive days. Required for
#'   rainfall criterion 2 and thermal criteria 1, 2, and 3.
#'   Ignored for all other criteria. Default: NULL.
#' @param occurrences Integer. Number of times the criterion must be met
#'   before onset is triggered (1-3). Default: 1.
#' @param path Output directory path for CAL files. Default: "CLIMATE/".
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
#'   cal_name  = "station_01",
#'   onset     = "fixed",
#'   fixed_day = 212
#' )
#'
#' # Rainfall criterion 1: cumulative rainfall >= 50 mm
#' write_cal(
#'   cal_name      = "station_01",
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
#'   cal_name        = "station_01",
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
#'   cal_name      = "station_01",
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
#'   cal_name      = "station_01",
#'   onset         = "rainfall",
#'   window_start  = 121,
#'   window_length = 92,
#'   criterion     = 4,
#'   preset_value  = 50,
#'   occurrences   = 1
#' )
#'
#' # Thermal criterion 1: daily Tmin >= 5 degC for 4 successive days
#' write_cal(
#'   cal_name        = "station_01",
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
#'   cal_name        = "station_01",
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
#'   cal_name        = "station_01",
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
#'   cal_name      = "station_01",
#'   onset         = "thermal",
#'   window_start  = 1,
#'   window_length = 60,
#'   criterion     = 4,
#'   preset_value  = 600.0,
#'   occurrences   = 1
#' )
#' }
#'
#' @family AquaCrop file writers
#' @export
write_cal <- function(
    cal_name,
    onset,
    fixed_day       = NULL,
    window_start    = NULL,
    window_length   = NULL,
    criterion       = NULL,
    preset_value    = NULL,
    successive_days = NULL,
    occurrences     = 1L,
    path            = "CAL/",
    description     = NULL,
    version         = 7.1,
    eol             = NULL
) {

  onset <- match.arg(tolower(onset), choices = c("fixed", "rainfall", "thermal"))

  if (onset == "fixed") {
    if (is.null(fixed_day)) {
      stop("fixed_day is required when onset is 'fixed'.", call. = FALSE)
    }
    if (fixed_day < 1L || fixed_day > 366L) {
      stop("fixed_day must be between 1 and 366.", call. = FALSE)
    }
    onset_code         <- 0L
    criterion_internal <- NULL

  } else {
    if (is.null(window_start) || is.null(window_length)) {
      stop("window_start and window_length are required for onset '",
           onset, "'.", call. = FALSE)
    }
    if (is.null(criterion) || !criterion %in% 1L:4L) {
      stop("criterion must be 1, 2, 3, or 4 for onset '", onset, "'.",
           call. = FALSE)
    }
    if (is.null(preset_value)) {
      stop("preset_value is required for onset '", onset, "'.", call. = FALSE)
    }
    if (occurrences < 1L || occurrences > 3L) {
      stop("occurrences must be between 1 and 3.", call. = FALSE)
    }

    # Convert user criterion to AquaCrop internal number
    criterion_internal <- if (onset == "thermal") criterion + 10L else criterion

    meta <- .cal_criterion_meta[[as.character(criterion_internal)]]

    if (meta$needs_succ && is.null(successive_days)) {
      stop(
        "successive_days is required for ", onset, " criterion ", criterion, ".",
        call. = FALSE
      )
    }
    if (!meta$needs_succ) successive_days <- -9L

    onset_code <- 1L
  }

  if (is.null(description)) {
    description <- if (onset == "fixed") {
      paste0("Onset is specified by a fixed calendar day (day ", fixed_day, ")")
    } else {
      # Extract the human-readable part of the criterion label:
      # strip "Criterion Nr (" prefix and ")" suffix, then append preset value
      meta        <- .cal_criterion_meta[[as.character(criterion_internal)]]
      label_clean <- sub("^Criterion Nr \\((.+)\\)$", "\\1", meta$label)
      unit        <- if (onset == "thermal") "degC" else if (criterion == 4) "%" else "mm"
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
    eol                = eol
  )

  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  output_file <- paste0(path, cal_name, ".CAL")

  readr::write_file(x = content, file = output_file)

  invisible(output_file)
}


