#' Parse an AquaCrop Calendar (.CAL) File
#'
#' Reads a .CAL file and returns a structured list of onset parameters.
#' Supports fixed onset, rainfall criteria 1-7, and thermal criteria 1-4.
#'
#' Criterion 5 (generalised Sivakumar) carries three extra fields:
#' \code{rday}, \code{dspell}, and \code{lookahead_days}.
#'
#' Criterion 6 (Marteau 2009) carries three extra fields:
#' \code{min_weekly_rain}, \code{spell_days}, and \code{lookahead_days}.
#'
#' Criterion 7 (fuzzy logic, Waongo et al. 2014) carries six extra fields:
#' \code{cum_rain_upper}, \code{wet_days_lower}, \code{wet_days_upper},
#' \code{dry_spell_lower}, \code{dry_spell_upper}, and \code{fuzzy_threshold}.
#'
#' All other criteria return \code{NA} for these fields.
#'
#' @param file Character. Full path to the .CAL file.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{onset}{Character. "fixed", "rainfall", or "thermal".}
#'     \item{fixed_day}{Integer or NA.}
#'     \item{window_start}{Integer or NA.}
#'     \item{window_length}{Integer or NA.}
#'     \item{criterion_internal}{Integer or NA.}
#'     \item{criterion}{Integer or NA. User-facing number (1-7 rainfall,
#'       1-4 thermal).}
#'     \item{preset_value}{Numeric or NA.}
#'     \item{successive_days}{Integer or NA.}
#'     \item{occurrences}{Integer or NA.}
#'     \item{rday}{Integer or NA. Criterion 5 only.}
#'     \item{dspell}{Integer or NA. Criterion 5 only.}
#'     \item{min_weekly_rain}{Numeric or NA. Criterion 6 only.}
#'     \item{spell_days}{Integer or NA. Criterion 6 only.}
#'     \item{lookahead_days}{Integer or NA. Criteria 5 and 6 only.}
#'     \item{cum_rain_upper}{Numeric or NA. Criterion 7 only.}
#'     \item{wet_days_lower}{Integer or NA. Criterion 7 only.}
#'     \item{wet_days_upper}{Integer or NA. Criterion 7 only.}
#'     \item{dry_spell_lower}{Integer or NA. Criterion 7 only.}
#'     \item{dry_spell_upper}{Integer or NA. Criterion 7 only.}
#'     \item{fuzzy_threshold}{Numeric or NA. Criterion 7 only.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Standard rainfall criterion
#' cal <- read_cal("CAL/station_01.CAL")
#' cal$onset        # "rainfall"
#' cal$criterion    # 2
#' cal$preset_value # 30
#'
#' # Sivakumar criterion
#' cal5 <- read_cal("CAL/station_siv.CAL")
#' cal5$criterion      # 5
#' cal5$rday           # 1
#' cal5$dspell         # 7
#' cal5$lookahead_days # 30
#'
#' # Marteau criterion
#' cal6 <- read_cal("CAL/station_mrt.CAL")
#' cal6$criterion       # 6
#' cal6$min_weekly_rain # 5
#' cal6$spell_days      # 7
#' cal6$lookahead_days  # 20
#'
#' # Fuzzy logic criterion
#' cal7 <- read_cal("CAL/station_fuz.CAL")
#' cal7$criterion      # 7
#' cal7$cum_rain_upper # 40
#' cal7$fuzzy_threshold # 0.5
#' }
#' @family AquaCrop readers
#'
#' @export
read_cal <- function(file) {

  if (!fs::file_exists(file)) {
    stop("CAL file not found: ", file, call. = FALSE)
  }

  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  .parse_value <- function(line) {
    suppressWarnings(as.numeric(trimws(strsplit(line, ":")[[1]][1])))
  }

  onset_code <- as.integer(.parse_value(lines[3]))

  # ---- Shared NA defaults for all extended fields ---------------------------
  rday            <- NA_integer_
  dspell          <- NA_integer_
  min_weekly_rain <- NA_real_
  spell_days      <- NA_integer_
  lookahead_days  <- NA_integer_
  cum_rain_upper  <- NA_real_
  wet_days_lower  <- NA_integer_
  wet_days_upper  <- NA_integer_
  dry_spell_lower <- NA_integer_
  dry_spell_upper <- NA_integer_
  fuzzy_threshold <- NA_real_

  # ---- Fixed onset ----------------------------------------------------------
  if (onset_code == 0L) {
    return(list(
      onset              = "fixed",
      fixed_day          = as.integer(.parse_value(lines[6])),
      window_start       = NA_integer_,
      window_length      = NA_integer_,
      criterion_internal = NA_integer_,
      criterion          = NA_integer_,
      preset_value       = NA_real_,
      successive_days    = NA_integer_,
      occurrences        = NA_integer_,
      rday               = rday,
      dspell             = dspell,
      min_weekly_rain    = min_weekly_rain,
      spell_days         = spell_days,
      lookahead_days     = lookahead_days,
      cum_rain_upper     = cum_rain_upper,
      wet_days_lower     = wet_days_lower,
      wet_days_upper     = wet_days_upper,
      dry_spell_lower    = dry_spell_lower,
      dry_spell_upper    = dry_spell_upper,
      fuzzy_threshold    = fuzzy_threshold
    ))
  }

  # ---- Rainfall / thermal onset ---------------------------------------------
  window_start       <- as.integer(.parse_value(lines[4]))
  window_length      <- as.integer(.parse_value(lines[5]))
  criterion_internal <- as.integer(.parse_value(lines[6]))
  preset_value       <- as.numeric(.parse_value(lines[7]))
  successive_days    <- as.integer(.parse_value(lines[8]))
  occurrences        <- as.integer(.parse_value(lines[9]))

  if (!is.na(successive_days) && successive_days == -9L) {
    successive_days <- NA_integer_
  }

  # ---- Resolve onset type and user-facing criterion -------------------------
  if (criterion_internal %in% c(5L, 6L, 7L)) {
    onset     <- "rainfall"
    criterion <- criterion_internal

  } else if (criterion_internal %in% 1L:4L) {
    onset     <- "rainfall"
    criterion <- criterion_internal

  } else if (criterion_internal %in% 11L:14L) {
    onset     <- "thermal"
    criterion <- criterion_internal - 10L

  } else {
    stop("Unknown criterion number: ", criterion_internal, call. = FALSE)
  }

  # ---- Criterion 5: generalised Sivakumar (lines 10-12) --------------------
  if (criterion_internal == 5L) {
    if (length(lines) < 12L) {
      stop(
        "CAL file declares Sivakumar criterion (5) but is missing lines ",
        "10-12 (rday, dspell, lookahead_days).",
        call. = FALSE
      )
    }
    rday           <- as.integer(.parse_value(lines[10]))
    dspell         <- as.integer(.parse_value(lines[11]))
    lookahead_days <- as.integer(.parse_value(lines[12]))
  }

  # ---- Criterion 6: Marteau (lines 10-12) ----------------------------------
  if (criterion_internal == 6L) {
    if (length(lines) < 12L) {
      stop(
        "CAL file declares Marteau criterion (6) but is missing lines ",
        "10-12 (min_weekly_rain, spell_days, lookahead_days).",
        call. = FALSE
      )
    }
    min_weekly_rain <- as.numeric(.parse_value(lines[10]))
    spell_days      <- as.integer(.parse_value(lines[11]))
    lookahead_days  <- as.integer(.parse_value(lines[12]))
  }

  # ---- Criterion 7: fuzzy logic (lines 10-15) ------------------------------
  if (criterion_internal == 7L) {
    if (length(lines) < 15L) {
      stop(
        "CAL file declares fuzzy criterion (7) but is missing extra ",
        "parameter lines (expected at least 15 non-empty lines).",
        call. = FALSE
      )
    }
    cum_rain_upper  <- as.numeric(.parse_value(lines[10]))
    wet_days_lower  <- as.integer(.parse_value(lines[11]))
    wet_days_upper  <- as.integer(.parse_value(lines[12]))
    dry_spell_lower <- as.integer(.parse_value(lines[13]))
    dry_spell_upper <- as.integer(.parse_value(lines[14]))
    fuzzy_threshold <- as.numeric(.parse_value(lines[15]))
  }

  list(
    onset              = onset,
    fixed_day          = NA_integer_,
    window_start       = window_start,
    window_length      = window_length,
    criterion_internal = criterion_internal,
    criterion          = criterion,
    preset_value       = preset_value,
    successive_days    = successive_days,
    occurrences        = occurrences,
    rday               = rday,
    dspell             = dspell,
    min_weekly_rain    = min_weekly_rain,
    spell_days         = spell_days,
    lookahead_days     = lookahead_days,
    cum_rain_upper     = cum_rain_upper,
    wet_days_lower     = wet_days_lower,
    wet_days_upper     = wet_days_upper,
    dry_spell_lower    = dry_spell_lower,
    dry_spell_upper    = dry_spell_upper,
    fuzzy_threshold    = fuzzy_threshold
  )
}
