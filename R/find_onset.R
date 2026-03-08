#' Maximum Consecutive Dry Days in a Look-ahead Window
#'
#' Computes the longest run of consecutive dry days (rainfall <= 0.1 mm)
#' within a given absolute DOY range. Used by the Sivakumar engine.
#'
#' @param rain_doy Numeric vector. Daily rainfall indexed by DOY.
#' @param start    Integer. First DOY of the look-ahead window.
#' @param end      Integer. Last DOY of the look-ahead window.
#'
#' @return Integer. Maximum consecutive dry days, or 0L if none.
#' @noRd
.max_dry_spell_window <- function(rain_doy, start, end) {
  end <- min(end, length(rain_doy))
  if (start > end || start < 1L) return(0L)
  x <- rain_doy[start:end] <= 0.1
  if (!any(x, na.rm = TRUE)) return(0L)
  r <- rle(x)
  max(r$lengths[r$values], na.rm = TRUE)
}


#' Minimum Rolling-Window Rainfall Total in a Look-ahead Window
#'
#' Computes the minimum total rainfall over any rolling \code{spell_days}-day
#' window within a given absolute DOY range. Used by the Marteau engine to
#' detect whether any sub-window falls below the dry-spell threshold.
#'
#' @param rain_doy   Numeric vector. Daily rainfall indexed by DOY.
#' @param start      Integer. First DOY of the look-ahead window.
#' @param end        Integer. Last DOY of the look-ahead window.
#' @param spell_days Integer. Length of the rolling window (days).
#'
#' @return Numeric. Minimum rolling total, or Inf if the window is too short
#'   to fit even one rolling sub-window (criterion is then trivially satisfied).
#' @noRd
.min_rolling_rain_window <- function(rain_doy, start, end, spell_days) {
  end <- min(end, length(rain_doy))
  if (start > end || start < 1L) return(Inf)
  x <- rain_doy[start:end]
  n <- length(x)
  if (n < spell_days) return(Inf)   # window too short — criterion trivially met
  # cumsum trick: rolling sum of length spell_days
  cs  <- c(0, cumsum(ifelse(is.na(x), 0, x)))
  min(cs[(spell_days + 1L):(n + 1L)] - cs[1L:(n - spell_days + 1L)])
}


#' Apply Rainfall Onset Criterion to One Year (AquaCrop criteria 1-4)
#'
#' @param rain_doy Numeric vector (length 366). Daily rainfall indexed by DOY.
#' @param eto_doy  Numeric vector (length 366) or NULL. Daily ETo indexed by
#'   DOY. Required only for criterion 4.
#' @param window_start Integer. First DOY of the search window.
#' @param window_length Integer. Length of the window in days.
#' @param criterion Integer. AquaCrop internal criterion number (1-4).
#' @param preset_value Numeric. Threshold value.
#' @param successive_days Integer or NA. Required for criterion 2.
#' @param occurrences Integer. Number of occurrences required.
#'
#' @return Integer. Onset DOY, or NA_integer_ if criterion not met.
#' @noRd
.onset_rainfall_one_year <- function(rain_doy, eto_doy, window_start,
                                     window_length, criterion, preset_value,
                                     successive_days, occurrences) {

  window_end <- window_start + window_length - 1L
  idx        <- seq(window_start, window_end)
  n          <- length(idx)

  upper_r <- length(rain_doy)
  r <- ifelse(idx >= 1L & idx <= upper_r, rain_doy[idx], 0)

  found <- 0L

  if (criterion == 1L) {
    cs <- cumsum(r)
    for (i in seq_along(cs)) {
      if (cs[i] >= preset_value) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i])
        r[seq_len(i)] <- 0
        cs <- cumsum(r)
      }
    }

  } else if (criterion == 2L) {
    sdays <- as.integer(successive_days)
    cs    <- c(0, cumsum(r))
    i     <- 1L
    while (i <= n - sdays + 1L) {
      if (cs[i + sdays] - cs[i] >= preset_value) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + sdays - 1L])
        i <- i + sdays
      } else {
        i <- i + 1L
      }
    }

  } else if (criterion == 3L) {
    cs <- c(0, cumsum(r))
    i  <- 1L
    while (i <= n - 9L) {
      if (cs[i + 10L] - cs[i] >= preset_value) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + 9L])
        i <- i + 10L
      } else {
        i <- i + 1L
      }
    }

  } else if (criterion == 4L) {
    upper_e <- length(eto_doy)
    e  <- ifelse(idx >= 1L & idx <= upper_e, eto_doy[idx], 0)
    cr <- c(0, cumsum(r))
    ce <- c(0, cumsum(e))
    i  <- 1L
    while (i <= n - 9L) {
      rain_10 <- cr[i + 10L] - cr[i]
      eto_10  <- ce[i + 10L] - ce[i]
      if (eto_10 > 0 && (rain_10 / eto_10 * 100) >= preset_value) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + 9L])
        i <- i + 10L
      } else {
        i <- i + 1L
      }
    }
  }

  NA_integer_
}


#' Apply Generalised Sivakumar Onset Criterion to One Year (criterion 5)
#'
#' Vectorised implementation. Conditions for candidate window starting at
#' DOY j:
#' \enumerate{
#'   \item Cumulative rainfall over \code{successive_days} days >=
#'     \code{preset_value} mm.
#'   \item Number of rainy days (> 0.1 mm) in that window >= \code{rday}.
#'   \item Longest consecutive dry spell in \code{[j+1, j+lookahead_days]} <
#'     \code{dspell} days.
#' }
#' Rolling sums are computed once via \code{cumsum}; dry spell detection uses
#' \code{rle}. Only candidates passing conditions 1-2 pay the cost of
#' condition 3.
#'
#' @param rain_doy       Numeric vector. Daily rainfall indexed by DOY.
#' @param window_start   Integer. First DOY of the search window.
#' @param window_length  Integer. Length of the search window in days.
#' @param preset_value   Numeric. Minimum cumulative rainfall (mm).
#' @param successive_days Integer. Rainfall accumulation window length (nday).
#' @param occurrences    Integer. Times criterion must be met before triggering.
#' @param rday           Integer. Minimum rainy days in the window.
#' @param dspell         Integer. Max consecutive dry days allowed (strictly <).
#' @param lookahead_days Integer. Length of the look-ahead window (days).
#'
#' @return Integer. Onset DOY (trigger DOY + 1), or NA_integer_.
#' @noRd
.onset_sivakumar_one_year <- function(rain_doy, window_start, window_length,
                                      preset_value, successive_days,
                                      occurrences, rday, dspell,
                                      lookahead_days) {

  window_end <- window_start + window_length - 1L
  idx        <- seq(window_start, window_end)
  n          <- length(idx)
  upper_r    <- length(rain_doy)
  sdays      <- as.integer(successive_days)

  r <- ifelse(idx >= 1L & idx <= upper_r, rain_doy[idx], 0)

  cs_rain <- c(0, cumsum(r))
  cs_rday <- c(0, cumsum(r > 0.1))

  found <- 0L
  i     <- 1L

  while (i <= n - sdays + 1L) {

    if (cs_rain[i + sdays] - cs_rain[i] >= preset_value) {
      if (cs_rday[i + sdays] - cs_rday[i] >= rday) {

        la_start <- idx[i] + 1L
        la_end   <- idx[i] + lookahead_days

        if (.max_dry_spell_window(rain_doy, la_start, la_end) < dspell) {
          found <- found + 1L
          if (found >= occurrences) return(idx[i] + 1L)
          i <- i + sdays
        } else {
          i <- i + 1L
        }

      } else {
        i <- i + 1L
      }
    } else {
      i <- i + 1L
    }
  }

  NA_integer_
}


#' Apply Marteau (2009) Onset Criterion to One Year (criterion 6)
#'
#' Vectorised implementation. The onset is the first day > 1 mm that is part
#' of \code{successive_days} (1 or 2) consecutive days receiving >=
#' \code{preset_value} mm in total, provided that no rolling
#' \code{spell_days}-day window in the following \code{lookahead_days} days
#' has a total rainfall below \code{min_weekly_rain} mm.
#'
#' Rolling totals for the trigger and the look-ahead are both computed via
#' \code{cumsum}, making conditions 1-2 O(1) per candidate. Condition 3
#' (look-ahead) uses a second \code{cumsum} over the look-ahead slice and is
#' only evaluated for candidates passing conditions 1-2.
#'
#' @param rain_doy       Numeric vector. Daily rainfall indexed by DOY.
#' @param window_start   Integer. First DOY of the search window.
#' @param window_length  Integer. Length of the search window in days.
#' @param preset_value   Numeric. Minimum total rainfall (mm) over
#'   \code{successive_days} days (trigger threshold).
#' @param successive_days Integer. 1 or 2. Trigger window length.
#' @param occurrences    Integer. Times criterion must be met before triggering.
#' @param min_weekly_rain Numeric. Minimum acceptable rolling total (mm) over
#'   \code{spell_days} days in the look-ahead (dry spell threshold).
#' @param spell_days     Integer. Rolling window length (days) for the
#'   look-ahead dry spell check.
#' @param lookahead_days Integer. Length of the look-ahead window (days).
#'
#' @return Integer. Onset DOY (first wet day of the trigger window), or
#'   NA_integer_ if criterion not met.
#' @noRd
.onset_marteau_one_year <- function(rain_doy, window_start, window_length,
                                    preset_value, successive_days,
                                    occurrences, min_weekly_rain,
                                    spell_days, lookahead_days) {

  window_end <- window_start + window_length - 1L
  idx        <- seq(window_start, window_end)
  n          <- length(idx)
  upper_r    <- length(rain_doy)
  sdays      <- as.integer(successive_days)   # 1 or 2

  r <- ifelse(idx >= 1L & idx <= upper_r, rain_doy[idx], 0)

  # Rolling trigger sums computed once
  cs_rain <- c(0, cumsum(r))

  found <- 0L
  i     <- 1L

  while (i <= n - sdays + 1L) {

    # Condition 1: total rainfall in sdays-day window >= preset_value
    if (cs_rain[i + sdays] - cs_rain[i] >= preset_value) {

      # Condition 2: first day of window must be > 1 mm (wet day trigger)
      if (r[i] > 1) {

        # Condition 3: no spell_days-day rolling window in look-ahead below
        # min_weekly_rain
        la_start <- idx[i] + sdays          # look-ahead starts after trigger
        la_end   <- idx[i] + lookahead_days

        if (.min_rolling_rain_window(rain_doy, la_start, la_end, spell_days) >=
            min_weekly_rain) {
          found <- found + 1L
          # Onset = first day of the trigger window (Marteau convention)
          if (found >= occurrences) return(idx[i])
          i <- i + sdays
        } else {
          i <- i + 1L
        }

      } else {
        i <- i + 1L
      }

    } else {
      i <- i + 1L
    }
  }

  NA_integer_
}


#' First Failing Position in a Successive-Day Window
#'
#' @noRd
.first_fail <- function(vals, threshold, i, sdays) {
  win  <- vals[i:(i + sdays - 1L)]
  fail <- which(is.na(win) | win < threshold)
  if (length(fail) == 0L) 0L else fail[1L]
}


#' Apply Thermal Onset Criterion to One Year (criteria 11-14)
#'
#' @param tmin_doy Numeric vector. Daily Tmin indexed by DOY.
#' @param tmax_doy Numeric vector. Daily Tmax indexed by DOY.
#' @param window_start Integer. First DOY of the search window.
#' @param window_length Integer. Length of the window in days.
#' @param criterion Integer. AquaCrop internal criterion number (11-14).
#' @param preset_value Numeric. Threshold value (degC or degree-days).
#' @param successive_days Integer or NA. Required for criteria 11, 12, 13.
#' @param occurrences Integer. Number of occurrences required.
#' @param base_temp Numeric. Base temperature for GDD (criteria 13-14).
#'   Default: 10.
#'
#' @return Integer. Onset DOY, or NA_integer_ if criterion not met.
#' @noRd
.onset_temperature_one_year <- function(tmin_doy, tmax_doy, window_start,
                                        window_length, criterion,
                                        preset_value, successive_days,
                                        occurrences, base_temp = 10) {

  window_end <- window_start + window_length - 1L
  idx        <- seq(window_start, window_end)
  n          <- length(idx)

  .safe <- function(v, d) if (d < 1L || d > length(v)) NA_real_ else v[d]

  tmin  <- vapply(idx, function(d) .safe(tmin_doy, d), numeric(1))
  tmax  <- vapply(idx, function(d) .safe(tmax_doy, d), numeric(1))
  tmean <- (tmin + tmax) / 2
  gdd   <- pmax(tmean - base_temp, 0)

  found <- 0L

  if (criterion == 11L) {
    sdays <- as.integer(successive_days)
    i     <- 1L
    while (i <= n - sdays + 1L) {
      ff <- .first_fail(tmin, preset_value, i, sdays)
      if (ff == 0L) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + sdays - 1L])
        i <- i + sdays
      } else {
        i <- i + ff
      }
    }

  } else if (criterion == 12L) {
    sdays <- as.integer(successive_days)
    i     <- 1L
    while (i <= n - sdays + 1L) {
      ff <- .first_fail(tmean, preset_value, i, sdays)
      if (ff == 0L) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + sdays - 1L])
        i <- i + sdays
      } else {
        i <- i + ff
      }
    }

  } else if (criterion == 13L) {
    sdays <- as.integer(successive_days)
    i     <- 1L
    while (i <= n - sdays + 1L) {
      if (sum(gdd[i:(i + sdays - 1L)], na.rm = TRUE) >= preset_value) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + sdays - 1L])
        i <- i + sdays
      } else {
        i <- i + 1L
      }
    }

  } else if (criterion == 14L) {
    cs <- cumsum(ifelse(is.na(gdd), 0, gdd))
    for (i in seq_along(cs)) {
      if (cs[i] >= preset_value) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i])
        gdd[seq_len(i)] <- 0
        cs <- cumsum(gdd)
      }
    }
  }

  NA_integer_
}


#' Compute AquaCrop Growing Season Onset Day per Year
#'
#' Reads a CAL file, determines the onset method, and computes the onset
#' day-of-year (DOY) for each year in the record. Supports:
#' \itemize{
#'   \item Fixed onset.
#'   \item AquaCrop rainfall criteria 1-4.
#'   \item Criterion 5: generalised Sivakumar (1988).
#'   \item Criterion 6: Marteau (2009).
#'   \item Thermal criteria 1-4.
#' }
#'
#' @param site_name Character. Station name used to locate both the CAL file
#'   and the climate files (.PLU, .Tnx, .ETo).
#' @param cal_path Character. Path to the CAL directory. Default: "CAL/".
#' @param climate_path Character. Path to the climate directory.
#'   Default: "CLIMATE/".
#' @param years Integer vector or NULL. Years to compute onset for. If NULL
#'   (default), all years present in the climate file are used.
#' @param base_path Character. Base absolute path. Default: current working
#'   directory.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{year}{Integer.}
#'     \item{onset_doy}{Integer. Onset DOY, or last day of the search window
#'       if criterion not met.}
#'     \item{onset_date}{Date.}
#'   }
#'
#' @examples
#' \dontrun{
#' # AquaCrop rainfall criterion 2
#' find_onset("site_01")
#'
#' # Generalised Sivakumar (parameters in CAL file)
#' find_onset("site_siv")
#'
#' # Marteau 2009 (parameters in CAL file)
#' find_onset("site_mrt")
#' }
#'
#' @seealso \code{\link{read_cal}}, \code{\link{read_plu}},
#'   \code{\link{read_eto}}, \code{\link{read_tnx}}
#' @export
find_onset <- function(
    site_name,
    cal_path     = "CAL/",
    climate_path = "CLIMATE/",
    years        = NULL,
    base_path    = getwd()
) {

  cal <- read_cal(fs::path(base_path, cal_path, paste0(site_name, ".CAL")))

  # Fixed onset
  if (cal$onset == "fixed") {
    if (is.null(years)) {
      plu_years <- read_plu(
        fs::path(base_path, climate_path, paste0(site_name, ".PLU"))
      ) |>
        dplyr::pull(year) |>
        unique() |>
        sort()
      years <- as.integer(plu_years)
    } else {
      years <- as.integer(years)
    }
    return(data.frame(
      year       = years,
      onset_doy  = as.integer(rep(cal$fixed_day, length(years))),
      onset_date = as.Date(paste(years, cal$fixed_day), format = "%Y %j"),
      stringsAsFactors = FALSE
    ))
  }

  # Read climate data
  
  if (cal$onset == "rainfall") {
    clim_data <- read_plu(
      fs::path(base_path, climate_path, paste0(site_name, ".PLU"))
    ) |>
      dplyr::mutate(
        doy = lubridate::yday(lubridate::make_date(year, month, day))
      )
  } else {
    clim_data <- read_tnx(
      fs::path(base_path, climate_path, paste0(site_name, ".Tnx"))
    ) |>
      dplyr::mutate(
        doy = lubridate::yday(lubridate::make_date(year, month, day))
      )
  }

  if (is.null(years)) {
    years <- sort(unique(clim_data$year))
  } else {
    years <- as.integer(years)
    miss  <- setdiff(years, unique(clim_data$year))
    if (length(miss) > 0) {
      warning(
        "Years not found in climate file and skipped: ",
        paste(miss, collapse = ", "),
        call. = FALSE
      )
      years <- intersect(years, unique(clim_data$year))
    }
  }

  # ETo needed only for criterion 4
  eto_data <- NULL
  if (cal$criterion_internal == 4L) {
    eto_data <- read_eto(
      fs::path(base_path, climate_path, paste0(site_name, ".ETo"))
    ) |>
      dplyr::mutate(
        doy = lubridate::yday(lubridate::make_date(year, month, day))
      )
  }

  # Per-year computation 
  results <- lapply(years, function(yr) {

    yr_data <- clim_data[clim_data$year == yr, ]

    if (cal$onset == "rainfall") {

      rain_doy <- rep(0, max(yr_data$doy))
      rain_doy[yr_data$doy] <- yr_data$rain

      onset_doy <- switch(
        as.character(cal$criterion_internal),

        # Generalised Sivakumar, Widely used versions in Sahel can be used.(PRESASS)
        "5" = .onset_sivakumar_one_year(
          rain_doy        = rain_doy,
          window_start    = cal$window_start,
          window_length   = cal$window_length,
          preset_value    = cal$preset_value,
          successive_days = cal$successive_days,
          occurrences     = cal$occurrences,
          rday            = cal$rday,
          dspell          = cal$dspell,
          lookahead_days  = cal$lookahead_days
        ),

        # Marteau (2009)
        "6" = .onset_marteau_one_year(
          rain_doy        = rain_doy,
          window_start    = cal$window_start,
          window_length   = cal$window_length,
          preset_value    = cal$preset_value,
          successive_days = cal$successive_days,
          occurrences     = cal$occurrences,
          min_weekly_rain = cal$min_weekly_rain,
          spell_days      = cal$spell_days,
          lookahead_days  = cal$lookahead_days
        ),

        # Standard AquaCrop rainfall criteria
        {
          eto_doy <- NULL
          if (!is.null(eto_data)) {
            eto_yr  <- eto_data[eto_data$year == yr, ]
            eto_doy <- rep(0, max(eto_yr$doy))
            eto_doy[eto_yr$doy] <- eto_yr$eto
          }
          .onset_rainfall_one_year(
            rain_doy        = rain_doy,
            eto_doy         = eto_doy,
            window_start    = cal$window_start,
            window_length   = cal$window_length,
            criterion       = cal$criterion_internal,
            preset_value    = cal$preset_value,
            successive_days = cal$successive_days,
            occurrences     = cal$occurrences
          )
        }
      )

    } else {
      
      # Standard AquaCrop thermal criteria
      
      tmin_doy <- rep(NA_real_, max(yr_data$doy))
      tmax_doy <- rep(NA_real_, max(yr_data$doy))
      tmin_doy[yr_data$doy] <- yr_data$tmin
      tmax_doy[yr_data$doy] <- yr_data$tmax

      onset_doy <- .onset_temperature_one_year(
        tmin_doy        = tmin_doy,
        tmax_doy        = tmax_doy,
        window_start    = cal$window_start,
        window_length   = cal$window_length,
        criterion       = cal$criterion_internal,
        preset_value    = cal$preset_value,
        successive_days = cal$successive_days,
        occurrences     = cal$occurrences
      )
    }

    # Fallback: last day of window when criterion not met
    if (is.na(onset_doy)) onset_doy <- cal$window_start + cal$window_length - 1L

    data.frame(
      year       = yr,
      onset_doy  = as.integer(onset_doy),
      onset_date = as.Date(paste(yr, onset_doy), format = "%Y %j"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, results)
}
