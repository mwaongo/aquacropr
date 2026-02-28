#' Apply Rainfall Onset Criterion to One Year
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
  
  # Extract window values once, padding out-of-range DOYs with 0
  upper_r <- length(rain_doy)
  r <- ifelse(idx >= 1L & idx <= upper_r, rain_doy[idx], 0)
  
  found <- 0L
  
  if (criterion == 1L) {
    # Cumulative rainfall since window start >= preset (reset after each hit)
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
    # Rainfall in N successive days >= preset
    # Pre-compute rolling sums via cumsum: sum[i..i+k-1] = cs[i+k-1] - cs[i-1]
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
    # 10-day rainfall >= preset
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
    # 10-day rainfall >= preset % of 10-day ETo
    # Pre-compute both rolling sums via cumsum
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


#' First Failing Position in a Successive-Day Window
#'
#' Returns the offset (1-based within the window) of the first element that
#' fails the threshold condition, or 0L if all pass. Used to skip redundant
#' window positions in criteria 11 and 12.
#'
#' @noRd
.first_fail <- function(vals, threshold, i, sdays) {
  win  <- vals[i:(i + sdays - 1L)]
  fail <- which(is.na(win) | win < threshold)
  if (length(fail) == 0L) 0L else fail[1L]
}


#' Apply Thermal Onset Criterion to One Year
#'
#' @param tmin_doy Numeric vector. Daily Tmin indexed by DOY.
#' @param tmax_doy Numeric vector. Daily Tmax indexed by DOY.
#' @param window_start Integer. First DOY of the search window.
#' @param window_length Integer. Length of the window in days.
#' @param criterion Integer. AquaCrop internal criterion number (11-14).
#' @param preset_value Numeric. Threshold value (degC or degree-days).
#' @param successive_days Integer or NA. Required for criteria 11, 12, 13.
#' @param occurrences Integer. Number of occurrences required.
#' @param base_temp Numeric. Base temperature for GDD computation (criteria
#'   13 and 14). Default: 10.
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
    # Daily Tmin >= preset for N successive days
    sdays <- as.integer(successive_days)
    i     <- 1L
    while (i <= n - sdays + 1L) {
      ff <- .first_fail(tmin, preset_value, i, sdays)
      if (ff == 0L) {
        found <- found + 1L
        if (found >= occurrences) return(idx[i + sdays - 1L])
        i <- i + sdays
      } else {
        i <- i + ff   # jump past the failing day
      }
    }
    
  } else if (criterion == 12L) {
    # Daily Tmean >= preset for N successive days
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
    # Cumulative GDD in N successive days >= preset
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
    # Cumulative GDD since start of window >= preset
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
#' day-of-year (DOY) for each year in the record. Supports fixed onset and
#' all four rainfall criteria. Criterion 4 (rainfall as percent of ETo)
#' automatically reads the ETo file via read_eto(). Thermal onset is not
#' yet supported.
#'
#' @param site_name Character. Station name used to locate both the CAL file
#'   and the climate files (.PLU, .Tnx, .ETo).
#' @param cal_path Path to the CAL directory. Default: "CAL/".
#' @param climate_path Path to the climate directory. Default: "CLIMATE/".
#' @param years Integer vector or NULL. Years to compute onset for. If NULL
#'   (default), all years present in the PLU file are used. Required when
#'   onset is "fixed".
#' @param base_path Base absolute path. Default: current working directory.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{year}{Integer. Year.}
#'     \item{onset_doy}{Integer. Onset day-of-year, or NA if the criterion
#'       was not met within the time window.}
#'     \item{onset_date}{Date. Calendar date of onset (NA when onset_doy
#'       is NA).}
#'   }
#'
#' @examples
#' \dontrun{
#' # Criterion 2: 30 mm in 3 successive days
#' onset <- find_onset("site_01")
#' head(onset)
#'
#' # Fixed onset: same DOY every year
#' onset_fixed <- find_onset("site_01_fixed", years = 1981:2020)
#'
#' # Criterion 4: 10-day rain >= 50 % of ETo (ETo read automatically)
#' onset_c4 <- find_onset("site_01_c4")
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
  
  # ---- Fixed onset ----
  if (cal$onset == "fixed") {
    if (is.null(years)) {
      plu_years <- read_plu(fs::path(base_path, climate_path, paste0(site_name, ".PLU"))) |>
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
  
  # ---- Read climate data and resolve years ----
  if (cal$onset == "rainfall") {
    clim_data <- read_plu(fs::path(base_path, climate_path, paste0(site_name, ".PLU"))) |>
      dplyr::mutate(doy = lubridate::yday(lubridate::make_date(year, month, day)))
  } else {
    clim_data <- read_tnx(fs::path(base_path, climate_path, paste0(site_name, ".Tnx"))) |>
      dplyr::mutate(doy = lubridate::yday(lubridate::make_date(year, month, day)))
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
  
  # Read ETo only if rainfall criterion 4 needs it
  eto_data <- NULL
  if (cal$criterion_internal == 4L) {
    eto_data <- read_eto(fs::path(base_path, climate_path, paste0(site_name, ".ETo"))) |>
      dplyr::mutate(doy = lubridate::yday(lubridate::make_date(year, month, day)))
  }
  
  # ---- Compute per year ----
  results <- lapply(years, function(yr) {
    
    yr_data <- clim_data[clim_data$year == yr, ]
    
    if (cal$onset == "rainfall") {
      
      rain_doy <- rep(0, max(yr_data$doy))
      rain_doy[yr_data$doy] <- yr_data$rain
      
      eto_doy <- NULL
      if (!is.null(eto_data)) {
        eto_yr  <- eto_data[eto_data$year == yr, ]
        eto_doy <- rep(0, max(eto_yr$doy))
        eto_doy[eto_yr$doy] <- eto_yr$eto
      }
      
      onset_doy <- .onset_rainfall_one_year(
        rain_doy        = rain_doy,
        eto_doy         = eto_doy,
        window_start    = cal$window_start,
        window_length   = cal$window_length,
        criterion       = cal$criterion_internal,
        preset_value    = cal$preset_value,
        successive_days = cal$successive_days,
        occurrences     = cal$occurrences
      )
      
    } else {
      
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