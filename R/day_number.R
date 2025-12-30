#' Compute AquaCrop day-number from a given date
#'
#' @description
#' Calculate AquaCrop day-number from various date formats.
#' The day number represents days elapsed since January 1, 1901.
#'
#' @param year Year (numeric 1901+), date string (yyyy-mm-dd), or Date object
#' @param month Month number (1-12). Optional if using doy or date string
#' @param day Day number (1-31). Optional if using doy or date string
#' @param doy Day of year (1-366). Optional, alternative to month/day
#'
#' @return A numeric representing AquaCrop day number
#'
#' @details
#' Flexible input handling:
#' - `day_number(2023, 4, 30)` — year, month, day
#' - `day_number(2023, doy = 120)` — year and day-of-year
#' - `day_number("2023-04-30")` — date string
#' - `day_number(as.Date("2023-04-30"))` — Date object
#'
#' @examples
#' day_number(2023, 4, 30)
#' day_number(2023, doy = 120)
#' day_number("2023-04-30")
#'
#' @export
day_number <- function(year, month = NULL, day = NULL, doy = NULL) {
  # Case 1: String date input
  if (is.character(year)) {
    date <- lubridate::ymd(year)
    if (is.na(date)) {
      stop("Invalid date string. Use format 'yyyy-mm-dd'")
    }
    year_val <- lubridate::year(date)
    month_val <- lubridate::month(date)
    day_val <- lubridate::day(date)

    if (year_val < 1901) stop("Year must be >= 1901")

    return(.compute_day_number(year_val, month_val, day_val))
  }

  # Case 2: Date object input
  if (inherits(year, "Date")) {
    year_val <- as.integer(format(year, "%Y"))
    month_val <- as.integer(format(year, "%m"))
    day_val <- as.integer(format(year, "%d"))

    if (year_val < 1901) stop("Date must be >= 1901-01-01")

    return(.compute_day_number(year_val, month_val, day_val))
  }

  # Case 3: Numeric inputs - year + doy
  if (!is.null(doy)) {
    if (is.null(year)) stop("year is required")
    return(.compute_from_ydoy(year, doy))
  }

  # Case 4: Numeric inputs - year + month + day
  if (!is.null(month) && !is.null(day)) {
    return(.compute_day_number(year, month, day))
  }

  # Invalid input
  stop(
    "Invalid arguments. Use one of:\n",
    "  day_number(2023, 4, 30)\n",
    "  day_number(2023, doy = 120)\n",
    "  day_number('2023-04-30')\n",
    "  day_number(as.Date('2023-04-30'))"
  )
}

# Internal helper: Year, month, day computation
.compute_day_number <- function(year, month, day) {
  # Validation
  if (!is.numeric(year) || !is.numeric(month) || !is.numeric(day)) {
    stop("year, month, day must be numeric")
  }

  year <- as.integer(year)
  month <- as.integer(month)
  day <- as.integer(day)

  if (year < 1901) stop("year must be >= 1901")
  if (month < 1 || month > 12) stop("month must be 1-12")
  if (day < 1 || day > 31) stop("day must be 1-31")

  # Days elapsed per month (accounting for leap years)
  elapsed_days <- c(
    0, 31, 59.25, 90.25,
    120.25, 151.25, 181.25, 212.25,
    243.25, 273.25, 304.25, 334.25
  )

  n_day <- floor(
    (year - 1901) * 365.25 + elapsed_days[month] + day + 0.05
  )

  return(n_day)
}

# Internal helper: Year + day-of-year
.compute_from_ydoy <- function(year, doy) {
  if (!is.numeric(year) || !is.numeric(doy)) {
    stop("year and doy must be numeric")
  }

  year <- as.integer(year)
  doy <- as.integer(doy)

  if (year < 1901) stop("year must be >= 1901")

  # Check leap year
  is_leap <- (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  max_doy <- ifelse(is_leap, 366, 365)

  if (doy < 1 || doy > max_doy) {
    stop("doy must be 1-", max_doy, " for year ", year)
  }

  # Convert to month/day
  date <- as.Date(paste0(year, "-01-01")) + (doy - 1)
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))

  return(.compute_day_number(year, month, day))
}

# Backward compatibility
#' @describeIn day_number Alias for backward compatibility
#' @export
DayNumber <- function(year, doy) {
  day_number(year = year, doy = doy)
}
