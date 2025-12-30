#' Convert Dates to AquaCrop Day Numbers
#'
#' @description
#' Convert calendar dates to AquaCrop's internal day numbering system, which counts
#' days elapsed since January 1, 1901 (day 1). This generic function has methods
#' for different input types: numeric components, character strings, and Date objects.
#'
#' @param x Input to convert. Can be:
#'   \itemize{
#'     \item Numeric: year (when using with month and day parameters)
#'     \item Character: date string in format "yyyy-mm-dd"
#'     \item Date: Date object from base R or lubridate
#'   }
#' @param ... Additional arguments passed to methods
#'
#' @return Integer representing the AquaCrop day number (days since Jan 1, 1901)
#'
#' @details
#' AquaCrop uses a continuous day numbering system starting from January 1, 1901 (day 1).
#' The calculation accounts for leap years using a 365.25-day average year length.
#'
#' @examples
#' # Method 1: Numeric components
#' to_aquacrop_day(2023, 6, 15)
#'
#' # Method 2: Character string
#' to_aquacrop_day("2023-06-15")
#'
#' # Method 3: Date object
#' to_aquacrop_day(as.Date("2023-06-15"))
#' to_aquacrop_day(Sys.Date())
#'
#' @export
to_aquacrop_day <- function(x, ...) {
  UseMethod("to_aquacrop_day")
}


#' @rdname to_aquacrop_day
#' @param month Integer representing the month (1-12)
#' @param day Integer representing the day of month (1-31)
#' @export
to_aquacrop_day.numeric <- function(x, month, day, ...) {
  year <- x # x is year when using numeric method

  # Validate inputs are numeric
  if (!is.numeric(year) || !is.numeric(month) || !is.numeric(day)) {
    stop(
      "All arguments must be numeric.",
      "\nReceived: year (", class(year)[1], "), ",
      "month (", class(month)[1], "), ",
      "day (", class(day)[1], ")"
    )
  }

  # Validate single values
  if (length(year) != 1 || length(month) != 1 || length(day) != 1) {
    stop(
      "All arguments must be single values.",
      "\nReceived lengths: year (", length(year), "), ",
      "month (", length(month), "), ",
      "day (", length(day), ")"
    )
  }

  # Convert to integers
  year <- as.integer(year)
  month <- as.integer(month)
  day <- as.integer(day)

  # Validate year range
  if (year < 1901) {
    stop(
      "year must be 1901 or later. Received: ", year,
      "\nAquaCrop day numbering starts from January 1, 1901"
    )
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (year > current_year + 50) {
    warning(
      "year (", year, ") is far in the future (> ", current_year + 50, ").",
      "\nVerify this is correct."
    )
  }

  # Validate month range
  if (month < 1 || month > 12) {
    stop("month must be between 1 and 12. Received: ", month)
  }

  # Validate day range (basic check)
  if (day < 1 || day > 31) {
    stop("day must be between 1 and 31. Received: ", day)
  }

  # More specific day validation based on month
  days_in_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (day > days_in_month[month]) {
    stop(
      "day (", day, ") is invalid for month ", month, ".",
      "\nMonth ", month, " has maximum ", days_in_month[month], " days."
    )
  }

  # Additional validation for February in non-leap years
  if (month == 2 && day == 29) {
    is_leap <- (year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0)
    if (!is_leap) {
      stop(
        "February 29 does not exist in year ", year, " (not a leap year)"
      )
    }
  }

  # Cumulative days at start of each month (accounting for leap year average)
  elapsed_days <- c(
    0, # January
    31, # February
    59.25, # March
    90.25, # April
    120.25, # May
    151.25, # June
    181.25, # July
    212.25, # August
    243.25, # September
    273.25, # October
    304.25, # November
    334.25 # December
  )

  # Calculate AquaCrop day number
  n_day <- floor(
    (year - 1901) * 365.25 + elapsed_days[month] + day + 0.05
  )

  return(as.integer(n_day))
}


#' @rdname to_aquacrop_day
#' @export
to_aquacrop_day.character <- function(x, ...) {
  # Validate input
  if (length(x) != 1) {
    stop(
      "Input must be a single date string. Received vector of length: ", length(x),
      "\nFor multiple dates, use vectorized operations: sapply(dates, to_aquacrop_day)"
    )
  }

  # Try to parse the date
  parsed_date <- tryCatch(
    {
      lubridate::ymd(x)
    },
    error = function(e) {
      stop(
        "Unable to parse date: '", x, "'",
        "\nExpected format: 'yyyy-mm-dd' (e.g., '2023-12-31')",
        "\nOriginal error: ", e$message
      )
    },
    warning = function(w) {
      stop(
        "Date parsing produced a warning for: '", x, "'",
        "\nExpected format: 'yyyy-mm-dd' (e.g., '2023-12-31')",
        "\nWarning: ", w$message
      )
    }
  )

  # Check if parsing resulted in NA
  if (is.na(parsed_date)) {
    stop(
      "Date parsing failed for: '", x, "'",
      "\nExpected format: 'yyyy-mm-dd' (e.g., '2023-12-31')"
    )
  }

  # Extract components and call numeric method
  year <- lubridate::year(parsed_date)
  month <- lubridate::month(parsed_date)
  day <- lubridate::day(parsed_date)

  # Use the numeric method
  to_aquacrop_day.numeric(year, month, day)
}


#' @rdname to_aquacrop_day
#' @export
to_aquacrop_day.Date <- function(x, ...) {
  # Validate input
  if (length(x) != 1) {
    stop(
      "Input must be a single Date object. Received vector of length: ", length(x),
      "\nFor multiple dates, use vectorized operations: sapply(dates, to_aquacrop_day)"
    )
  }

  # Check for NA
  if (is.na(x)) {
    stop("Cannot convert NA Date to AquaCrop day number")
  }

  # Extract components
  year <- as.integer(format(x, "%Y"))
  month <- as.integer(format(x, "%m"))
  day <- as.integer(format(x, "%d"))

  # Use the numeric method
  to_aquacrop_day.numeric(year, month, day)
}


#' @rdname to_aquacrop_day
#' @export
to_aquacrop_day.POSIXt <- function(x, ...) {
  # Convert POSIXt to Date first
  date_obj <- as.Date(x)

  # Use the Date method
  to_aquacrop_day.Date(date_obj)
}


#' @rdname to_aquacrop_day
#' @export
to_aquacrop_day.default <- function(x, ...) {
  stop(
    "No method for class '", class(x)[1], "'",
    "\nSupported types: numeric (year, month, day), character (date string), Date, POSIXt",
    "\nExample: to_aquacrop_day(2023, 6, 15) or to_aquacrop_day('2023-06-15')"
  )
}
