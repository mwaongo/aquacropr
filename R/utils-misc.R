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
