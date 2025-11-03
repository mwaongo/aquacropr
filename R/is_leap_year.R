#' Check if a Year is a Leap Year
#'
#' @param x A numeric scalar representing a year
#'
#' @return A logical scalar (TRUE if leap year, FALSE otherwise)
#'
#' @export
#'
#' @examples
#' is_leap_year(2020)
#' is_leap_year(2021)
#'
is_leap_year <- function(x) {
  if (!is.numeric(x) || length(x) != 1) {
    rlang::abort(
      message = "x should be a numeric scalar (single year)",
      class = "error",
      call = rlang::caller_env()
    )
  }

  lubridate::leap_year(
    lubridate::make_date(year = x)
  )
}
