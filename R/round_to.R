#' Round to Nearest Multiple
#'
#' @description
#' Round numeric values to the nearest multiple of a specified number.
#' Useful for standardizing parameter values or creating rounded intervals.
#'
#' @param x Numeric vector to round
#' @param to Numeric value specifying the multiple to round to. Default = 5
#' @param threshold Numeric value specifying the threshold for rounding up.
#'   Values where \code{x \%\% to >= threshold} will round up, otherwise round down.
#'   Default = \code{to/2} (standard rounding)
#'
#' @details
#' The function rounds values to the nearest multiple of \code{to}:
#' \itemize{
#'   \item If the remainder (\code{x \%\% to}) is greater than or equal to \code{threshold},
#'     the value rounds up to the next multiple
#'   \item Otherwise, it rounds down to the previous multiple
#' }
#'
#' By default, \code{threshold = to/2} provides standard rounding behavior
#' (e.g., 47.5 rounds to 50 when \code{to = 5}).
#' @family utility functions
#' @return
#' Numeric vector of the same length as \code{x} with values rounded to
#' the nearest multiple of \code{to}.
#'
#' @examples
#' # Round to nearest 5
#' round_to(47, to = 5) # Returns 45
#' round_to(48, to = 5) # Returns 50
#' round_to(50, to = 5) # Returns 50
#' round_to(52, to = 5) # Returns 50
#'
#' # Round to nearest 10
#' round_to(23, to = 10) # Returns 20
#' round_to(27, to = 10) # Returns 30
#' round_to(25, to = 10) # Returns 30 (standard rounding)
#'
#' # Round to nearest integer
#' round_to(47.8, to = 1) # Returns 48
#' round_to(47.3, to = 1) # Returns 47
#'
#' # Round to nearest 0.5
#' round_to(3.2, to = 0.5) # Returns 3.0
#' round_to(3.7, to = 0.5) # Returns 3.5
#'
#' # Works with vectors
#' round_to(c(12, 17, 23, 28), to = 5) # Returns c(10, 15, 25, 30)
#'
#' # Custom threshold (round up at 3 instead of 2.5 for to = 5)
#' round_to(47, to = 5, threshold = 3) # Returns 45
#' round_to(48, to = 5, threshold = 3) # Returns 50
#'
#' @export
round_to <- function(x, to = 5, threshold = to / 2) {
  ifelse(x %% to >= threshold, x - x %% to + to, x - x %% to)
}
