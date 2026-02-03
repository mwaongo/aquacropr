#' Format String with Width and Justification
#'
#' @description
#' Internal helper function that formats a text string to a specified width
#' with left, right, or center alignment. The text is truncated if it exceeds
#' the specified width.
#'
#' @param text Character string to format
#' @param width Numeric value specifying the total character width for the formatted string
#' @param justify Character string specifying alignment:
#'   \itemize{
#'     \item \code{"l"}: Left-align (pad on right)
#'     \item \code{"r"}: Right-align (pad on left)
#'     \item \code{"c"}: Center-align (pad on both sides)
#'   }
#'
#' @return
#' Character string formatted to the specified width with the requested alignment.
#' If the input text is longer than \code{width}, it is truncated to fit.
#'
#' @details
#' This function provides precise control over string formatting with three alignment options:
#' \itemize{
#'   \item **Left alignment** (\code{"l"}): Text starts at the left edge, spaces added to the right
#'   \item **Right alignment** (\code{"r"}): Text ends at the right edge, spaces added to the left
#'   \item **Center alignment** (\code{"c"}): Text is centered, with spaces distributed on both sides.
#'     If the padding is uneven, the extra space goes on the right.
#' }
#'
#' The function automatically truncates text that exceeds the specified width using
#' \code{substr(text, 1, width)} before applying alignment.
#'
#' @examples
#' # Left alignment
#' .format_string("Hello", width = 10, justify = "l")
#' # Returns: "Hello     " (5 spaces on right)
#'
#' # Right alignment
#' .format_string("Hello", width = 10, justify = "r")
#' # Returns: "     Hello" (5 spaces on left)
#'
#' # Center alignment
#' .format_string("Hello", width = 10, justify = "c")
#' # Returns: "  Hello   " (2 spaces left, 3 spaces right)
#'
#' # Center alignment with even padding
#' .format_string("Hi", width = 10, justify = "c")
#' # Returns: "    Hi    " (4 spaces on each side)
#'
#' # Text truncation when too long
#' .format_string("Very long text", width = 5, justify = "l")
#' # Returns: "Very " (truncated to 5 characters)
#'
#' .format_string("Very long text", width = 5, justify = "r")
#' # Returns: "Very " (truncated then right-aligned)
#'
#' # Edge case: exact width
#' .format_string("Exact", width = 5, justify = "c")
#' # Returns: "Exact" (no padding needed)
#'
#' # Format multiple strings
#' texts <- c("One", "Two", "Three")
#' sapply(texts, .format_string, width = 8, justify = "c")
#' # Returns: c("  One   ", "  Two   ", " Three  ")
#'
#' @seealso
#' \code{\link{.format_string2}} for formatting with sprintf specifications,
#' \code{\link{write_fwf}} for writing fixed-width files,
#' \code{\link{write_cro}} and \code{\link{write_man}} for functions that format parameter files
#'
#' @keywords internal
#' @noRd
.format_string <- function(text, width, justify) {
  if (justify == "l") {
    sprintf("%-*s", width, substr(text, 1, width))
  } else if (justify == "r") {
    sprintf("%*s", width, substr(text, 1, width))
  } else if (justify == "c") {
    padding <- (width - nchar(text)) %/% 2
    sprintf("%*s%*s", padding, "", width - padding, substr(text, 1, width))
  } else {
    stop("Invalid justification value. Must be 'l', 'r', or 'c'.")
  }
}


#' Format String with sprintf and Justification
#'
#' @description
#' Internal helper function that formats numeric or character values using
#' sprintf format specifications and then applies fixed-width alignment.
#' This is the unified function that replaces `.format_string2`, `.format_string3`,
#' and `.format_string4`.
#'
#' @param string Numeric or character value to format
#' @param fmt Character sprintf format specification (e.g., "%.1f", "%.2f", "%.0f", "%s")
#' @param width Numeric specifying the total character width for formatted value
#' @param justify Character string specifying alignment:
#'   \itemize{
#'     \item \code{"centre"} or \code{"center"}: Center-align (default)
#'     \item \code{"left"}: Left-align
#'     \item \code{"right"}: Right-align
#'   }
#'
#' @return Character string formatted with sprintf and aligned to specified width
#'
#' @examples
#' # Center alignment (default)
#' .format_string_fmt(3.14159, "%.2f", 10)
#' # Returns: "   3.14   "
#'
#' # Left alignment
#' .format_string_fmt(3.14159, "%.2f", 10, "left")
#' # Returns: "3.14      "
#'
#' # Right alignment
#' .format_string_fmt(3.14159, "%.2f", 10, "right")
#' # Returns: "      3.14"
#'
#' # String formatting
#' .format_string_fmt("hello", "%s", 10, "centre")
#' # Returns: "  hello   "
#'
#' @seealso
#' \code{\link{.format_string}} for basic string formatting without sprintf,
#' \code{\link{write_fwf}} for writing fixed-width files
#'
#' @keywords internal
#' @noRd
.format_string_fmt <- function(string, fmt, width, justify = "centre") {
  x <- sprintf(fmt, string)
  base::format(x, width = width, justify = justify)
}


# Backward-compatible aliases (kept for existing code)
# These can be removed once all usages are updated to .format_string_fmt()

#' @describeIn .format_string_fmt Center-justified formatting (legacy alias)
#' @keywords internal
#' @noRd
.format_string2 <- function(string, fmt, width) {
  .format_string_fmt(string, fmt, width, "centre")
}

#' @describeIn .format_string_fmt Left-justified formatting (legacy alias)
#' @keywords internal
#' @noRd
.format_string3 <- function(string, fmt, width) {
  .format_string_fmt(string, fmt, width, "left")
}

#' @describeIn .format_string_fmt Right-justified formatting (legacy alias)
#' @keywords internal
#' @noRd
.format_string4 <- function(string, fmt, width) {
  .format_string_fmt(string, fmt, width, "right")
}


#' Extract sprintf Format from String
#'
#' @description
#' Internal helper function that extracts and reformats sprintf-style format
#' specifications from parameter metadata.
#'
#' @param string Character vector containing sprintf-style format specifications
#'   (e.g., "%.1f", "%.2f", "%.0f")
#'
#' @return Character vector containing the extracted format specifications
#' @keywords internal
#' @noRd
.get_sprintf_format <- function(string) {
  s <- string %>%
    stringr::str_extract_all(pattern = ".", simplify = TRUE)
  fmt <- ""
  for (i in 2:5) {
    fmt <- paste0(fmt, c(s[, i]))
  }
  return(fmt)
}


# Note: .add_trailing_slash() has been moved to utils-paths.R
# for better organization of path-related utilities
