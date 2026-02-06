#' Write Fixed-Width Format File
#'
#' @description
#' Write data to a fixed-width format (FWF) file where each column has a specified
#' character width. This function creates properly aligned text files commonly used
#' for data exchange with legacy systems and specific file format requirements like
#' AquaCrop climate files.
#'
#' @param x Data frame to write to file. All columns will be converted to character format.
#'   Factor columns are automatically converted to character before writing.
#' @param file Character string specifying the output file path. Can be a relative or
#'   absolute path.
#' @param width Numeric value or vector specifying the character width for each column:
#'   \itemize{
#'     \item Single value: All columns use the same width (e.g., \code{width = 10})
#'     \item Vector: Each column uses its corresponding width (e.g., \code{width = c(10, 15, 8)})
#'   }
#'   If a single value is provided for a multi-column data frame, it is recycled for all columns.
#' @param justify Character string specifying text alignment within each column width:
#'   \itemize{
#'     \item \code{"l"}: Left-align all columns
#'     \item \code{"r"}: Right-align all columns
#'     \item Multi-character string: Align each column individually (e.g., \code{"lrl"} for
#'       left, right, left alignment of three columns)
#'   }
#'   Default = \code{"l"}. If a single character is provided for a multi-column data frame,
#'   it is recycled for all columns.
#' @param replace_na Character string to use in place of NA values. Default = \code{"NA"}
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#'   \itemize{
#'     \item \code{"windows"}: Windows-style line endings (\\r\\n)
#'     \item \code{"unix"}, \code{"linux"}, \code{"macOS"}: Unix-style line endings (\\n)
#'   }
#'   Default = \code{"windows"}
#' @param append Logical. If \code{TRUE}, append to existing file. If \code{FALSE}, overwrite
#'   existing file. Default = \code{TRUE}
#'
#' @details
#' This function provides a lightweight solution for writing fixed-width format files without
#' additional dependencies beyond base R and readr. It is particularly useful for:
#' \itemize{
#'   \item Creating AquaCrop climate data files (.PLU, .ETo, .Tnx)
#'   \item Generating data files for legacy systems
#'   \item Producing human-readable aligned text output
#' }
#'
#' ## Column Alignment:
#' Text is aligned within the specified width using \code{sprintf} formatting:
#' \itemize{
#'   \item Left-aligned (\code{"l"}): Text starts at the left edge, padded on the right
#'   \item Right-aligned (\code{"r"}): Text ends at the right edge, padded on the left
#' }
#'
#' ## Data Conversion:
#' \itemize{
#'   \item Factor columns are automatically converted to character
#'   \item NA values are replaced with the string specified in \code{replace_na}
#'   \item All data is formatted as character strings with specified widths
#' }
#'
#' ## File Writing:
#' \itemize{
#'   \item By default, data is appended to existing files (\code{append = TRUE})
#'   \item Use \code{append = FALSE} to overwrite existing files
#'   \item Column names are not included in the output
#' }
#'
#' @return
#' Invisibly returns \code{NULL}. The function is called for its side effect of
#' writing data to a file.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' df <- data.frame(
#'   year = c(2020, 2021, 2022),
#'   rainfall = c(850.5, 920.3, 780.1),
#'   temp = c(25.2, 26.1, 24.8)
#' )
#'
#' # Write with uniform width, left-aligned
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = 10,
#'   justify = "l",
#'   append = FALSE
#' )
#'
#' # Write with different widths per column, right-aligned
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = c(6, 10, 10),
#'   justify = "r",
#'   append = FALSE
#' )
#'
#' # Write with mixed alignment (left, right, right)
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = c(6, 10, 10),
#'   justify = "lrr",
#'   append = FALSE
#' )
#'
#' # Append to existing file
#' write_fwf(
#'   x = df[1:2, ],
#'   file = "output.txt",
#'   width = 10,
#'   justify = "r",
#'   append = TRUE
#' )
#'
#' # Handle NA values with custom replacement
#' df_na <- data.frame(
#'   year = c(2020, 2021, NA),
#'   value = c(100, NA, 150)
#' )
#'
#' write_fwf(
#'   x = df_na,
#'   file = "output.txt",
#'   width = 10,
#'   justify = "r",
#'   replace_na = "-9999",
#'   append = FALSE
#' )
#'
#' # Unix-style line endings
#' write_fwf(
#'   x = df,
#'   file = "output.txt",
#'   width = 10,
#'   justify = "r",
#'   eol = "unix",
#'   append = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{write_plu}}, \code{\link{write_eto}}, \code{\link{write_tnx}} for
#' functions that use \code{write_fwf} to create AquaCrop climate files
#'
#' @export
write_fwf <- function(x, file, width,
                      justify = "l",
                      replace_na = "NA",
                      eol = NULL, append = TRUE) {
  # Convert factor columns to character
  fct_col <- which(sapply(x, is.factor))
  if (length(fct_col) > 0) {
    for (i in fct_col) {
      x[, i] <- as.character(x[, i])
    }
  }

  # Replace NA values
  x[is.na(x)] <- replace_na

  # Get number of columns
  n_col <- ncol(x)

  # Process justify parameter
  justify <- unlist(strsplit(justify, ""))
  justify <- as.character(factor(justify, c("l", "r"), c("-", "")))

  # Recycle width and justify if needed
  if (n_col != 1) {
    if (length(width) == 1) width <- rep(width, n_col)
    if (length(justify) == 1) justify <- rep(justify, n_col)
  }

  # Build sprintf format string
  sptf_fmt <- paste0(
    paste0("%", justify, width, "s"),
    collapse = ""
  )

  # Get end-of-line separator

  if (is.null(eol)) eol <- get_os()

  eol <- match.arg(tolower(eol), choices = c("windows", "linux", "macos"))

  sep <- .get_eol(eol = eol)

  # Format data and write to file
  tbl_content <- do.call(sprintf, c(fmt = sptf_fmt, x))
  readr::write_lines(tbl_content, file, append = append, sep = sep)
}
