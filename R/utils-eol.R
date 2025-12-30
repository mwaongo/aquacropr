#' Get End-of-Line Character
#'
#' @description
#' Internal function to return the appropriate end-of-line character sequence
#' based on the operating system specification.
#'
#' @param eol Character string specifying the end-of-line style.
#'   Options: "windows", "unix", "linux", or "macOS". Default = "windows"
#'
#' @return Character string containing the EOL sequence:
#'   \itemize{
#'     \item Windows: "\\r\\n" (carriage return + line feed)
#'     \item Unix/Linux/macOS: "\\n" (line feed only)
#'   }
#'
#' @keywords internal
#' @noRd
.get_eol <- function(eol = "windows") {
  valid_eol <- c("windows", "unix", "linux", "macOS")

  if (!eol %in% valid_eol) {
    stop(
      "Invalid eol value: '", eol, "'\n",
      "Valid options are: ", paste(valid_eol, collapse = ", ")
    )
  }

  sep <- ifelse(
    test = eol %in% c("unix", "linux", "macOS"),
    yes = "\n",
    no = "\r\n"
  )

  return(sep)
}
