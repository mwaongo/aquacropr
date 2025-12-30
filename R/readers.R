#' Read AquaCrop Season Output Files
#'
#' Reads seasonal output files from AquaCrop simulations (PRMSeason.OUT) and
#' returns a tibble with formatted data and cleaned column names.
#'
#' @param file Path to the AquaCrop output file (fooPRMSeason.OUT)
#'
#' @return A tibble containing the output file data with:
#'   \itemize{
#'     \item Columns formatted with camelCase names
#'     \item Automatically detected data types
#'   }
#'
#' @details
#' AquaCrop season output files follow a fixed-width format with:
#' - Lines 1-2: Headers/metadata
#' - Line 3: Column names
#' - Line 4+: Data
#'
#' @examples
#' \dontrun{
#' data <- read_season_out("C1PRMSeason.OUT")
#' }
#'
#' @export
read_season_out <- function(file) {
  .validate_file(file, "PRMseason.OUT")

  header <- .read_header(file)
  data <- readr::read_fwf(file = file, skip = 4)
  names(data) <- header

  return(data)
}



#' @keywords internal
.validate_file <- function(file, expected_suffix) {
  if (!file.exists(file)) {
    stop("file does not exist: ", file, call. = FALSE)
  }

  if (!endsWith(basename(file), expected_suffix)) {
    stop("file should be ended with ", expected_suffix,
      ", got: ", basename(file),
      call. = FALSE
    )
  }
}

#' @keywords internal
.read_header <- function(file) {
  readr::read_fwf(file = file, skip = 2, n_max = 1) %>%
    c() %>%
    unlist() %>%
    c("PRMName") %>%
    janitor::make_clean_names(case = "snake") %>%
    gsub(pattern = "_", replacement = "", x = .)
}
