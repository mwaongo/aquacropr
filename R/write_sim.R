#' Write an AquaCrop Simulation Settings File
#'
#' Writes either \code{AggregationResults.SIM} or \code{DailyResults.SIM}
#' to the \code{SIMUL/} subdirectory.
#'
#' @param code Integer or integer vector.
#'   \itemize{
#'     \item For \code{"AggregationResults"}: single value, 0-3.
#'       0 = none, 1 = daily, 2 = 10-daily, 3 = monthly.
#'     \item For \code{"DailyResults"}: one or more values from 1-7.
#'       1 = soil water balance, 2 = crop development,
#'       3 = soil water content profile, 4 = soil salinity profile,
#'       5 = soil water content at depths, 6 = soil salinity at depths,
#'       7 = climate inputs.
#'   }
#' @param what Character. One of \code{"AggregationResults"} or
#'   \code{"DailyResults"}.
#' @param path Character. Output directory. Default: \code{"SIMUL/"}.
#' @param eol Character. End-of-line style: \code{"windows"}, \code{"linux"},
#'   or \code{"macos"}. If \code{NULL} (default), auto-detected.
#'
#' @return Invisibly returns the output file path.
#' @export
write_sim <- function(code, what = c("AggregationResults", "DailyResults"),
                      path = "SIMUL/", eol = NULL) {

  what <- match.arg(what)
  code <- as.integer(code)
  sep  <- .get_eol(eol)

  lines <- switch(what,

                  AggregationResults = {
                    if (length(code) != 1L || !code %in% 0L:3L)
                      stop("code must be a single integer 0-3 for AggregationResults.",
                           call. = FALSE)
                    paste0(code, " :  Time aggregation for intermediate results ",
                           "(0 = none ; 1 = daily; 2 = 10-daily; 3 = monthly)")
                  },

                  DailyResults = {
                    valid <- 1L:7L
                    bad   <- setdiff(code, valid)
                    if (length(bad) > 0L)
                      stop("Invalid code(s): ", paste(bad, collapse = ", "),
                           ". Must be 1-7 for DailyResults.", call. = FALSE)
                    labels <- c(
                      "Various parameters of the soil water balance",
                      "Crop development and production",
                      "Soil water content in the soil profile and root zone",
                      "Soil salinity in the soil profile and root zone",
                      "Soil water content at various depths of the soil profile",
                      "Soil salinity at various depths of the soil profile",
                      "Climate input parameters"
                    )
                    vapply(sort(code), function(i) paste0(i, " : ", labels[i]), character(1L))
                  }
  )

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  file <- file.path(path, paste0(what, ".SIM"))

  readr::write_file(
    x    = paste(paste0(lines, sep), collapse = ""),
    file = file
  )

  invisible(file)
}
