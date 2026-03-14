#' Write AquaCrop Observed Data (.OBS) File
#'
#' Creates an AquaCrop observed data file (.OBS) containing measured canopy
#' cover, dry biomass, and soil water content for model calibration and
#' validation. Missing values must be set to -9.0. Entire columns can be
#' -9.0 when a variable was not measured.
#'
#' @param site_name Character. Name of the OBS file (without extension).
#'   Typically the station name.
#' @param obs_data data.frame. Observed data with columns:
#'   \describe{
#'     \item{day}{Integer. Day number from reference date.}
#'     \item{cc_mean}{Numeric. Mean canopy cover in percent. Use -9.0 if missing.}
#'     \item{cc_std}{Numeric. Standard deviation of canopy cover. Use -9.0 if missing.}
#'     \item{biomass_mean}{Numeric. Mean dry biomass in ton/ha. Use -9.0 if missing.}
#'     \item{biomass_std}{Numeric. Standard deviation of dry biomass. Use -9.0 if missing.}
#'     \item{swc_mean}{Numeric. Mean soil water content in mm. Use -9.0 if missing.}
#'     \item{swc_std}{Numeric. Standard deviation of soil water content. Use -9.0 if missing.}
#'   }
#' @param path Output directory path for OBS files. Default: "OBS/".
#' @param soil_depth Numeric. Depth of sampled soil profile in meters.
#'   Default: 1.00.
#' @param start_day Integer. First day of observations. Default: 1.
#' @param start_month Integer. First month of observations. Default: 1.
#' @param start_year Integer. First year of observations. Use 1901 if not
#'   linked to a specific year. Default: 1901.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#'
#' @return Invisibly returns the output file path.
#'
#' @examples
#' \dontrun{
#' # Only biomass measured, canopy cover and soil water content missing
#' obs <- data.frame(
#'   day          = c(144, 160, 169, 176, 184),
#'   cc_mean      = rep(-9.0, 5),
#'   cc_std       = rep(-9.0, 5),
#'   biomass_mean = c(0.045, 0.386, 1.039, 1.217, 2.390),
#'   biomass_std  = rep(-9.0, 5),
#'   swc_mean     = rep(-9.0, 5),
#'   swc_std      = rep(-9.0, 5)
#' )
#'
#' write_obs(
#'   site_name    = "Ottawa",
#'   obs_data    = obs,
#'   soil_depth  = 1.00,
#'   start_day   = 1,
#'   start_month = 1,
#'   start_year  = 2014
#' )
#' }
#'
#' @family AquaCrop file writers
#' @export
write_obs <- function(
    site_name,
    obs_data,
    path        = "OBS/",
    soil_depth  = 1.00,
    start_day   = 1,
    start_month = 1,
    start_year  = 1901,
    version     = 7.1,
    eol         = NULL
) {

  # ---- Input validation ----
  if (!is.data.frame(obs_data)) {
    stop("obs_data must be a data.frame.", call. = FALSE)
  }

  required_cols <- c("day", "cc_mean", "cc_std",
                     "biomass_mean", "biomass_std",
                     "swc_mean", "swc_std")

  if (!all(required_cols %in% names(obs_data))) {
    stop(
      "obs_data must have columns: ",
      paste(required_cols, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (nrow(obs_data) == 0) {
    stop("obs_data must have at least one row.", call. = FALSE)
  }

  sep <- .get_eol(eol)

  # ---- Build header ----
  content <- .get_obs_header(
    version     = version,
    soil_depth  = soil_depth,
    start_day   = start_day,
    start_month = start_month,
    start_year  = start_year,
    eol         = eol
  )

  # ---- Append data rows ----
  # Column widths derived from the header alignment:
  #   day          : %6d
  #   cc_mean      : %10.1f
  #   cc_std       : %8.1f
  #   biomass_mean : %13.3f
  #   biomass_std  : %9.1f
  #   swc_mean     : %15.1f
  #   swc_std      : %9.1f
  data_rows <- paste(
    mapply(
      function(day, cc_mean, cc_std, biomass_mean, biomass_std, swc_mean, swc_std) {
        paste0(
          sprintf("%6d%10.1f%8.1f%13.3f%9.1f%15.1f%9.1f",
                  day, cc_mean, cc_std, biomass_mean, biomass_std, swc_mean, swc_std),
          sep
        )
      },
      obs_data$day,
      obs_data$cc_mean,
      obs_data$cc_std,
      obs_data$biomass_mean,
      obs_data$biomass_std,
      obs_data$swc_mean,
      obs_data$swc_std
    ),
    collapse = ""
  )

  content <- paste0(content, data_rows)

  # ---- Write file ----
  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  output_file <- paste0(path, site_name, ".OBS")

  readr::write_file(x = content, file = output_file)

  invisible(output_file)
}


