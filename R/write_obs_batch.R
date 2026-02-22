#' Write AquaCrop Observed Data (.OBS) Files for Multiple Stations
#'
#' Generate AquaCrop observed data files for multiple stations.
#'
#' @param station_name Character vector or NULL. Names of stations to process.
#'   If NULL, all stations are automatically discovered from .CLI files in
#'   the climate directory. If a vector, only the specified stations will be
#'   processed; all must have corresponding climate files.
#' @param obs_data Either a single data.frame applied to all stations, or a
#'   list of data.frames (one per station). Each data.frame must have columns:
#'   day, cc_mean, cc_std, biomass_mean, biomass_std, swc_mean, swc_std.
#'   Use -9.0 for missing values.
#' @param path Output directory path for OBS files. Default: "OBS/".
#' @param climate_path Path to climate files directory, used for station
#'   discovery. Default: "CLIMATE/".
#' @param soil_depth Numeric. Depth of sampled soil profile in meters.
#'   Default: 1.00.
#' @param start_day Integer. First day of observations. Default: 1.
#' @param start_month Integer. First month of observations. Default: 1.
#' @param start_year Integer. First year of observations. Use 1901 if not
#'   linked to a specific year. Default: 1901.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently.
#' @param clean Logical. If TRUE, removes existing .OBS files from path
#'   before writing new files. Default: FALSE.
#'
#' @details
#' The function validates that all specified stations have corresponding
#' climate files. A single data.frame for obs_data will be automatically
#' applied to all stations. The first line of every OBS file is always
#' "default" as AquaCrop ignores this label at runtime.
#'
#' @family batch operations
#' @return Invisibly returns NULL. The main effect is writing OBS files to
#'   the specified directory.
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
#' # Same observations for all stations
#' write_obs_batch(
#'   station_name = c("grid_001", "grid_002"),
#'   obs_data     = obs,
#'   soil_depth   = 1.00,
#'   start_day    = 1,
#'   start_month  = 1,
#'   start_year   = 2014
#' )
#'
#' # Different observations per station
#' write_obs_batch(
#'   station_name = c("grid_001", "grid_002"),
#'   obs_data     = list(obs, obs),
#'   start_year   = 2014
#' )
#' }
#'
#' @seealso \code{\link{write_obs}} for single station OBS file generation.
#'
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_obs_batch <- function(
    station_name = NULL,
    obs_data,
    path         = "OBS/",
    climate_path = "CLIMATE/",
    soil_depth   = 1.00,
    start_day    = 1,
    start_month  = 1,
    start_year   = 1901,
    version      = 7.1,
    eol          = NULL,
    base_path    = getwd(),
    verbose      = TRUE,
    clean        = FALSE
) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.OBS$", verbose)
  }

  # Discover or validate stations from climate files
  station_name <- .discover_or_validate_items(
    item_names   = station_name,
    climate_path = climate_path,
    base_path    = base_path,
    item_type    = "station",
    verbose      = verbose
  )

  n <- length(station_name)
  .warn_single_item(n, "write_obs_batch", "write_obs", verbose)

  # Normalize obs_data: single data.frame or list of data.frames
  if (is.data.frame(obs_data)) {
    if (verbose) {
      message("Applying the same observation data to all ", n, " station(s)")
    }
    obs_data <- rep(list(obs_data), n)
  }

  if (!is.list(obs_data) || length(obs_data) != n) {
    stop(
      "obs_data must be either:\n",
      "  - A single data.frame (applied to all stations), or\n",
      "  - A list of data.frames with length matching station_name\n",
      "Expected length: ", n, ", got: ", length(obs_data),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Writing OBS files for ", n, " station(s)...")
  }

  .batch_with_progress(
    items     = station_name,
    params    = obs_data,
    verbose   = verbose,
    item_type = "station",
    fn = function(item, params, path, soil_depth, start_day,
                  start_month, start_year, version, eol) {
      write_obs(
        obs_name    = item,
        obs_data    = params,
        path        = path,
        soil_depth  = soil_depth,
        start_day   = start_day,
        start_month = start_month,
        start_year  = start_year,
        version     = version,
        eol         = eol
      )
    },
    path        = path,
    soil_depth  = soil_depth,
    start_day   = start_day,
    start_month = start_month,
    start_year  = start_year,
    version     = version,
    eol         = eol
  )

  if (verbose) {
    message("Successfully created OBS files for ", n, " station(s)")
  }

  invisible(NULL)
}
