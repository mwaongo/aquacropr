#' Write AquaCrop Calendar (.CAL) Files for Multiple Stations
#'
#' Generate AquaCrop calendar files for multiple stations. All stations share
#' the same onset type, criterion, and parameters.
#'
#' @param station_name Character vector or NULL. Names of stations to process.
#'   If NULL, all stations are automatically discovered from .CLI files in
#'   the climate directory. If a vector, only the specified stations will be
#'   processed; all must have corresponding climate files.
#' @param onset Character. Onset type: "fixed", "rainfall", or "thermal".
#' @param fixed_day Integer. Calendar day (1-366) for fixed onset. Required
#'   when onset is "fixed".
#' @param window_start Integer. Start day of the time window. Required when
#'   onset is "rainfall" or "thermal".
#' @param window_length Integer. Length of the time window in days. Required
#'   when onset is "rainfall" or "thermal".
#' @param criterion Integer. Criterion number within the onset group (1-4).
#'   Required when onset is "rainfall" or "thermal".
#'   See write_cal() for full description of each criterion per onset group.
#' @param preset_value Numeric. Threshold value for the criterion. Required
#'   when onset is "rainfall" or "thermal".
#' @param successive_days Integer. Number of consecutive days. Required for
#'   rainfall criterion 2 and thermal criteria 1, 2, and 3. Default: NULL.
#' @param occurrences Integer. Number of occurrences before onset is triggered
#'   (1-3). Default: 1.
#' @param path Output directory path for CAL files. Default: "CLIMATE/".
#' @param climate_path Path to climate files directory, used for station
#'   discovery. Default: "CLIMATE/".
#' @param description Character. Description on the first line. Default:
#'   auto-generated based on onset type and criterion.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently.
#' @param clean Logical. If TRUE, removes existing .CAL files from path
#'   before writing new files. Default: FALSE.
#'
#' @return Invisibly returns NULL. The main effect is writing CAL files to
#'   the specified directory.
#'
#' @examples
#' \dontrun{
#' stations <- c("grid_001", "grid_002", "grid_003")
#'
#' # Fixed onset on day 212 for all stations
#' write_cal_batch(
#'   station_name = stations,
#'   onset        = "fixed",
#'   fixed_day    = 212
#' )
#'
#' # Rainfall criterion 2 for all stations: 30 mm in 3 successive days
#' write_cal_batch(
#'   station_name    = stations,
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 2,
#'   preset_value    = 30,
#'   successive_days = 3,
#'   occurrences     = 1
#' )
#'
#' # Thermal criterion 1 for all stations: daily Tmin >= 5 degC for 4 days
#' write_cal_batch(
#'   station_name    = stations,
#'   onset           = "thermal",
#'   window_start    = 1,
#'   window_length   = 60,
#'   criterion       = 1,
#'   preset_value    = 5.0,
#'   successive_days = 4,
#'   occurrences     = 1
#' )
#' }
#'
#' @seealso \code{\link{write_cal}} for single station CAL file generation.
#'
#' @family batch operations
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_cal_batch <- function(
    station_name    = NULL,
    onset,
    fixed_day       = NULL,
    window_start    = NULL,
    window_length   = NULL,
    criterion       = NULL,
    preset_value    = NULL,
    successive_days = NULL,
    occurrences     = 1L,
    path            = "CAL/",
    climate_path    = "CLIMATE/",
    description     = NULL,
    version         = 7.1,
    eol             = NULL,
    base_path       = getwd(),
    verbose         = TRUE,
    clean           = FALSE
) {

  if (clean) {
    .clean_directory(path, "\\.CAL$", verbose)
  }

  station_name <- .discover_or_validate_items(
    item_names   = station_name,
    climate_path = climate_path,
    base_path    = base_path,
    item_type    = "station",
    verbose      = verbose
  )

  n <- length(station_name)
  .warn_single_item(n, "write_cal_batch", "write_cal", verbose)

  if (verbose) {
    message("Writing CAL files for ", n, " station(s)...")
  }

  .batch_with_progress(
    items     = station_name,
    params    = station_name,
    verbose   = verbose,
    item_type = "station",
    fn = function(item, params, onset, fixed_day, window_start,
                  window_length, criterion, preset_value,
                  successive_days, occurrences, path, description,
                  version, eol) {
      write_cal(
        cal_name        = item,
        onset           = onset,
        fixed_day       = fixed_day,
        window_start    = window_start,
        window_length   = window_length,
        criterion       = criterion,
        preset_value    = preset_value,
        successive_days = successive_days,
        occurrences     = occurrences,
        path            = path,
        description     = description,
        version         = version,
        eol             = eol
      )
    },
    onset           = onset,
    fixed_day       = fixed_day,
    window_start    = window_start,
    window_length   = window_length,
    criterion       = criterion,
    preset_value    = preset_value,
    successive_days = successive_days,
    occurrences     = occurrences,
    path            = path,
    description     = description,
    version         = version,
    eol             = eol
  )

  if (verbose) {
    message("Successfully created CAL files for ", n, " station(s)")
  }

  invisible(NULL)
}
