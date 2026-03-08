#' Write AquaCrop Calendar (.CAL) Files for Multiple Stations
#'
#' Generate AquaCrop calendar files for multiple stations. All stations share
#' the same onset type, criterion, and parameters.
#'
#' @param site_name Character vector or NULL. Names of stations to process.
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
#' @param criterion Integer. Criterion number within the onset group (1-4
#'   for thermal; 1-5 for rainfall). Required when onset is "rainfall" or
#'   "thermal". See \code{\link{write_cal}} for full description of each
#'   criterion per onset group.
#' @param preset_value Numeric. Threshold value for the criterion. Required
#'   when onset is "rainfall" or "thermal". For criterion 5 (fuzzy):
#'   lower bound of cumulative rainfall (mm), i.e. \code{cum_rain_lower}.
#' @param successive_days Integer. Number of consecutive days. Required for
#'   rainfall criteria 2 and 5 (where it represents \code{accum_days}) and
#'   thermal criteria 1, 2, and 3. Default: NULL.
#' @param occurrences Integer. Number of occurrences before onset is triggered
#'   (1-3). Ignored for criterion 5. Default: 1.
#' @param cum_rain_upper Numeric. Upper bound of cumulative rainfall (mm)
#'   for the fuzzy gamma_1 function. Required for criterion 5.
#'   Must be > \code{preset_value}.
#' @param wet_days_lower Integer. Lower bound of wet-day count for the fuzzy
#'   gamma_2 function. Required for criterion 5.
#' @param wet_days_upper Integer. Upper bound of wet-day count for the fuzzy
#'   gamma_2 function. Required for criterion 5.
#'   Must be > \code{wet_days_lower}.
#' @param dry_spell_lower Integer. Lower bound of longest dry spell (days)
#'   for the fuzzy gamma_3 function. Required for criterion 5.
#' @param dry_spell_upper Integer. Upper bound of longest dry spell (days)
#'   for the fuzzy gamma_3 function. Required for criterion 5.
#'   Must be > \code{dry_spell_lower}.
#' @param fuzzy_threshold Numeric between 0 and 1. Defuzzification threshold.
#' @param path Output directory path for CAL files. Default: "CAL/".
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
#'   site_name = stations,
#'   onset     = "fixed",
#'   fixed_day = 212
#' )
#'
#' # Rainfall criterion 2: 30 mm in 3 successive days
#' write_cal_batch(
#'   site_name       = stations,
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 2,
#'   preset_value    = 30,
#'   successive_days = 3,
#'   occurrences     = 1
#' )
#'
#' # Rainfall criterion 5: fuzzy logic (Waongo et al. 2014)
#' write_cal_batch(
#'   site_name       = stations,
#'   onset           = "rainfall",
#'   window_start    = 121,
#'   window_length   = 92,
#'   criterion       = 5,
#'   preset_value    = 20,
#'   successive_days = 3,
#'   cum_rain_upper  = 40,
#'   wet_days_lower  = 1,
#'   wet_days_upper  = 3,
#'   dry_spell_lower = 7,
#'   dry_spell_upper = 15,
#'   fuzzy_threshold = 0.5
#' )
#'
#' # Thermal criterion 1: daily Tmin >= 5 degC for 4 successive days
#' write_cal_batch(
#'   site_name       = stations,
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
    site_name       = NULL,
    onset,
    fixed_day       = NULL,
    window_start    = NULL,
    window_length   = NULL,
    criterion       = NULL,
    preset_value    = NULL,
    successive_days = NULL,
    occurrences     = 1L,
    # --- fuzzy extras (criterion 5 only) ---
    cum_rain_upper  = NULL,
    wet_days_lower  = NULL,
    wet_days_upper  = NULL,
    dry_spell_lower = NULL,
    dry_spell_upper = NULL,
    fuzzy_threshold = NULL,
    # ----------------------------------------
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

  site_name <- .discover_or_validate_items(
    item_names   = site_name,
    climate_path = climate_path,
    base_path    = base_path,
    item_type    = "site",
    verbose      = verbose
  )

  n <- length(site_name)
  .warn_single_item(n, "write_cal_batch", "write_cal", verbose)

  if (verbose) {
    message("Writing CAL files for ", n, " site(s)...")
  }

  .batch_with_progress(
    items     = site_name,
    params    = site_name,
    verbose   = verbose,
    item_type = "site",
    fn = function(item, params, onset, fixed_day, window_start,
                  window_length, criterion, preset_value,
                  successive_days, occurrences,
                  cum_rain_upper, wet_days_lower, wet_days_upper,
                  dry_spell_lower, dry_spell_upper, fuzzy_threshold,
                  path, description, version, eol) {
      write_cal(
        site_name       = item,
        onset           = onset,
        fixed_day       = fixed_day,
        window_start    = window_start,
        window_length   = window_length,
        criterion       = criterion,
        preset_value    = preset_value,
        successive_days = successive_days,
        occurrences     = occurrences,
        cum_rain_upper  = cum_rain_upper,
        wet_days_lower  = wet_days_lower,
        wet_days_upper  = wet_days_upper,
        dry_spell_lower = dry_spell_lower,
        dry_spell_upper = dry_spell_upper,
        fuzzy_threshold = fuzzy_threshold,
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
    cum_rain_upper  = cum_rain_upper,
    wet_days_lower  = wet_days_lower,
    wet_days_upper  = wet_days_upper,
    dry_spell_lower = dry_spell_lower,
    dry_spell_upper = dry_spell_upper,
    fuzzy_threshold = fuzzy_threshold,
    path            = path,
    description     = description,
    version         = version,
    eol             = eol
  )

  if (verbose) {
    message("Successfully created CAL files for ", n, " site(s)")
  }

  invisible(NULL)
}
