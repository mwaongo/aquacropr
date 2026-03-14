#' Write AquaCrop PRM files for Multiple Stations
#'
#' Generate AquaCrop PRM files for multiple stations.
#'
#' @param site_name Character vector or NULL. Names of stations to process.
#'   If NULL, all stations are automatically discovered from .CLI files in
#'   the climate directory. If a vector, only the specified stations will be
#'   processed; all must have corresponding climate files.
#' @param crop_name Character. Crop name (e.g., "maize", "wheat").
#'   If NULL, section 3 is written as (None). Default: NULL.
#' @param planting_schedule Either a single data.frame with columns year and
#'   planting_doy, applied to all stations, or a list of data.frames (one per
#'   station), each with columns year and planting_doy. The length of the list
#'   must match the number of stations.
#'   Optional when calendar_path is provided: the planting schedule is then
#'   derived automatically from find_onset() for each station. If both are
#'   supplied, calendar_path takes precedence and planting_schedule is ignored
#'   with a warning.
#' @param path Output directory path for PRM files. Default: "LIST/".
#' @param crop_path Path to crop files directory. Default: "CROP/".
#'   If crop_name is NULL, crop_path is ignored and section 3 is written
#'   as (None).
#' @param climate_path Path to climate files directory. Default: "CLIMATE/".
#' @param calendar_path Path to calendar files directory. Default: NULL.
#'   When provided, the planting schedule is derived automatically via
#'   find_onset() for each station, and planting_schedule can be NULL.
#'   Section 2 is always written as (None): the CAL file is used only to
#'   compute onset dates, not referenced in the PRM.
#' @param management_path Path to field management files directory.
#'   Default: "MANAGEMENT/". If NULL, section 5 is written as (None).
#' @param irrigation_path Path to irrigation management files directory.
#'   Default: "MANAGEMENT/". If the .IRR file is not found for a station,
#'   a warning is issued and section 4 is written as (None) for that station.
#'   If NULL, section 4 is written as (None) for all stations.
#' @param soil_path Path to soil files directory. Required, cannot be NULL.
#'   Default: "SOIL/".
#' @param groundwater_path Path to groundwater table files directory.
#'   Default: NULL. If NULL, section 7 is written as (None).
#' @param offseason_path Path to off-season conditions files directory.
#'   Default: NULL. If the .OFF file is not found for a station, a warning
#'   is issued and section 9 is written as (None) for that station.
#' @param obs_path Path to field data files directory.
#'   Default: NULL. If the .OBS file is not found for a station, a warning
#'   is issued and section 10 is written as (None) for that station.
#' @param crop_duration Integer or NULL. Crop duration in days. If NULL (default), read automatically from the .CRO file.
#' @param simulation_start_doy Integer. Day of year to start simulation.
#'   Default: NULL.
#' @param scenario Character. Scenario name. Default: "hist".
#' @param eol End-of-line character style. One of "windows", "linux", "macos".
#'   If NULL (default), eol is auto-detected.
#' @param use_standalone Logical. Use standalone mode? Default: TRUE.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently.
#' @param clean Logical. If TRUE, removes existing .PRM files from path
#'   before writing new files. Default: FALSE.
#'
#' @details
#' The function validates that all specified stations have corresponding
#' climate files. If crop_name is not NULL, the crop file is also validated.
#' A single data.frame for planting_schedule will be automatically applied
#' to all stations.
#'
#' Validation of optional file paths (irrigation, off-season, observations)
#' is delegated to \code{\link{write_prm}}, which issues a warning and sets
#' the corresponding path to NULL when the file is not found.
#'
#' @family batch operations
#' @return Invisibly returns NULL. The main effect is writing PRM files to
#'   the specified directory.
#'
#' @examples
#' \dontrun{
#' # Example 1: multiple stations, same planting schedule
#' plsch    <- data.frame(year = 1981:2020, planting_doy = 201)
#' stations <- c("grid_001", "grid_002")
#'
#' write_prm_batch(
#'   site_name         = stations,
#'   crop_name         = "maize",
#'   planting_schedule = plsch,
#'   crop_duration     = 90
#' )
#'
#' # Example 2: multiple stations, different planting schedules
#' plsch_list <- list(
#'   data.frame(year = 1981:2020, planting_doy = 201),
#'   data.frame(year = 1981:2020, planting_doy = 205)
#' )
#'
#' write_prm_batch(
#'   site_name         = stations,
#'   crop_name         = "maize",
#'   planting_schedule = plsch_list,
#'   crop_duration     = 120,
#'   path              = "LIST/"
#' )
#'
#' # Example 3: calendar-based onset, no planting_schedule needed
#' write_prm_batch(
#'   site_name        = NULL,
#'   crop_name        = "wheat",
#'   calendar_path    = "CAL/",
#'   groundwater_path = "GWT/",
#'   base_path        = "/my/project/path",
#'   verbose          = FALSE
#' )
#' }
#'
#' @seealso \code{\link{write_prm}} for single station PRM file generation.
#'
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_prm_batch <- function(
    site_name            = NULL,
    crop_name            = NULL,
    planting_schedule    = NULL,
    path                 = "LIST/",
    crop_path            = "CROP/",
    climate_path         = "CLIMATE/",
    calendar_path        = NULL,
    management_path      = "MANAGEMENT/",
    irrigation_path      = "MANAGEMENT/",
    soil_path            = "SOIL/",
    groundwater_path     = NULL,
    offseason_path       = NULL,
    obs_path             = NULL,
    crop_duration        = NULL,
    simulation_start_doy = NULL,
    scenario             = "hist",
    eol                  = NULL,
    use_standalone       = TRUE,
    base_path            = getwd(),
    verbose              = TRUE,
    clean                = FALSE
) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.PRM$", verbose)
  }

  # Validate crop file only when crop_name is provided
  if (!is.null(crop_name)) {
    crop_file <- fs::path(base_path, crop_path, paste0(crop_name, ".CRO"))
    if (!fs::file_exists(crop_file)) {
      stop(
        "Crop file not found: ", crop_file, "\n",
        "Please create the crop file first using write_cro().",
        call. = FALSE
      )
    }
  }

  # Discover or validate stations from climate files
  site_name <- .discover_or_validate_items(
    item_names   = site_name,
    climate_path = climate_path,
    base_path    = base_path,
    item_type    = "site",
    verbose      = verbose
  )

  n <- length(site_name)
  .warn_single_item(n, "write_prm_batch", "write_prm", verbose)

  # ---- Resolve planting schedule -------------------------------------------
  if (!is.null(calendar_path)) {
    if (!is.null(planting_schedule)) {
      warning(
        "Both calendar_path and planting_schedule were provided. ",
        "calendar_path takes precedence; planting_schedule is ignored.",
        call. = FALSE
      )
    }
    if (verbose) message("Computing onset days from CAL files...")
    planting_schedule <- lapply(site_name, function(site) {
      onset <- find_onset(
        site_name    = site,
        cal_path     = calendar_path,
        climate_path = climate_path,
        base_path    = base_path
      )
      data.frame(year = onset$year, planting_doy = onset$onset_doy)
    })
  } else {
    if (is.null(planting_schedule)) {
      stop(
        "planting_schedule is required when calendar_path is NULL.",
        call. = FALSE
      )
    }
    # Normalize: single data.frame applied to all stations
    if (is.data.frame(planting_schedule)) {
      if (verbose) {
        message("Applying the same planting schedule to all ", n, " site(s)")
      }
      planting_schedule <- rep(list(planting_schedule), n)
    }
    if (!is.list(planting_schedule) || length(planting_schedule) != n) {
      stop(
        "planting_schedule must be either:\n",
        "  - A single data.frame (applied to all stations), or\n",
        "  - A list of data.frames with length matching site_name\n",
        "Expected length: ", n, ", got: ", length(planting_schedule),
        call. = FALSE
      )
    }
  }

  if (verbose) message("Writing PRM files for ", n, " site(s)...")

  .batch_with_progress(
    items     = site_name,
    params    = planting_schedule,
    verbose   = verbose,
    item_type = "site",
    fn = function(item, params, crop_name, path, base_path, crop_path,
                  crop_duration, climate_path,
                  management_path, irrigation_path, soil_path,
                  groundwater_path, offseason_path, obs_path,
                  simulation_start_doy, scenario, eol, use_standalone) {
      write_prm(
        site_name            = item,
        crop_name            = crop_name,
        planting_schedule    = params,
        path                 = path,
        base_path            = base_path,
        crop_path            = crop_path,
        crop_duration        = crop_duration,
        climate_path         = climate_path,
        calendar_path        = NULL,
        management_path      = management_path,
        irrigation_path      = irrigation_path,
        soil_path            = soil_path,
        groundwater_path     = groundwater_path,
        offseason_path       = offseason_path,
        obs_path             = obs_path,
        simulation_start_doy = simulation_start_doy,
        scenario             = scenario,
        eol                  = eol,
        use_standalone       = use_standalone
      )
    },
    crop_name            = crop_name,
    path                 = path,
    base_path            = base_path,
    crop_path            = crop_path,
    crop_duration        = crop_duration,
    climate_path         = climate_path,
    management_path      = management_path,
    irrigation_path      = irrigation_path,
    soil_path            = soil_path,
    groundwater_path     = groundwater_path,
    offseason_path       = offseason_path,
    obs_path             = obs_path,
    simulation_start_doy = simulation_start_doy,
    scenario             = scenario,
    eol                  = eol,
    use_standalone       = use_standalone
  )

  if (verbose) message("Successfully created PRM files for ", n, " site(s)")

  invisible(NULL)
}
