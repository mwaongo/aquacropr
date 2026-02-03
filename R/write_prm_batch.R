#' Write AquaCrop PRM files in batch
#'
#' Generate AquaCrop PRM files for multiple stations in a batch.
#'
#' @param station_name Character vector or `NULL`. Names of stations to process:
#'   - If `NULL`, all stations are automatically discovered from `.CLI` files in the `CLIMATE/` directory.
#'   - If a vector, only the specified stations will be processed; all must have corresponding climate files.
#' @param crop_name Character. Crop name (e.g., "maize", "wheat"). Required.
#' @param planting_schedule Either:
#'   - A single `data.frame` with columns `year` and `planting_doy`, applied to all stations, or
#'   - A `list` of `data.frame`s (one per station), each with columns `year` and `planting_doy`.
#'     The length of the list must match the number of stations.
#' @param path Output directory path for PRM files. Default: `"LIST/"`.
#' @param crop_path Path to crop files directory. Default: `"CROP/"`.
#' @param climate_path Path to climate files directory. Default: `"CLIMATE/"`.
#' @param management_path Path to management files directory. Default: `"MANAGEMENT/"`.
#' @param soil_path Path to soil files directory. Default: `"SOIL/"`.
#' @param crop_duration Integer. Crop duration in days. Default: `90`.
#' @param simulation_start_doy Integer. Day of year to start simulation. Default: `NULL`.
#' @param scenario Character. Scenario name. Default: `"hist"`.
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#' @param use_standalone Logical. Use standalone mode? Default: `TRUE`.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If `TRUE` (default), prints progress messages. If `FALSE`, runs silently.
#' @param clean Logical. If `TRUE`, removes existing `.PRM` files
#'   from `path` before writing new files. Default: `FALSE`.
#'
#' @details
#' The function validates that:
#' - All specified stations have corresponding climate files
#' - The crop file exists in the `CROP/` directory
#'
#' A single `data.frame` will be automatically applied to all stations.
#'
#' Use this function when you need to create PRM files for **multiple stations at once**.
#'
#' @family batch operations
#' @return Invisibly returns `NULL`. The main effect is writing PRM files to the specified directory.
#'
#' @examples
#' \dontrun{
#' # Example 1: multiple stations, same planting schedule
#' plsch <- data.frame(year = 1981:2020, planting_doy = 201)
#' stations <- c("grid_001", "grid_002")
#' write_prm_batch(
#'   station_name = stations,
#'   crop_name = "maize",
#'   planting_schedule = plsch,
#'   crop_duration = 90
#' )
#'
#' # Example 2: multiple stations, different planting schedules
#' plsch_list <- list(
#'   data.frame(year = 1981:2020, planting_doy = 201),
#'   data.frame(year = 1981:2020, planting_doy = 205)
#' )
#' write_prm_batch(
#'   station_name = stations,
#'   crop_name = "maize",
#'   planting_schedule = plsch_list,
#'   crop_duration = 120,
#'   path = "LIST/"
#' )
#'
#' # Example 3: auto-discover all stations, silent mode
#' write_prm_batch(
#'   station_name = NULL,
#'   crop_name = "wheat",
#'   planting_schedule = plsch,
#'   base_path = "/my/project/path",
#'   verbose = FALSE
#' )
#' }
#'
#' @seealso \code{\link{write_prm}} for single station PRM file generation
#'
#' @importFrom purrr pwalk
#' @importFrom tools file_path_sans_ext
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_prm_batch <- function(
    station_name = NULL,
    crop_name,
    planting_schedule,
    path = "LIST/",
    crop_path = "CROP/",
    climate_path = "CLIMATE/",
    management_path = "MANAGEMENT/",
    soil_path = "SOIL/",
    crop_duration = 90,
    simulation_start_doy = NULL,
    scenario = "hist",
    eol = NULL,
    use_standalone = TRUE,
    base_path = getwd(),
    verbose = TRUE,
    clean = FALSE) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.PRM$", verbose)
  }

  # Check if crop file exists
  crop_file <- fs::path(base_path, crop_path, paste0(crop_name, ".CRO"))
  if (!fs::file_exists(crop_file)) {
    stop(
      "Crop file not found: ", crop_file, "\n",
      "Please create the crop file first using write_cro().",
      call. = FALSE
    )
  }

  # Discover or validate stations from climate files
  station_name <- .discover_or_validate_items(
    item_names = station_name,
    climate_path = climate_path,
    base_path = base_path,
    item_type = "station",
    verbose = verbose
  )

  # Warn if single item
  n <- length(station_name)
  .warn_single_item(n, "write_prm_batch", "write_prm", verbose)

  # Handle planting schedule (similar to params normalization but for data.frames)
  if (is.data.frame(planting_schedule)) {
    if (verbose) {
      message("Applying the same planting schedule to all ", n, " station(s)")
    }
    planting_schedule <- rep(list(planting_schedule), n)
  }

  if (!is.list(planting_schedule) || length(planting_schedule) != n) {
    stop(
      "planting_schedule must be either:\n",
      "  - A single data.frame (applied to all stations), or\n",
      "  - A list of data.frames with length matching station_name\n",
      "Expected length: ", n, ", got: ", length(planting_schedule),
      call. = FALSE
    )
  }

  # Write PRM files
  if (verbose) {
    message("Writing PRM files for ", n, " station(s)...")
  }

  .batch_with_progress(
    items = station_name,
    params = planting_schedule,  # Using params slot for planting_schedule
    verbose = verbose,
    item_type = "station",
    fn = function(item, params, crop_name, path, base_path, crop_path,
                  crop_duration, climate_path, management_path, soil_path,
                  simulation_start_doy, scenario, eol, use_standalone) {
      write_prm(
        station_name = item,
        crop_name = crop_name,
        planting_schedule = params,  # params contains planting_schedule
        path = path,
        base_path = base_path,
        crop_path = crop_path,
        crop_duration = crop_duration,
        climate_path = climate_path,
        management_path = management_path,
        soil_path = soil_path,
        simulation_start_doy = simulation_start_doy,
        scenario = scenario,
        eol = eol,
        use_standalone = use_standalone
      )
    },
    crop_name = crop_name,
    path = path,
    base_path = base_path,
    crop_path = crop_path,
    crop_duration = crop_duration,
    climate_path = climate_path,
    management_path = management_path,
    soil_path = soil_path,
    simulation_start_doy = simulation_start_doy,
    scenario = scenario,
    eol = eol,
    use_standalone = use_standalone
  )

  if (verbose) {
    message("Successfully created PRM files for ", n, " station(s)")
  }

  invisible(NULL)
}
