#' Write AquaCrop MAN files in batch
#'
#' Generate AquaCrop MAN files for multiple stations in a batch.
#'
#' @param station_name Character vector or `NULL`. Names of stations to process:
#'   - If `NULL`, all stations are automatically discovered from `.CLI` files in the `CLIMATE/` directory.
#'   - If a vector, only the specified stations will be processed; all must have corresponding climate files.
#' @param params Either:
#'   - A single named `list` of management parameters (var_02 to var_21), applied to all stations, or
#'   - A `list` of named `list`s (one per station), each containing management parameters.
#'     The length of the list must match the number of stations.
#'   Set to `NULL` or `list()` to use all default values for all stations.
#'   See \code{\link{ManData}} documentation for further details.
#' @param path Output directory path. Default: `"MANAGEMENT/"`.
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#' @param climate_path Path to climate file directory for auto-discovery. Default: `"CLIMATE/"`.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If `TRUE` (default), prints progress messages. If `FALSE`, runs silently.
#' @param clean Logical. If `TRUE`, removes existing `.MAN` files
#'   from `path` before writing new files. Default: `FALSE`.
#'
#' @details
#' The function validates that all specified stations have corresponding climate files.
#'
#' A single `list` of parameters will be automatically applied to all stations.
#'
#' Valid parameters include var_02 through var_21. See \code{\link{ManData}}
#' documentation for further details on parameter descriptions and valid ranges.
#'
#' Use this function when you need to create MAN files for **multiple stations at once**.
#'
#' @family batch operations
#' @return Invisibly returns `NULL`. The main effect is writing MAN files to the specified directory.
#'
#' @examples
#' \dontrun{
#' # Example 1: Multiple stations, same management parameters
#' base_params <- list(var_03 = 20, var_04 = 60, var_05 = 50)
#' stations <- c("grid_001", "grid_002")
#' write_man_batch(
#'   station_name = stations,
#'   params = base_params
#' )
#'
#' # Example 2: Multiple stations, different management parameters
#' params_list <- list(
#'   list(var_03 = 0, var_04 = 0, var_05 = 30), # Low fertility, no mulch
#'   list(var_03 = 60, var_04 = 80, var_05 = 70) # High fertility, heavy mulch
#' )
#' write_man_batch(
#'   station_name = stations,
#'   params = params_list,
#'   path = "MANAGEMENT/"
#' )
#'
#' # Example 3: Auto-discover all stations, default parameters
#' write_man_batch(
#'   station_name = NULL,
#'   params = NULL,
#'   base_path = "/my/project/path",
#'   verbose = FALSE
#' )
#'
#' # Example 4: Station-specific fertility gradient
#' params_list <- list(
#'   list(var_05 = 30), # Station 1: low fertility
#'   list(var_05 = 60), # Station 2: medium fertility
#'   list(var_05 = 90) # Station 3: high fertility
#' )
#' write_man_batch(
#'   station_name = c("site_A", "site_B", "site_C"),
#'   params = params_list
#' )
#' }
#'
#' @seealso \code{\link{write_man}} for single management file generation,
#'   \code{\link{write_prm_batch}} for batch PRM file generation,
#'   \code{\link{ManData}} for complete parameter reference
#'
#' @importFrom purrr pwalk
#' @importFrom tools file_path_sans_ext
#' @importFrom fs dir_exists dir_ls file_delete
#' @export
write_man_batch <- function(
    station_name = NULL,
    params = NULL,
    path = "MANAGEMENT/",
    eol = NULL,
    climate_path = "CLIMATE/",
    base_path = getwd(),
    verbose = TRUE,
    clean = FALSE) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.MAN$", verbose)
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
  .warn_single_item(n, "write_man_batch", "write_man", verbose)

  # Normalize params to list of lists
  params <- .normalize_batch_params(params, n, "management", verbose)

  # Write MAN files
  if (verbose) {
    message("Writing MAN files for ", n, " station(s)...")
  }

  .batch_with_progress(
    items = station_name,
    params = params,
    verbose = verbose,
    item_type = "station",
    fn = function(item, params, path, eol) {
      write_man(
        management_name = item,
        params = params,
        path = path,
        eol = eol
      )
    },
    path = path,
    eol = eol
  )

  if (verbose) {
    message("Successfully created MAN files for ", n, " station(s)")
  }

  invisible(NULL)
}
