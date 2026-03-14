#' Write AquaCrop SOL files in batch
#'
#' Generate AquaCrop SOL (and optionally SW0) files for multiple sites in a batch.
#'
#' @param site_name Character vector or `NULL`. Names of sites to process:
#'   - If `NULL`, all sites are automatically discovered from `.CLI` files in the `CLIMATE/` directory.
#'   - If a vector, only the specified sites will be processed.
#' @param params Either:
#'   - A single named `list` of soil parameters (cn, rew, texture, thickness), applied to all sites, or
#'   - A `list` of named `list`s (one per site), each containing soil parameters.
#'     The length of the list must match the number of sites.
#'   Set to `NULL` or `list()` to use all default values for all sites.
#'   Valid parameters: `cn`, `rew`, `texture`, `thickness`.
#' @param path Output directory path. Default: `"SOIL/"`.
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#' @param verbose Logical. If `TRUE` (default), prints progress messages. If `FALSE`, runs silently.
#' @param clean Logical. If `TRUE`, removes existing `.SOL` and `.SW0` files
#'   from `path` before writing new files. Default: `FALSE`
#' @param write_sw0 Logical; if `TRUE`, also write SW0 files. Default: `TRUE`.
#' @param initial_water Initial water content specification for SW0 files. Default: `"WP"`.
#' @param salinity Salinity specification for SW0 files. Default: `"none"`.
#' @param initial_cc Initial canopy cover for SW0 files. Default: `-9.00`.
#' @param initial_biomass Initial biomass for SW0 files. Default: `0.000`.
#' @param initial_root_depth Initial root depth for SW0 files. Default: `-9.00`.
#' @param bund_water Bund water for SW0 files. Default: `0.0`.
#' @param bund_ec Bund EC for SW0 files. Default: `0.00`.
#' @param swo_layers Optional layers for SW0 files. Default: `NULL`.
#' @param climate_path Path to climate file directory for auto-discovery. Default: `"CLIMATE/"`.
#' @param base_path Base absolute path. Default: current working directory.
#'
#' @details
#' When `site_name = NULL`, the function discovers sites from `.CLI` files in the climate directory.
#'
#' A single `list` of parameters will be automatically applied to all sites.
#'
#' Valid parameters in `params` include `cn`, `rew`, `texture`, `thickness`.
#' SW0-related parameters (`initial_water`, `salinity`, etc.) are passed as direct arguments
#' to `write_sol_batch()` and will be applied to all sites.
#'
#' See \code{\link{write_sol}} documentation for parameter descriptions and valid ranges.
#'
#' Use this function when you need to create SOL files for **multiple sites at once**.
#'
#' @family batch operations
#' @return Invisibly returns `NULL`. The main effect is writing SOL (and optionally SW0) files
#'   to the specified directory.
#'
#' @examples
#' \dontrun{
#' # Example 1: Multiple sites, same soil profile
#' soil_params <- list(
#'   texture = c("sandy loam", "clay loam"),
#'   thickness = c(0.4, 0.6),
#'   cn = 65,
#'   rew = 7
#' )
#' sites <- c("grid_001", "grid_002", "grid_003")
#' write_sol_batch(
#'   site_name = sites,
#'   params = soil_params
#' )
#'
#' # Example 2: Multiple sites, different soil profiles
#' params_list <- list(
#'   list(texture = "sandy loam", thickness = 1.0, cn = 55, rew = 5),
#'   list(texture = "loam", thickness = 1.0, cn = 70, rew = 6),
#'   list(texture = c("clay", "clay loam"), thickness = c(0.3, 0.7), cn = 80, rew = 7)
#' )
#' write_sol_batch(
#'   site_name = sites,
#'   params = params_list,
#'   path = "SOIL/",
#'   initial_water = "FC",
#'   salinity = "none"
#' )
#'
#' # Example 3: Auto-discover all sites, default soil parameters
#' write_sol_batch(
#'   site_name = NULL,
#'   params = NULL,
#'   base_path = "/my/project/path",
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso \code{\link{write_sol}} for single soil file generation,
#'   \code{\link{write_swo}} for initial water content files,
#'   \code{\link{write_man_batch}} for batch management file generation,
#'   \code{\link{SoilWater}} for hydraulic property reference
#'
#' @importFrom purrr pwalk
#' @importFrom tools file_path_sans_ext
#' @export
write_sol_batch <- function(
    site_name = NULL,
    params = NULL,
    path = "SOIL/",
    eol = NULL,
    verbose = TRUE,
    clean = FALSE,
    write_sw0 = TRUE,
    initial_water = "WP",
    salinity = "none",
    initial_cc = -9.00,
    initial_biomass = 0.000,
    initial_root_depth = -9.00,
    bund_water = 0.0,
    bund_ec = 0.00,
    swo_layers = NULL,
    climate_path = "CLIMATE/",
    base_path = getwd()) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.(SOL|SW0)$", verbose)
  }

  # Discover or validate sites from climate files
  # Note: write_sol_batch uses stop_on_missing = FALSE (warning only)
  site_name <- .discover_or_validate_items(
    item_names = site_name,
    climate_path = climate_path,
    base_path = base_path,
    item_type = "site",
    verbose = verbose,
    stop_on_missing = FALSE
  )

  # Warn if single item
  n <- length(site_name)
  .warn_single_item(n, "write_sol_batch", "write_sol", verbose)

  # Normalize params to list of lists
  params <- .normalize_batch_params(params, n, "soil", verbose)

  # Write SOL files
  if (verbose) {
    message("Writing SOL files for ", n, " site(s)...")
  }

  .batch_with_progress(
    items = site_name,
    params = params,
    verbose = verbose,
    item_type = "site",
    fn = function(item, params, path, eol, write_sw0, initial_water, salinity,
                  initial_cc, initial_biomass, initial_root_depth,
                  bund_water, bund_ec, swo_layers) {
      write_sol(
        path = path,
        site_name = item,
        cn = params$cn,
        rew = params$rew,
        texture = params$texture,
        thickness = params$thickness,
        eol = eol,
        write_sw0 = write_sw0,
        initial_water = initial_water,
        salinity = salinity,
        initial_cc = initial_cc,
        initial_biomass = initial_biomass,
        initial_root_depth = initial_root_depth,
        bund_water = bund_water,
        bund_ec = bund_ec,
        swo_layers = swo_layers
      )
    },
    path = path,
    eol = eol,
    write_sw0 = write_sw0,
    initial_water = initial_water,
    salinity = salinity,
    initial_cc = initial_cc,
    initial_biomass = initial_biomass,
    initial_root_depth = initial_root_depth,
    bund_water = bund_water,
    bund_ec = bund_ec,
    swo_layers = swo_layers
  )

  if (verbose) {
    message("Successfully created SOL files for ", n, " site(s)")
  }

  invisible(NULL)
}
