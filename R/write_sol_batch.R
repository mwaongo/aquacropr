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
  #
  # clean SOIL/ dir if clean = TRUE (default)
  if (clean && fs::dir_exists(path)) {
    files <- fs::dir_ls(path, regexp = "\\.(SOL|SW0)$")
    if (length(files) > 0) {
      fs::file_delete(files)
      if (verbose) message("Cleaned ", length(files), " file(s) from ", path)
    }
  }

  # Discover or validate site_name from climate files
  cli_dir <- file.path(base_path, climate_path)
  cli_files <- list.files(
    cli_dir,
    pattern = "\\.CLI$",
    ignore.case = TRUE
  )
  available_sites <- tools::file_path_sans_ext(cli_files)

  if (is.null(site_name)) {
    if (length(available_sites) == 0) {
      stop(
        "No sites to process.\n",
        "No climate files were found in '", cli_dir, "'.\n",
        "Please generate climate files first using write_climate().",
        call. = FALSE
      )
    }
    site_name <- available_sites
    if (verbose) {
      message(
        "Auto-discovered ", length(site_name), " site(s): ",
        paste(head(site_name, 5), collapse = ", "),
        if (length(site_name) > 5) "..." else ""
      )
    }
  } else {
    # Optional: validate that sites exist (if climate files are required)
    missing <- setdiff(site_name, available_sites)
    if (length(missing) > 0 && verbose) {
      warning(
        "Some sites do not have corresponding climate files: ",
        paste(missing, collapse = ", "), "\n",
        "Available sites: ",
        paste(head(available_sites, 5), collapse = ", "),
        if (length(available_sites) > 5) "..." else "",
        call. = FALSE
      )
    }
  }

  # Site count validation
  n <- length(site_name)
  if (n == 1 && verbose) {
    warning(
      "write_sol_batch() called with a single site.\n",
      "Consider using write_sol() instead for clarity.",
      call. = FALSE
    )
  }

  # Handle params
  if (is.null(params)) {
    params <- list()
  }

  if (is.list(params) && length(params) > 0 && !is.null(names(params))) {
    # Single named list - apply to all sites
    if (verbose) {
      message("Applying the same soil parameters to all ", n, " site(s)")
    }
    params <- rep(list(params), n)
  } else if (is.list(params) && length(params) == 0) {
    # Empty list - use defaults for all
    if (verbose) {
      message("Using default soil parameters for all ", n, " site(s)")
    }
    params <- rep(list(list()), n)
  } else if (!is.list(params) || length(params) != n) {
    stop(
      "params must be either:\n",
      "  - A single named list (applied to all sites), or\n",
      "  - A list of lists with length matching site_name\n",
      "Expected length: ", n, ", got: ", length(params),
      call. = FALSE
    )
  }

  # Write SOL files for each site
  if (verbose) {
    message("Writing SOL files for ", n, " site(s)...")
  }

  # Progress counter
  counter <- 0

  purrr::pwalk(
    .l = list(
      site_name = site_name,
      params = params
    ),
    .f = function(site_name, params) {
      if (verbose) {
        counter <<- counter + 1
        message("  [", counter, "/", n, "] Processing site: ", site_name)
      }

      # Call write_sol with explicit arguments
      write_sol(
        path = path,
        site_name = site_name,
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
    }
  )

  if (verbose) {
    message("Successfully created SOL files for ", n, " site(s)")
  }

  invisible(NULL)
}
