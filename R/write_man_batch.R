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
#' @param eol End-of-line type. Default: `"windows"`.
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
    eol = "windows",
    climate_path = "CLIMATE/",
    base_path = getwd(),
    verbose = TRUE,
    clean = FALSE) {
  # Clean MANAGEMENT/ directory if clean = TRUE
  if (clean && fs::dir_exists(path)) {
    files <- fs::dir_ls(path, regexp = "\\.MAN$")
    if (length(files) > 0) {
      fs::file_delete(files)
      if (verbose) message("Cleaned ", length(files), " file(s) from ", path)
    }
  }

  # Discover or validate station_name from climate files
  cli_dir <- file.path(base_path, climate_path)
  cli_files <- list.files(
    cli_dir,
    pattern = "\\.CLI$",
    ignore.case = TRUE
  )
  available_stations <- tools::file_path_sans_ext(cli_files)

  if (is.null(station_name)) {
    if (length(available_stations) == 0) {
      stop(
        "No stations to process.\n",
        "No climate files were found in '", cli_dir, "'.\n",
        "Please generate climate files first using write_climate().",
        call. = FALSE
      )
    }
    station_name <- available_stations
    if (verbose) {
      message(
        "Auto-discovered ", length(station_name), " station(s): ",
        paste(head(station_name, 5), collapse = ", "),
        if (length(station_name) > 5) "..." else ""
      )
    }
  } else {
    missing <- setdiff(station_name, available_stations)
    if (length(missing) > 0) {
      stop(
        "Some stations do not have corresponding climate files.\n",
        "Missing climate files for: ",
        paste(missing, collapse = ", "), "\n",
        "Available stations: ",
        paste(head(available_stations, 5), collapse = ", "),
        if (length(available_stations) > 5) "..." else "", "\n",
        "Please generate them first using write_climate().",
        call. = FALSE
      )
    }
  }

  # Station count validation
  n <- length(station_name)
  if (n == 1 && verbose) {
    warning(
      "write_man_batch() called with a single station.\n",
      "Consider using write_man() instead for clarity.",
      call. = FALSE
    )
  }

  # Handle params
  if (is.null(params)) {
    params <- list()
  }

  if (is.list(params) && length(params) > 0 && !is.null(names(params))) {
    # Single named list - apply to all stations
    if (verbose) {
      message("Applying the same management parameters to all ", n, " station(s)")
    }
    params <- rep(list(params), n)
  } else if (is.list(params) && length(params) == 0) {
    # Empty list - use defaults for all
    if (verbose) {
      message("Using default management parameters for all ", n, " station(s)")
    }
    params <- rep(list(list()), n)
  } else if (!is.list(params) || length(params) != n) {
    stop(
      "params must be either:\n",
      "  - A single named list (applied to all stations), or\n",
      "  - A list of lists with length matching station_name\n",
      "Expected length: ", n, ", got: ", length(params),
      call. = FALSE
    )
  }

  # Write MAN files for each station
  if (verbose) {
    message("Writing MAN files for ", n, " station(s)...")
  }

  # Progress counter
  counter <- 0

  purrr::pwalk(
    .l = list(
      station_name = station_name,
      params = params
    ),
    .f = function(station_name, params) {
      if (verbose) {
        counter <<- counter + 1
        message("  [", counter, "/", n, "] Processing station: ", station_name)
      }

      # Call write_man with explicit arguments
      write_man(
        management_name = station_name,
        params = params,
        path = path,
        eol = eol
      )
    }
  )

  if (verbose) {
    message("Successfully created MAN files for ", n, " station(s)")
  }

  invisible(NULL)
}
