#' Write AquaCrop Off-Season Conditions (.OFF) Files for Multiple Stations
#'
#' Generate AquaCrop off-season conditions files for multiple stations.
#'
#' @param station_name Character vector or NULL. Names of stations to process.
#'   If NULL, all stations are automatically discovered from .CLI files in
#'   the climate directory. If a vector, only the specified stations will be
#'   processed; all must have corresponding climate files.
#' @param mulch_before Integer. Percentage of ground surface covered by
#'   mulches before the growing period (0-100). Default: 0.
#' @param mulch_after Integer. Percentage of ground surface covered by
#'   mulches after the growing period (0-100). Default: 0.
#' @param mulch_effect Integer. Effect of mulches on reduction of soil
#'   evaporation in percent (10-100). Default: 50.
#' @param ecw_before Numeric. Electrical conductivity of irrigation water
#'   before the growing period (dS/m). Default: 0.
#' @param ecw_after Numeric. Electrical conductivity of irrigation water
#'   after the growing period (dS/m). Default: 0.
#' @param wet_surface Integer. Percentage of soil surface wetted by
#'   off-season irrigation (0-100). Default: 100.
#' @param irr_before Either a single data.frame applied to all stations, or
#'   a list of data.frames (one per station), or NULL (no events).
#'   Each data.frame must have columns day and depth. Maximum 5 rows.
#'   Default: NULL.
#' @param irr_after Either a single data.frame applied to all stations, or
#'   a list of data.frames (one per station), or NULL (no events).
#'   Each data.frame must have columns day and depth. Maximum 5 rows.
#'   Default: NULL.
#' @param path Output directory path for OFF files. Default: "MANAGEMENT/".
#' @param climate_path Path to climate files directory, used for station
#'   discovery. Default: "CLIMATE/".
#' @param description Character. Description written on the first line.
#'   Default: "Field and irrigation management conditions in the off-season".
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#' @param base_path Base absolute path. Default: current working directory.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'   If FALSE, runs silently.
#' @param clean Logical. If TRUE, removes existing .OFF files from path
#'   before writing new files. Default: FALSE.
#'
#' @details
#' The function validates that all specified stations have corresponding
#' climate files. A single data.frame for irr_before or irr_after will be
#' automatically applied to all stations. Mulch and ECw parameters are
#' scalar and applied uniformly to all stations.
#'
#' @family batch operations
#' @return Invisibly returns NULL. The main effect is writing OFF files to
#'   the specified directory.
#'
#' @examples
#' \dontrun{
#' stations <- c("grid_001", "grid_002")
#'
#' # Same conditions for all stations
#' write_off_batch(
#'   station_name = stations,
#'   mulch_after  = 70,
#'   mulch_effect = 50,
#'   ecw_before   = 1.5,
#'   ecw_after    = 3.0,
#'   irr_before   = data.frame(day = 10, depth = 40)
#' )
#'
#' # Different irrigation events per station
#' write_off_batch(
#'   station_name = stations,
#'   ecw_before   = 1.5,
#'   irr_before   = list(
#'     data.frame(day = 10, depth = 40),
#'     data.frame(day = 15, depth = 35)
#'   )
#' )
#' }
#'
#' @seealso \code{\link{write_off}} for single station OFF file generation.
#'
#' @importFrom fs path file_exists dir_exists dir_ls file_delete
#' @export
write_off_batch <- function(
    station_name = NULL,
    mulch_before = 0,
    mulch_after  = 0,
    mulch_effect = 50,
    ecw_before   = 0,
    ecw_after    = 0,
    wet_surface  = 100,
    irr_before   = NULL,
    irr_after    = NULL,
    path         = "MANAGEMENT/",
    climate_path = "CLIMATE/",
    description  = "Field and irrigation management conditions in the off-season",
    version      = 7.1,
    eol          = NULL,
    base_path    = getwd(),
    verbose      = TRUE,
    clean        = FALSE
) {

  # Clean directory if requested
  if (clean) {
    .clean_directory(path, "\\.OFF$", verbose)
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
  .warn_single_item(n, "write_off_batch", "write_off", verbose)

  # Normalize irr_before: NULL, single data.frame, or list
  irr_before <- .normalize_off_irr(irr_before, n, "irr_before", verbose)
  irr_after  <- .normalize_off_irr(irr_after,  n, "irr_after",  verbose)

  if (verbose) {
    message("Writing OFF files for ", n, " station(s)...")
  }

  .batch_with_progress(
    items     = station_name,
    params    = seq_len(n),   # index to retrieve per-station irr data
    verbose   = verbose,
    item_type = "station",
    fn = function(item, params, mulch_before, mulch_after, mulch_effect,
                  ecw_before, ecw_after, wet_surface, irr_before, irr_after,
                  path, description, version, eol) {
      write_off(
        off_name     = item,
        mulch_before = mulch_before,
        mulch_after  = mulch_after,
        mulch_effect = mulch_effect,
        ecw_before   = ecw_before,
        ecw_after    = ecw_after,
        wet_surface  = wet_surface,
        irr_before   = irr_before[[params]],
        irr_after    = irr_after[[params]],
        path         = path,
        description  = description,
        version      = version,
        eol          = eol
      )
    },
    mulch_before = mulch_before,
    mulch_after  = mulch_after,
    mulch_effect = mulch_effect,
    ecw_before   = ecw_before,
    ecw_after    = ecw_after,
    wet_surface  = wet_surface,
    irr_before   = irr_before,
    irr_after    = irr_after,
    path         = path,
    description  = description,
    version      = version,
    eol          = eol
  )

  if (verbose) {
    message("Successfully created OFF files for ", n, " station(s)")
  }

  invisible(NULL)
}


#' Normalize Off-Season Irrigation Data for Batch Processing
#'
#' Converts NULL, a single data.frame, or a list of data.frames into a
#' list of length n suitable for per-station indexing in write_off_batch.
#'
#' @param irr data.frame, list of data.frames, or NULL.
#' @param n Integer. Number of stations.
#' @param label Character. Argument name for error messages.
#' @param verbose Logical. Whether to print normalization messages.
#'
#' @return A list of length n. Each element is a data.frame or NULL.
#' @keywords internal
#' @noRd
.normalize_off_irr <- function(irr, n, label, verbose) {
  if (is.null(irr)) {
    return(rep(list(NULL), n))
  }

  if (is.data.frame(irr)) {
    if (verbose) {
      message("Applying the same ", label, " to all ", n, " station(s)")
    }
    return(rep(list(irr), n))
  }

  if (!is.list(irr) || length(irr) != n) {
    stop(
      label, " must be either:\n",
      "  - NULL (no events), or\n",
      "  - A single data.frame (applied to all stations), or\n",
      "  - A list of data.frames with length matching station_name\n",
      "Expected length: ", n, ", got: ", length(irr),
      call. = FALSE
    )
  }

  irr
}
