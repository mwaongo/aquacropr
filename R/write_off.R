#' Write AquaCrop Off-Season Conditions (.OFF) File
#'
#' Creates an AquaCrop off-season conditions file (.OFF) describing field
#' management (mulches) and irrigation events before and after the growing
#' cycle.
#'
#' @param off_name Character. Name of the OFF file (without extension).
#'   Typically the station name.
#' @param mulch_before Integer. Percentage of ground surface covered by
#'   mulches before the growing period (0-100). Default: 0.
#' @param mulch_after Integer. Percentage of ground surface covered by
#'   mulches after the growing period (0-100). Default: 0.
#' @param mulch_effect Integer. Effect of mulches on reduction of soil
#'   evaporation in percent. Indicative values: 100 for synthetic plastic
#'   mulches, 50 for organic mulches, 10-100 for user-specified. Default: 50.
#' @param ecw_before Numeric. Electrical conductivity of irrigation water
#'   before the growing period (dS/m). Default: 0.
#' @param ecw_after Numeric. Electrical conductivity of irrigation water
#'   after the growing period (dS/m). Default: 0.
#' @param wet_surface Integer. Percentage of soil surface wetted by
#'   off-season irrigation (0-100). Default: 100.
#' @param irr_before data.frame or NULL. Irrigation events before the growing
#'   period. Must have columns:
#'   \describe{
#'     \item{day}{Integer. Number of days after start of simulation period.}
#'     \item{depth}{Integer. Net irrigation application depth in mm.}
#'   }
#'   Maximum 5 rows. Default: NULL (no events).
#' @param irr_after data.frame or NULL. Irrigation events after the growing
#'   period. Must have columns:
#'   \describe{
#'     \item{day}{Integer. Number of days after end of growing period.}
#'     \item{depth}{Integer. Net irrigation application depth in mm.}
#'   }
#'   Maximum 5 rows. Default: NULL (no events).
#' @param path Output directory path for OFF files. Default: "MANAGEMENT/".
#' @param description Character. Description written on the first line.
#'   Default: "Field and irrigation management conditions in the off-season".
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#'
#' @return Invisibly returns the output file path.
#'
#' @examples
#' \dontrun{
#' # No mulches, one irrigation event before season
#' write_off(
#'   off_name   = "station_01",
#'   mulch_after = 70,
#'   mulch_effect = 50,
#'   ecw_before = 1.5,
#'   ecw_after  = 3.0,
#'   irr_before = data.frame(day = 10, depth = 40)
#' )
#'
#' # Mulches and irrigation events both before and after
#' write_off(
#'   off_name     = "station_01",
#'   mulch_before = 50,
#'   mulch_after  = 70,
#'   mulch_effect = 100,
#'   ecw_before   = 1.5,
#'   ecw_after    = 3.0,
#'   irr_before   = data.frame(day = c(5, 10), depth = c(30, 40)),
#'   irr_after    = data.frame(day = c(5),     depth = c(25))
#' )
#' }
#'
#' @family AquaCrop file writers
#' @export
write_off <- function(
    off_name,
    mulch_before = 0,
    mulch_after  = 0,
    mulch_effect = 50,
    ecw_before   = 0,
    ecw_after    = 0,
    wet_surface  = 100,
    irr_before   = NULL,
    irr_after    = NULL,
    path         = "MANAGEMENT/",
    description  = "Field and irrigation management conditions in the off-season",
    version      = 7.1,
    eol          = NULL
) {

  # ---- Input validation ----
  if (mulch_before < 0 || mulch_before > 100) {
    stop("mulch_before must be 0-100.", call. = FALSE)
  }
  if (mulch_after < 0 || mulch_after > 100) {
    stop("mulch_after must be 0-100.", call. = FALSE)
  }
  if (mulch_effect < 10 || mulch_effect > 100) {
    stop("mulch_effect must be 10-100.", call. = FALSE)
  }
  if (wet_surface < 0 || wet_surface > 100) {
    stop("wet_surface must be 0-100.", call. = FALSE)
  }

  .validate_off_irr <- function(df, label) {
    if (is.null(df)) return(invisible(NULL))
    if (!is.data.frame(df)) {
      stop(label, " must be a data.frame.", call. = FALSE)
    }
    if (!all(c("day", "depth") %in% names(df))) {
      stop(label, " must have columns: day, depth.", call. = FALSE)
    }
    if (nrow(df) > 5) {
      stop(label, " cannot have more than 5 irrigation events.", call. = FALSE)
    }
  }

  .validate_off_irr(irr_before, "irr_before")
  .validate_off_irr(irr_after,  "irr_after")

  n_before <- if (is.null(irr_before)) 0L else nrow(irr_before)
  n_after  <- if (is.null(irr_after))  0L else nrow(irr_after)

  sep <- .get_eol(eol)

  # ---- Build header ----
  content <- .get_off_header(
    description  = description,
    version      = version,
    mulch_before = mulch_before,
    mulch_after  = mulch_after,
    mulch_effect = mulch_effect,
    n_before     = n_before,
    ecw_before   = ecw_before,
    n_after      = n_after,
    ecw_after    = ecw_after,
    wet_surface  = wet_surface,
    eol          = eol
  )

  # ---- Append irrigation events ----
  # Before season rows
  if (n_before > 0) {
    rows_before <- paste(
      mapply(function(day, depth) {
        paste0(sprintf("%7d%9d", day, depth), "    before season", sep)
      },
      irr_before$day,
      irr_before$depth
      ),
      collapse = ""
    )
    content <- paste0(content, rows_before)
  }

  # After season rows
  if (n_after > 0) {
    rows_after <- paste(
      mapply(function(day, depth) {
        paste0(sprintf("%7d%9d", day, depth), "    after season", sep)
      },
      irr_after$day,
      irr_after$depth
      ),
      collapse = ""
    )
    content <- paste0(content, rows_after)
  }

  # ---- Write file ----
  path <- .add_trailing_slash(path)
  fs::dir_create(path, recurse = TRUE)
  output_file <- paste0(path, off_name, ".OFF")

  readr::write_file(x = content, file = output_file)

  invisible(output_file)
}
