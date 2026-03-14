#' Write AquaCrop Calendar (.CAL) Files for Multiple Sites
#'
#' Batch version of \code{\link{write_cal}}. Writes one .CAL file per row of
#' a parameter data frame or per element of a list. A progress bar is shown
#' when more than one file is written.
#'
#' @param params A data frame or list of named lists, each containing the
#'   arguments to pass to \code{\link{write_cal}} for one site. Required
#'   columns/names: \code{site_name}, \code{onset}. All other
#'   \code{write_cal()} arguments may be supplied; defaults from
#'   \code{write_cal()} apply for any column that is absent or \code{NA}.
#' @param path Output directory path for CAL files. Default: \code{"CAL/"}.
#'   Overridden per row if a \code{path} column is present in \code{params}.
#' @param version Numeric. AquaCrop version number. Default: 7.1.
#'   Overridden per row if a \code{version} column is present in \code{params}.
#' @param climate_path Character. Path to climate files directory. Used for
#'   auto-discovery of stations when \code{site_name} is absent from
#'   \code{params}. Default: \code{"CLIMATE/"}.
#' @param base_path Character. Base absolute path. Default: \code{getwd()}.
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages.
#' @param clean Logical. If \code{TRUE}, removes existing .CAL files from
#'   \code{path} before writing. Default: \code{FALSE}.
#' @param eol End-of-line character style. One of \code{"windows"},
#'   \code{"linux"}, \code{"macos"}. If \code{NULL} (default), auto-detected.
#'   Overridden per row if an \code{eol} column is present in \code{params}.
#'
#' @return Invisibly returns a character vector of output file paths, one per
#'   site.
#'
#' @examples
#' \dontrun{
#' params <- data.frame(
#'   site_name       = c("site_A", "site_B"),
#'   onset           = "rainfall",
#'   window_start    = 121L,
#'   window_length   = 92L,
#'   criterion       = 2L,
#'   preset_value    = 30,
#'   successive_days = 3L,
#'   occurrences     = 1L,
#'   stringsAsFactors = FALSE
#' )
#' write_cal_batch(params)
#'
#' # Fuzzy logic (criterion 7) for multiple sites
#' params_fuz <- data.frame(
#'   site_name       = c("site_C", "site_D"),
#'   onset           = "rainfall",
#'   window_start    = 121L,
#'   window_length   = 92L,
#'   criterion       = 7L,
#'   preset_value    = 20,   # cum_rain_lower
#'   successive_days = 3L,   # accum_days
#'   cum_rain_upper  = 40,
#'   wet_days_lower  = 1L,
#'   wet_days_upper  = 3L,
#'   dry_spell_lower = 7L,
#'   dry_spell_upper = 15L,
#'   fuzzy_threshold = 0.5,
#'   stringsAsFactors = FALSE
#' )
#' write_cal_batch(params_fuz)
#' }
#'
#' @family AquaCrop file writers
#' @export
write_cal_batch <- function(
    params,
    path         = "CAL/",
    version      = 7.1,
    eol          = NULL,
    climate_path = "CLIMATE/",
    base_path    = getwd(),
    verbose      = TRUE,
    clean        = FALSE
) {

  # ---- Clean directory if requested ---------------------------------------
  if (clean) .clean_directory(path, "\\.CAL$", verbose)

  # ---- Auto-discover site names if site_name absent from params -----------
  # When params has no site_name column (or params is a single named list),
  # stations are discovered from CLIMATE/ and the same params applied to all.
  if (is.data.frame(params) && !"site_name" %in% names(params)) {
    sites <- .discover_or_validate_items(
      item_names   = NULL,
      climate_path = climate_path,
      base_path    = base_path,
      item_type    = "site",
      verbose      = verbose
    )
    params <- do.call(
      rbind,
      lapply(sites, function(s) {
        row <- params
        row$site_name <- s
        row
      })
    )
  } else if (
    is.list(params) && !is.data.frame(params) && is.null(params[["site_name"]])
  ) {
    sites <- .discover_or_validate_items(
      item_names   = NULL,
      climate_path = climate_path,
      base_path    = base_path,
      item_type    = "site",
      verbose      = verbose
    )
    params <- lapply(sites, function(s) c(params, list(site_name = s)))
  }

  # ---- Normalise input to a list of named lists ----------------------------
  if (is.data.frame(params)) {
    rows <- lapply(seq_len(nrow(params)), function(i) {
      row <- as.list(params[i, , drop = FALSE])
      # Convert single-element lists to scalars; keep NA as NA
      lapply(row, function(x) if (length(x) == 1L) x[[1L]] else x)
    })
  } else if (is.list(params)) {
    rows <- params
  } else {
    stop("params must be a data frame or a list of named lists.", call. = FALSE)
  }

  .get_arg <- function(row, nm, default) {
    v <- row[[nm]]
    if (is.null(v) || (length(v) == 1L && is.na(v))) default else v
  }

  .batch_with_progress(
    items     = lapply(rows, function(r) r[["site_name"]]),
    params    = rows,
    verbose   = verbose,
    item_type = "site",
    fn = function(item, params, ...) {
      row <- params
      write_cal(
        site_name       = row[["site_name"]],
        onset           = row[["onset"]],
        fixed_day       = .get_arg(row, "fixed_day",       NULL),
        window_start    = .get_arg(row, "window_start",    NULL),
        window_length   = .get_arg(row, "window_length",   NULL),
        criterion       = .get_arg(row, "criterion",       NULL),
        preset_value    = .get_arg(row, "preset_value",    NULL),
        successive_days = .get_arg(row, "successive_days", NULL),
        occurrences     = .get_arg(row, "occurrences",     1L),
        rday            = .get_arg(row, "rday",            NULL),
        dspell          = .get_arg(row, "dspell",          NULL),
        lookahead_days  = .get_arg(row, "lookahead_days",  NULL),
        min_weekly_rain = .get_arg(row, "min_weekly_rain", NULL),
        spell_days      = .get_arg(row, "spell_days",      NULL),
        cum_rain_upper  = .get_arg(row, "cum_rain_upper",  NULL),
        wet_days_lower  = .get_arg(row, "wet_days_lower",  NULL),
        wet_days_upper  = .get_arg(row, "wet_days_upper",  NULL),
        dry_spell_lower = .get_arg(row, "dry_spell_lower", NULL),
        dry_spell_upper = .get_arg(row, "dry_spell_upper", NULL),
        fuzzy_threshold = .get_arg(row, "fuzzy_threshold", NULL),
        path            = .get_arg(row, "path",            path),
        description     = .get_arg(row, "description",     NULL),
        version         = .get_arg(row, "version",         version),
        eol             = .get_arg(row, "eol",             eol)
      )
    }
  )

  invisible(NULL)
}
