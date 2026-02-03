#' Discover or Validate Items from Climate Files
#'
#' @description
#' Internal helper function that discovers available stations/sites from climate
#' files or validates that provided item names have corresponding climate files.
#' This consolidates the repeated discovery logic used across `write_man_batch()`,
#' `write_sol_batch()`, and `write_prm_batch()`.
#'
#' @param item_names Character vector of item names to validate, or `NULL` for
#'   auto-discovery from climate files.
#' @param climate_path Path to climate directory (relative to base_path).
#'   Default: `"CLIMATE/"`
#' @param base_path Base absolute path for the project. Default: current working directory.
#' @param item_type Description for messages (e.g., "station", "site").
#'   Default: `"station"`
#' @param verbose Logical. If `TRUE`, prints discovery/validation messages.
#'   Default: `TRUE`
#' @param stop_on_missing Logical. If `TRUE` (default), stops with error when
#'   specified items don't have climate files. If `FALSE`, issues a warning.
#'
#' @return Character vector of validated/discovered item names.
#'
#' @details
#' When `item_names = NULL`, the function discovers all available items by
#' scanning for `.CLI` files in the climate directory and extracting their
#' base names (without extension).
#'
#' When `item_names` is provided, it validates that each item has a corresponding
#' `.CLI` file and either stops with an error or warns about missing files.
#'
#' @examples
#' # Auto-discover all stations
#' stations <- .discover_or_validate_items(
#'   item_names = NULL,
#'   climate_path = "CLIMATE/",
#'   base_path = getwd()
#' )
#'
#' # Validate specific stations
#' stations <- .discover_or_validate_items(
#'   item_names = c("grid_001", "grid_002"),
#'   climate_path = "CLIMATE/",
#'   base_path = getwd()
#' )
#'
#' @keywords internal
#' @noRd
.discover_or_validate_items <- function(
    item_names = NULL,
    climate_path = "CLIMATE/",
    base_path = getwd(),
    item_type = "station",
    verbose = TRUE,
    stop_on_missing = TRUE) {

  # Build path and discover available items
  cli_dir <- file.path(base_path, climate_path)
  cli_files <- list.files(cli_dir, pattern = "\\.CLI$", ignore.case = TRUE)
  available_items <- tools::file_path_sans_ext(cli_files)

  if (is.null(item_names)) {
    # Auto-discovery mode
    if (length(available_items) == 0) {
      stop(
        "No ", item_type, "s to process.\n",
        "No climate files were found in '", cli_dir, "'.\n",
        "Please generate climate files first using write_climate().",
        call. = FALSE
      )
    }

    item_names <- available_items

    if (verbose) {
      message(
        "Auto-discovered ", length(item_names), " ", item_type, "(s): ",
        paste(head(item_names, 5), collapse = ", "),
        if (length(item_names) > 5) "..." else ""
      )
    }
  } else {
    # Validation mode
    missing <- setdiff(item_names, available_items)

    if (length(missing) > 0) {
      msg <- paste0(
        "Some ", item_type, "s do not have corresponding climate files.\n",
        "Missing climate files for: ", paste(missing, collapse = ", "), "\n",
        "Available ", item_type, "s: ",
        paste(head(available_items, 5), collapse = ", "),
        if (length(available_items) > 5) "..." else "", "\n",
        "Please generate them first using write_climate()."
      )

      if (stop_on_missing) {
        stop(msg, call. = FALSE)
      } else {
        warning(msg, call. = FALSE)
      }
    }
  }

  item_names
}


#' Normalize Batch Parameters
#'
#' @description
#' Internal helper function that normalizes parameter input for batch operations.
#' Handles three input cases: NULL (use defaults), single named list (apply to all),
#' or list of lists (one per item). This consolidates repeated parameter handling
#' logic across batch functions.
#'
#' @param params Either:
#'   - `NULL` or empty list: Use default parameters for all items
#'   - A single named list: Apply same parameters to all items
#'   - A list of lists: One parameter set per item (must match `n`)
#' @param n Integer. Number of items being processed.
#' @param param_type Description for messages (e.g., "management", "soil").
#'   Default: `"parameter"`
#' @param verbose Logical. If `TRUE`, prints informational messages.
#'   Default: `TRUE`
#'
#' @return A list of lists with length `n`, where each element contains
#'   the parameters for one item.
#'
#' @examples
#' # NULL -> defaults for all
#' params <- .normalize_batch_params(NULL, n = 5, "soil")
#' # Returns: list(list(), list(), list(), list(), list())
#'
#' # Single list -> replicate for all
#' params <- .normalize_batch_params(list(cn = 65), n = 3, "soil")
#' # Returns: list(list(cn = 65), list(cn = 65), list(cn = 65))
#'
#' # List of lists -> validate length
#' params <- .normalize_batch_params(
#'   list(list(cn = 65), list(cn = 70)),
#'   n = 2, "soil"
#' )
#'
#' @keywords internal
#' @noRd
.normalize_batch_params <- function(params, n, param_type = "parameter", verbose = TRUE) {
  # Handle NULL
  if (is.null(params)) {
    params <- list()
  }

  # Case 1: Single named list - apply to all items
  if (is.list(params) && length(params) > 0 && !is.null(names(params))) {
    if (verbose) {
      message("Applying the same ", param_type, " parameters to all ", n, " item(s)")
    }
    return(rep(list(params), n))
  }

  # Case 2: Empty list - use defaults for all
  if (is.list(params) && length(params) == 0) {
    if (verbose) {
      message("Using default ", param_type, " parameters for all ", n, " item(s)")
    }
    return(rep(list(list()), n))
  }

  # Case 3: List of lists - validate length matches
  if (!is.list(params) || length(params) != n) {
    stop(
      "params must be either:\n",
      "  - A single named list (applied to all items), or\n",
      "  - A list of lists with length matching item count\n",
      "Expected length: ", n, ", got: ", length(params),
      call. = FALSE
    )
  }

  params
}


#' Run Batch Operation with Progress
#'
#' @description
#' Internal helper function that executes a function for each item in a batch
#' with optional progress messages. This consolidates the repeated pwalk pattern
#' across batch functions.
#'
#' @param items Character vector of item names to process.
#' @param params List of parameter lists (one per item). Must have same length as `items`.
#' @param fn Function to call for each item. Must accept `item` and `params` arguments,
#'   plus any additional arguments passed via `...`.
#' @param verbose Logical. If `TRUE`, prints progress messages for each item.
#'   Default: `TRUE`
#' @param item_type Description for messages (e.g., "station", "site").
#'   Default: `"item"`
#' @param ... Additional arguments passed to `fn`.
#'
#' @return Invisibly returns `NULL`. Called for its side effects.
#'
#' @details
#' The provided function `fn` is called for each item with the signature:
#' `fn(item = item_name, params = params_list, ...)`.
#'
#' Progress messages show: `[1/10] Processing station: grid_001`
#'
#' @examples
#' # Example usage in write_man_batch
#' .batch_with_progress(
#'   items = station_names,
#'   params = params_list,
#'   verbose = TRUE,
#'   item_type = "station",
#'   fn = function(item, params, path, eol) {
#'     write_man(management_name = item, params = params, path = path, eol = eol)
#'   },
#'   path = "MANAGEMENT/",
#'   eol = NULL
#' )
#'
#' @keywords internal
#' @noRd
.batch_with_progress <- function(items, params, fn, verbose = TRUE, item_type = "item", ...) {
  n <- length(items)
  counter <- 0

  purrr::pwalk(
    .l = list(item = items, params = params),
    .f = function(item, params, ...) {
      if (verbose) {
        counter <<- counter + 1
        message("  [", counter, "/", n, "] Processing ", item_type, ": ", item)
      }
      fn(item = item, params = params, ...)
    },
    ...
  )

  invisible(NULL)
}


#' Warn About Single Item in Batch Function
#'
#' @description
#' Internal helper that issues a warning when a batch function is called
#' with only a single item, suggesting use of the non-batch version instead.
#'
#' @param n Integer. Number of items.
#' @param batch_fn Character. Name of the batch function (e.g., "write_man_batch").
#' @param single_fn Character. Name of the single-item function (e.g., "write_man").
#' @param verbose Logical. If `TRUE`, issues the warning. Default: `TRUE`
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.warn_single_item <- function(n, batch_fn, single_fn, verbose = TRUE) {
  if (n == 1 && verbose) {
    warning(
      batch_fn, "() called with a single item.\n",
      "Consider using ", single_fn, "() instead for clarity.",
      call. = FALSE
    )
  }
  invisible(NULL)
}


#' Clean Directory Before Batch Write
#'
#' @description
#' Internal helper that removes files matching a pattern from a directory.
#' Used by batch functions when `clean = TRUE` to clear existing files
#' before writing new ones.
#'
#' @param path Directory path to clean.
#' @param pattern Regex pattern for files to remove (e.g., `"\\.MAN$"`).
#' @param verbose Logical. If `TRUE`, prints message about cleaned files.
#'   Default: `TRUE`
#'
#' @return Invisibly returns the number of files removed.
#'
#' @keywords internal
#' @noRd
.clean_directory <- function(path, pattern, verbose = TRUE) {
  n_removed <- 0

  if (fs::dir_exists(path)) {
    files <- fs::dir_ls(path, regexp = pattern)
    if (length(files) > 0) {
      fs::file_delete(files)
      n_removed <- length(files)
      if (verbose) {
        message("Cleaned ", n_removed, " file(s) from ", path)
      }
    }
  }

  invisible(n_removed)
}
