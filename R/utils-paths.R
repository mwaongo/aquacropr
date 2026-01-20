#' Validate and Normalize Path to Absolute
#'
#' @description
#' Internal helper function to validate and convert paths to absolute paths.
#'
#' @param path Character string specifying a file or directory path
#' @param param_name Character string for error messages identifying the parameter
#'
#' @return Character string containing the normalized absolute path with trailing slash
#' @keywords internal
#' @noRd
.validate_and_normalize_path <- function(path, param_name) {
  # Check if path is relative
  if (!fs::is_absolute_path(path)) {
    warning(
      "Parameter '", param_name, "' is relative: '", path, "'\n",
      "Converting to absolute path: '", fs::path_abs(path), "'"
    )
    path <- fs::path_abs(path)
  }

  # Ensure trailing slash
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }

  return(path)
}


#' Get CO2 File Based on Scenario
#'
#' @description
#' Internal helper function to map climate scenario names to CO2 concentration files.
#'
#' @param scenario Character string specifying the climate scenario
#'   Options: "hist", "rcp26", "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585"
#'
#' @return Character string with the CO2 filename
#' @keywords internal
#' @noRd
.get_co2_file <- function(scenario) {
  co2_files <- list(
    "hist" = "MaunaLoa.CO2",
    "rcp26" = "RCP2-6.CO2",
    "rcp45" = "RCP4-5.CO2",
    "rcp60" = "RCP6-0.CO2",
    "rcp85" = "RCP8-5.CO2",
    "ssp119" = "SSP1_1.9.CO2",
    "ssp126" = "SSP1_2.6.CO2",
    "ssp245" = "SSP2_4.5.CO2",
    "ssp370" = "SSP3_7.0.CO2",
    "ssp585" = "SSP5_8.5.CO2"
  )

  if (!scenario %in% names(co2_files)) {
    warning("Scenario '", scenario, "' not recognized. Using default 'MaunaLoa.CO2'")
    return("MaunaLoa.CO2")
  }

  return(co2_files[[scenario]])
}


#' Internal: Generate Path for PRM Directories
#'
#' @description
#' Internal helper to generate directory paths formatted for .PRM files.
#' Handles both standalone mode (relative paths) and non-standalone mode (absolute paths).
#'
#' @param dir_name Character. Directory name (e.g., "CLIMATE", "CROP", "SOIL")
#' @param use_standalone Logical. Whether running in standalone mode (default: TRUE)
#' @param base_path Character. Base absolute path (required if use_standalone=FALSE)
#'
#' @return Character string with formatted path:
#'   - Standalone + Windows: ".\\DIRNAME\\"
#'   - Standalone + Non-Windows: "./DIRNAME/"
#'   - Non-standalone: absolute path to /DIRNAME/
#'
#' @keywords internal
#' @noRd
path_for_prm <- function(dir_name, use_standalone = TRUE, base_path = NULL) {
  # Validate dir_name
  if (!is.character(dir_name) || length(dir_name) != 1 || nchar(dir_name) == 0) {
    stop("dir_name must be a non-empty character string")
  }

  # Validate use_standalone
  if (!is.logical(use_standalone) || length(use_standalone) != 1) {
    stop("use_standalone must be a single logical value")
  }

  # Clean dir_name: remove any leading/trailing slashes
  dir_name <- sub("^[/\\\\]+", "", dir_name)  # Remove leading slashes
  dir_name <- sub("[/\\\\]+$", "", dir_name)  # Remove trailing slashes

  # Auto-detect OS
  is_windows <- .Platform$OS.type == "windows"

  # Generate path based on standalone mode
  if (use_standalone) {
    if (is_windows) {
      return(paste0(".\\", dir_name, "\\"))
    } else {
      return(paste0("./", dir_name, "/"))
    }
  } else {
    # Non-standalone: absolute path
    if (is.null(base_path) || nchar(base_path) == 0) {
      stop("base_path is required when use_standalone = FALSE")
    }

    # Normalize base_path (remove trailing separator if present)
    base_path <- sub("[/\\]+$", "", base_path)

    # Use forward slashes for consistency across platforms
    return(paste0(base_path, "/", dir_name, "/"))
  }
}
#' Check if AquaCrop Executable is Available
#'
#' Checks whether the AquaCrop executable for the current operating system
#' is available in the package installation.
#'
#' @return Logical; `TRUE` if executable is available, `FALSE` otherwise.
#'
#' @examples
#' if (has_aquacrop()) {
#'   message("AquaCrop is ready to use!")
#' } else {
#'   message("AquaCrop executable not found for this system.")
#' }
#'
#' @export
has_aquacrop <- function() {
  .has_aquacrop()
}

#' Get AquaCrop Executable Path
#'
#' Returns the full path to the AquaCrop executable for the current OS.
#'
#' @return Character string with the path to the executable, or `NULL` if not found.
#'
#' @examples
#' \dontrun{
#' exe_path <- get_aquacrop_path()
#' if (!is.null(exe_path)) {
#'   system(exe_path)
#' }
#' }
#'
#' @export
get_aquacrop_path <- function() {
  sysname <- Sys.info()["sysname"]

  exe_name <- switch(sysname,
    "Windows" = "aquacrop.exe",
    "Darwin" = "aquacrop_macos",
    "Linux" = "aquacrop_linux",
    stop("Unsupported operating system: ", sysname, call. = FALSE)
  )

  exe_path <- path_to_file(exe_name)

  if (is.null(exe_path) || !file.exists(exe_path) || !nzchar(exe_path)) {
    warning(
      "AquaCrop executable not found for ", sysname,
      call. = FALSE
    )
    return(NULL)
  }

  exe_path
}

#' Get AquaCrop Version
#'
#' Returns the version of the bundled AquaCrop executable.
#'
#' @return Character string with version information.
#'
#' @examples
#' \dontrun{
#' version <- get_aquacrop_version()
#' print(version)
#' }
#'
#' @export
get_aquacrop_version <- function() {
  tryCatch(
    {
      get("aquacrop_version", envir = asNamespace(.pkg_name()))
    },
    error = function(e) {
      "unknown"
    }
  )
}

#' Internal helper to check if AquaCrop executable is available
#' @noRd
#' @keywords internal
.has_aquacrop <- function() {
  exe_path <- tryCatch(
    {
      get_aquacrop_path()
    },
    error = function(e) {
      NULL
    }
  )

  !is.null(exe_path)
}


#' Get package name dynamically
#' @noRd
#' @keywords internal
.pkg_name <- function() {
  pkg <- tryCatch(
    utils::packageName(),
    error = function(e) "aquacroptools"
  )

  if (is.null(pkg)) pkg <- "aquacroptools"

  pkg
}

#' Get Aquacrop version string
aquacrop_version <- function() .aquacrop_state$version
