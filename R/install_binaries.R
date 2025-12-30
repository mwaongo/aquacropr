#' Install AquaCrop Binary
#'
#' Downloads and installs the AquaCrop executable for the current operating system.
#' Automatically detects OS, manages versioning with intelligent fallbacks, and caches
#' downloads to avoid repeated transfers.
#'
#' @param version Character string specifying the AquaCrop version to install.
#'   If \code{NULL} (default), installs the latest available version.
#'   Version must be >= 7.0. Accepts formats like "7.1", "v7.1", or "7.1.0".
#'   If requested version is < 7.0 or > latest, falls back to latest version.
#' @param os Character string specifying the operating system: "windows", "linux",
#'   or "macos". If \code{NULL} (default), automatically detects the current OS.
#' @param path Character string specifying the installation directory path where
#'   the AquaCrop executable will be installed.
#' @param force Logical. If \code{TRUE}, reinstalls even if executable already exists.
#'   If \code{FALSE} (default), skips installation if executable is present.
#'
#' @return Invisibly returns the installed version number as a character string.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Detects the operating system if not specified
#'   \item Queries GitHub for the latest AquaCrop release
#'   \item Validates the requested version (minimum 7.0)
#'   \item Downloads the binary from GitHub releases (with 5-minute timeout)
#'   \item Validates cached downloads to detect corruption
#'   \item Extracts and installs the executable
#'   \item Sets appropriate permissions on Unix systems
#' }
#'
#' Fallback behavior:
#' \itemize{
#'   \item Version < 7.0 → falls back to latest
#'   \item Version > latest → falls back to latest
#'   \item Download failure → retries with latest version
#' }
#'
#' @examples
#' \dontrun{
#' # Install latest version
#' install_binaries(path = "~/aquacrop")
#'
#' # Install specific version
#' install_binaries(version = "7.1", path = "~/aquacrop")
#'
#' # Force reinstall
#' install_binaries(version = "7.1", path = "~/aquacrop", force = TRUE)
#'
#' # Specify OS explicitly
#' install_binaries(version = "7.1", os = "linux", path = "~/aquacrop")
#' }
#'
#' @importFrom gh gh
#' @importFrom glue glue
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils download.file unzip
#'
#' @export
install_binaries <- function(version = NULL, os = NULL, path, force = FALSE) {
  # Detect OS
  if (is.null(os)) {
    os <- if (.Platform$OS.type == "windows") {
      "windows"
    } else if (grepl("linux", tolower(Sys.info()["sysname"]))) {
      "linux"
    } else if (grepl("darwin", tolower(Sys.info()["sysname"]))) {
      "macos"
    } else {
      stop("Unsupported OS")
    }
  }

  # Get latest version info from GitHub
  latest_release <- gh::gh("/repos/KUL-RSDA/AquaCrop/releases/latest")
  latest_tag <- latest_release$tag_name
  latest_version <- gsub("^v", "", latest_tag)

  # Determine version to install
  if (is.null(version)) {
    # Use latest
    version <- latest_version
    tag <- latest_tag
    message("Installing latest version: ", version)
  } else {
    # Clean version
    version <- gsub("^v", "", as.character(version))
    tag <- paste0("v", version)

    # Validate version >= 7.0
    version_parts <- strsplit(version, "\\.")[[1]]
    major <- as.numeric(version_parts[1])

    if (is.na(major) || major < 7) {
      message("Requested version (", version, ") is below minimum supported version (7.0)")
      message("Falling back to latest version: ", latest_version)
      version <- latest_version
      tag <- latest_tag
    } else {
      # Check if requested version > latest
      if (numeric_version(version) > numeric_version(latest_version)) {
        message("Requested version (", version, ") is higher than latest (", latest_version, ")")
        message("Falling back to latest version: ", latest_version)
        version <- latest_version
        tag <- latest_tag
      }
    }
  }

  # Executable name
  exe_name <- if (os == "windows") "aquacrop.exe" else "aquacrop"
  exe_path <- file.path(path, exe_name)

  # Check if exists
  if (file.exists(exe_path)) {
    if (force) {
      message("Removing existing binary...")
      unlink(exe_path)
    } else {
      message("Binary already exists: ", exe_path)
      message("Use force = TRUE to reinstall")
      return(invisible(version))
    }
  }

  # Build download URL
  zip_name <- glue::glue("aquacrop-{version}-x86_64-{os}.zip")
  url <- glue::glue(
    "https://github.com/KUL-RSDA/AquaCrop/releases/download/{tag}/{zip_name}"
  )

  # Cache directory for downloads
  cache_dir <- rappdirs::user_cache_dir("aquacroptools")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cached_zip <- file.path(cache_dir, zip_name)

  # Download or use cache
  use_cache <- FALSE
  if (file.exists(cached_zip) && !force) {
    # Validate cached file
    is_valid_cache <- tryCatch(
      {
        zip_files <- utils::unzip(cached_zip, list = TRUE)
        nrow(zip_files) > 0
      },
      error = function(e) FALSE
    )

    if (is_valid_cache) {
      message("Using cached download: ", basename(cached_zip))
      use_cache <- TRUE
    } else {
      message("Cached file is corrupted, re-downloading...")
      unlink(cached_zip)
    }
  }

  if (!use_cache) {
    message("Downloading AquaCrop ", version, " for ", os, "...")

    # Set timeout (default is 60 seconds, increase to 300 for large files)
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = 300)

    download_success <- tryCatch(
      {
        download.file(url, cached_zip, mode = "wb", quiet = TRUE)
        message("Downloaded: ", basename(cached_zip))
        TRUE
      },
      error = function(e) {
        # If download fails, try latest as fallback
        if (version != latest_version) {
          message("Download failed for version ", version)
          message("Trying latest version (", latest_version, ") instead...")
          return(FALSE)
        }
        stop("Download failed. Check version and OS.\nURL: ", url, "\nError: ", e$message)
      }
    )

    # If download failed and we need to retry with latest version
    if (!download_success) {
      return(install_binaries(
        version = latest_version,
        os = os,
        path = path,
        force = force
      ))
    }
  }

  # Extract to path
  message("Extracting...")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(cached_zip, exdir = path)

  # Find executable
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  exe_pattern <- if (os == "windows") "\\.exe$" else "aquacrop"
  exe_found <- grep(exe_pattern, files, value = TRUE, ignore.case = TRUE)

  if (length(exe_found) == 0) {
    stop("Executable not found after extraction")
  }

  # Move to root of path if nested
  if (exe_found[1] != exe_path) {
    file.rename(exe_found[1], exe_path)
    # Clean up nested dirs
    subdirs <- list.dirs(path, recursive = FALSE)
    if (length(subdirs) > 0) unlink(subdirs, recursive = TRUE)
  }

  # Set permissions (Unix)
  if (os != "windows") Sys.chmod(exe_path, mode = "0755")

  message("Installed: ", exe_path)
  invisible(version)
}
