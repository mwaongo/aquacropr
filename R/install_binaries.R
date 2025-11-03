#' Install AquaCrop binaries
#'
#' @param version Version (NULL = latest, e.g. "7.1"). Minimum: 7.0
#' @param os OS ("windows", "linux", "macos", NULL = auto-detect)
#' @param path Install path (single directory, no subdirs)
#' @param force Force reinstall (default: FALSE)
#'
#' @return Path to executable
#' @export
install_binaries <- function(version = NULL, os = NULL, path, force = FALSE) {

  # Detect OS
  if (is.null(os)) {
    os <- if (.Platform$OS.type == "windows") "windows"
    else if (grepl("linux", tolower(Sys.info()["sysname"]))) "linux"
    else if (grepl("darwin", tolower(Sys.info()["sysname"]))) "macos"
    else stop("Unsupported OS")
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
    minor <- if (length(version_parts) > 1) as.numeric(version_parts[2])

    if (is.na(major) || major < 7) {
      stop("Minimum AquaCrop version is 7.0. Requested: ", version)
    }

    # Check if requested version > latest
    version_num <- as.numeric(paste0(version_parts, collapse = ""))
    latest_parts <- strsplit(latest_version, "\\.")[[1]]
    latest_num <- as.numeric(paste0(latest_parts, collapse = ""))

    if (version_num > latest_num) {
      message("Requested version (", version, ") is higher than latest (", latest_version, ")")
      message("Falling back to latest version: ", latest_version)
      version <- latest_version
      tag <- latest_tag
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
      return(invisible(exe_path))
    }
  }

  # Build download URL
  zip_name <- glue::glue("aquacrop-{version}-x86_64-{os}.zip")
  url <- glue::glue(
    "https://github.com/KUL-RSDA/AquaCrop/releases/download/{tag}/{zip_name}"
  )

  # Cache directory for downloads
  cache_dir <- rappdirs::user_cache_dir("aquacropr")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cached_zip <- file.path(cache_dir, zip_name)

  # Download or use cache
  if (file.exists(cached_zip) && !force) {
    message("Using cached download: ", basename(cached_zip))
  } else {
    message("Downloading AquaCrop ", version, " for ", os, "...")
    tryCatch({
      download.file(url, cached_zip, mode = "wb", quiet = TRUE)
      message("Downloaded: ", basename(cached_zip))
    }, error = function(e) {
      # If download fails, try latest as fallback
      if (version != latest_version) {
        message("Download failed for version ", version)
        message("Trying latest version (", latest_version, ") instead...")
        return(install_binaries(
          version = latest_version,
          os = os,
          path = path,
          force = force
        ))
      }
      stop("Download failed. Check version and OS.\nURL: ", url, "\nError: ", e$message)
    })
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
  invisible(exe_path)
}
