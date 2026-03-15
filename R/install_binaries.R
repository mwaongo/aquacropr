#' Install AquaCrop Binary
#'
#' Downloads and installs the AquaCrop executable for the current operating
#' system. Automatically detects OS, manages versioning with intelligent
#' fallbacks, and caches downloads to avoid repeated transfers.
#'
#' @param version Character string specifying the AquaCrop version to install.
#'   If NULL (default), installs the latest available version.
#'   Version must be >= 7.0. Accepts formats like "7.1", "v7.1", or "7.1.0".
#'   Use "dev" to compile from latest source code.
#'   If requested version is < 7.0 or > latest, falls back to latest version.
#' @param os Character string specifying the operating system: "windows",
#'   "linux", or "macos". If NULL (default), automatically detects current OS.
#' @param path Character string specifying the installation directory path
#'   where the AquaCrop executable will be installed. Current working directory by default.
#' @param force Logical. If TRUE, reinstalls even if executable already exists.
#'   Default: FALSE.
#' @param compiler Character. Fortran compiler for dev builds.
#'   Default: "gfortran". Only used when version = "dev".
#' @param keep_source Logical. Keep source code after compilation.
#'   Default: FALSE. Only used when version = "dev".
#'
#' @return Invisibly returns the installed version number as a character string.
#'
#' @examples
#' \dontrun{
#' # Install latest version
#' install_binaries(path = "~/aquacrop")
#'
#' # Install specific version
#' install_binaries(version = "7.1", path = "~/aquacrop")
#'
#' # Install dev version (compile from source)
#' install_binaries(version = "dev", path = "~/aquacrop")
#'
#' # Force reinstall
#' install_binaries(version = "7.1", path = "~/aquacrop", force = TRUE)
#' }
#'
#' @importFrom gh gh
#' @importFrom glue glue
#' @importFrom rappdirs user_cache_dir
#' @importFrom utils download.file unzip
#' @export
install_binaries <- function(
    version     = NULL,
    os          = NULL,
    path        = getwd(),
    force       = FALSE,
    compiler    = "gfortran",
    keep_source = FALSE
) {

  # Handle dev version (compile from source)
  if (!is.null(version) && tolower(version) == "dev") {
    message("Installing development version from source...")
    return(install_source(
      install_dir = path,
      compiler    = compiler,
      keep_source = keep_source,
      force       = force
    ))
  }

  # Detect OS
  if (is.null(os)) os <- get_os()
  os <- match.arg(tolower(os), choices = c("windows", "linux", "macos"))

  # TO DO: Version tag naming convention
  # Currently, patch releases use inconsistent tag formats (e.g. "7.3_typo")
  # which makes programmatic version resolution fragile.
  #
  # A cleaner convention would follow semantic versioning:
  #   - tag:      v7.3.1
  #   - archive:  aquacrop-7.3.1-x86_64-{os}.zip
  #
  # This would allow aquacropr to expose the latest patch release
  # automatically, with no manual workaround needed.
  #
  # Kindly requesting the AquaCrop development team to consider adopting
  # this convention for future releases.

  # Get latest version info from GitHub
  # latest_release <- tryCatch(
  #   gh::gh("/repos/KUL-RSDA/AquaCrop/releases/latest"),
  #   error = function(e) {
  #     stop(
  #       "Failed to fetch release info from GitHub.\n",
  #       "Check your internet connection or try again later.\n",
  #       "Error: ", conditionMessage(e),
  #       call. = FALSE
  #     )
  #   }
  # )
  # latest_tag     <- latest_release$tag_name
  # latest_version <- gsub("^v", "", latest_tag)

  # Determine version to install
  # if (is.null(version)) {
  #   version <- latest_version
  #   tag     <- latest_tag
  #   message("Installing latest version: ", version)
  #
  # } else {
  #   version       <- gsub("^v", "", as.character(version))
  #   version_parts <- strsplit(version, "\\.")[[1]]
  #   major         <- as.numeric(version_parts[1])
  #
  #   if (is.na(major) || major < 7) {
  #     message("Requested version (", version, ") is below minimum (7.0), ",
  #             "falling back to latest: ", latest_version)
  #     version <- latest_version
  #     tag     <- latest_tag
  #
  #   } else if (numeric_version(version) > numeric_version(latest_version)) {
  #     message("Requested version (", version, ") is above latest (",
  #             latest_version, "), falling back to latest")
  #     version <- latest_version
  #     tag     <- latest_tag
  #
  #   } else {
  #     tag <- .get_version_tag(version)
  #   }
  # }

  # Determine version to install (due to 7.3_typo error, fall back to 7.2)
  # the code below is a trick to contourn bad tag naming.
  # Once AquaCrop adopts a clean semantic versioning convention (see TO DO above),
  # replace MAX_VERSION with a GitHub API call to fetch the latest release tag.
  MAX_VERSION <- "7.2"

  if (is.null(version)) {
    version <- MAX_VERSION
    tag     <- .get_version_tag(version)
    message("Installing latest available version: ", version)
  } else {
    version <- gsub("^v", "", as.character(version))
    major   <- as.numeric(strsplit(version, "\\.")[[1]][1])
    if (is.na(major) || major < 7) {
      message("Requested version (", version, ") is below minimum (7.0), ",
              "falling back to ", MAX_VERSION)
      version <- MAX_VERSION
    } else if (numeric_version(version) > numeric_version(MAX_VERSION)) {
      message("Requested version (", version, ") is above maximum (", MAX_VERSION, "), ",
              "falling back to ", MAX_VERSION)
      version <- MAX_VERSION
    }
    tag <- .get_version_tag(version)
  }

  exe_name <- if (os == "windows") "aquacrop.exe" else "aquacrop"
  exe_path <- file.path(path, exe_name)

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
  url      <- glue::glue(
    "https://github.com/KUL-RSDA/AquaCrop/releases/download/{tag}/{zip_name}"
  )

  # Cache
  cache_dir  <- rappdirs::user_cache_dir("aquacropr")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cached_zip <- file.path(cache_dir, zip_name)

  use_cache <- FALSE
  if (file.exists(cached_zip) && !force) {
    is_valid <- tryCatch(
      { nrow(utils::unzip(cached_zip, list = TRUE)) > 0 },
      error = function(e) FALSE
    )
    if (is_valid) {
      message("Using cached download: ", basename(cached_zip))
      use_cache <- TRUE
    } else {
      message("Cached file corrupted, re-downloading...")
      unlink(cached_zip)
    }
  }

  if (!use_cache) {
    message("Downloading AquaCrop ", version, " for ", os, "...")
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = 300)

    tryCatch(
      {
        utils::download.file(url, cached_zip, mode = "wb", quiet = TRUE)
        message("Downloaded: ", basename(cached_zip))
      },
      error = function(e) {
        stop("Download failed.\nURL: ", url, "\nError: ", e$message, call. = FALSE)
      }
    )
  }

  # Extract
  message("Extracting...")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(cached_zip, exdir = path)

  # Locate executable
  files       <- list.files(path, recursive = TRUE, full.names = TRUE)
  exe_pattern <- if (os == "windows") "aquacrop\\.exe$" else "/aquacrop$"
  exe_found   <- grep(exe_pattern, files, value = TRUE, ignore.case = TRUE)

  if (length(exe_found) == 0) {
    stop("Executable not found after extraction.", call. = FALSE)
  }

  if (exe_found[1] != exe_path) {
    extracted_dir <- file.path(path, glue::glue("aquacrop-{version}-x86_64-{os}"))
    file.rename(exe_found[1], exe_path)
    if (dir.exists(extracted_dir)) unlink(extracted_dir, recursive = TRUE)
  }

  if (os != "windows") Sys.chmod(exe_path, mode = "0755")

  message("Installed: ", exe_path)
  invisible(version)
}
