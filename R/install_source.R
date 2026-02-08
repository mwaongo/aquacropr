#' Install AquaCrop from Source
#'
#' @description
#' Complete workflow to download, compile, and install AquaCrop executable from source.
#' Always compiles from the latest development version (main branch).
#' This function is called by install_binaries(version = "dev").
#'
#' @param dest_dir Character. Destination directory for the AquaCrop executable.
#' @param compiler Character. Fortran compiler to use. Options: "gfortran" (default), "ifort".
#' @param keep_source Logical. If TRUE, keeps source code after compilation. Default: FALSE.
#' @param force Logical. If TRUE, overwrites existing installation. Default: FALSE.
#'
#' @return Invisibly returns "dev" as the version string.
#'
#' @details
#' ## System Requirements
#'
#' **Linux/macOS:**
#' - GNU Make (>= 3.82)
#' - GNU Fortran (gfortran >= 6.4.0) or Intel Fortran (ifort >= 18.0.1)
#'
#' **Windows:**
#' - MinGW with gfortran
#' - GNU Make
#'
#' ## Installation Steps
#'
#' 1. Checks for required build tools (make, compiler)
#' 2. Downloads latest source code from GitHub main branch
#' 3. Compiles AquaCrop executable
#' 4. Installs binary to destination directory
#' 5. Cleans up source files (unless keep_source = TRUE)
#'
#' @examples
#' \dontrun{
#' # Usually called via install_binaries(version = "dev")
#' install_binaries(version = "dev", path = "~/aquacrop")
#'
#' # Direct call (advanced users)
#' install_source(dest_dir = "~/aquacrop")
#'
#' # Keep source code for development
#' install_source(dest_dir = "~/aquacrop", keep_source = TRUE)
#'
#' # Use Intel Fortran compiler
#' install_source(dest_dir = "~/aquacrop", compiler = "ifort")
#' }
#'
#' @export
install_source <- function(
    dest_dir,
    compiler = "gfortran",
    keep_source = FALSE,
    force = FALSE) {

  dest_dir <- fs::path_expand(dest_dir)
  fs::dir_create(dest_dir, recurse = TRUE)

  # Check if already installed
  exe_name <- if (.Platform$OS.type == "windows") "aquacrop.exe" else "aquacrop"
  final_exe <- fs::path(dest_dir, exe_name)

  if (fs::file_exists(final_exe) && !force) {
    message("AquaCrop already installed at: ", final_exe)
    message("Use force = TRUE to reinstall.")
    return(invisible("dev"))
  }

  message("=== Installing AquaCrop from Source (Development Version) ===\n")

  # Step 1: Check dependencies
  message("Step 1/4: Checking system dependencies...")
  check_sys_deps(compiler = compiler)
  message("✓ All dependencies satisfied\n")

  # Step 2: Download source
  message("Step 2/4: Downloading source code from main branch...")
  temp_dir <- fs::path_temp("aquacrop_build")
  source_dir <- download_source(dest_dir = temp_dir)
  message("✓ Source downloaded\n")

  # Step 3: Build
  message("Step 3/4: Compiling AquaCrop...")
  exe_path <- build_source(
    source_dir = source_dir,
    compiler = compiler
  )
  message("✓ Compilation successful\n")

  # Step 4: Install and clean
  message("Step 4/4: Installing binary...")

  if (fs::file_exists(exe_path)) {
    fs::file_copy(exe_path, final_exe, overwrite = TRUE)
    fs::file_chmod(final_exe, "755")
    message("✓ Binary installed to: ", final_exe)
  } else {
    stop("Compilation succeeded but executable not found", call. = FALSE)
  }

  # Clean up
  if (!keep_source) {
    message("Cleaning up source files...")
    fs::dir_delete(temp_dir)
    message("✓ Cleanup complete")
  } else {
    message("Source code kept at: ", source_dir)
  }

  message("\n=== Installation Complete ===")
  message("AquaCrop version: dev (latest from main branch)")
  message("Executable: ", final_exe)

  invisible("dev")
}


#' Check System Dependencies for Building AquaCrop
#'
#' @description
#' Verifies that required build tools (make, Fortran compiler) are available.
#'
#' @param compiler Character. Fortran compiler to check. Default: "gfortran".
#'
#' @return Invisibly returns TRUE if all dependencies are satisfied.
#'
#' @examples
#' \dontrun{
#' check_sys_deps()
#' check_sys_deps(compiler = "ifort")
#' }
#'
#' @export
check_sys_deps <- function(compiler = "gfortran") {

  if (!compiler %in% c("gfortran", "ifort")) {
    stop("compiler must be 'gfortran' or 'ifort'", call. = FALSE)
  }

  deps_ok <- TRUE

  # Check make
  make_path <- Sys.which("make")
  if (make_path == "") {
    message("✗ GNU Make not found")
    deps_ok <- FALSE
  } else {
    make_version <- tryCatch(
      system2("make", "--version", stdout = TRUE, stderr = FALSE)[1],
      error = function(e) "unknown"
    )
    message("✓ make: ", make_path)
    if (make_version != "unknown") {
      message("  ", make_version)
    }
  }

  # Check compiler
  fc_path <- Sys.which(compiler)
  if (fc_path == "") {
    message("✗ ", compiler, " not found")
    deps_ok <- FALSE
  } else {
    fc_version <- tryCatch(
      system2(compiler, "--version", stdout = TRUE, stderr = FALSE)[1],
      error = function(e) "unknown"
    )
    message("✓ ", compiler, ": ", fc_path)
    if (fc_version != "unknown") {
      message("  ", fc_version)
    }
  }

  if (!deps_ok) {
    stop(
      "\nMissing required dependencies. Please install:\n\n",
      "Ubuntu/Debian:\n",
      "  sudo apt-get install make gfortran\n\n",
      "Fedora/RHEL:\n",
      "  sudo dnf install make gcc-gfortran\n\n",
      "macOS:\n",
      "  brew install make gcc\n\n",
      "Windows:\n",
      "  Install MinGW-w64 with gfortran support",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Download AquaCrop Source Code
#'
#' @description
#' Downloads and extracts AquaCrop source code from GitHub main branch.
#'
#' @param dest_dir Character. Directory where source will be extracted.
#'   Default: temporary directory.
#' @param url Character. URL to download source code.
#'   Default: official GitHub repository main branch.
#'
#' @return Path to extracted AquaCrop source directory.
#'
#' @examples
#' \dontrun{
#' source_dir <- download_source()
#' source_dir <- download_source(dest_dir = "~/builds")
#' }
#'
#' @export
download_source <- function(
    dest_dir = fs::path_temp("aquacrop_source"),
    url = "https://github.com/KUL-RSDA/AquaCrop/archive/refs/heads/main.zip") {

  fs::dir_create(dest_dir, recurse = TRUE)

  zip_file <- fs::path(dest_dir, "aquacrop_main.zip")

  message("Downloading from: ", url)
  tryCatch({
    utils::download.file(url, zip_file, mode = "wb", quiet = FALSE)
  }, error = function(e) {
    stop("Failed to download AquaCrop source: ", e$message, call. = FALSE)
  })

  message("Extracting...")
  utils::unzip(zip_file, exdir = dest_dir)

  # GitHub adds -main suffix
  extracted_name <- fs::path(dest_dir, "AquaCrop-main")
  final_dir <- fs::path(dest_dir, "AquaCrop")

  if (fs::dir_exists(extracted_name)) {
    if (fs::dir_exists(final_dir)) fs::dir_delete(final_dir)
    fs::file_move(extracted_name, final_dir)
  }

  fs::file_delete(zip_file)

  if (!fs::dir_exists(final_dir)) {
    stop("Failed to locate extracted source directory", call. = FALSE)
  }

  return(final_dir)
}

find_make <- function() {
  os <- get_os()
  if (os == "windows") {
    make <- Sys.which(c("make", "mingw32-make"))
    make <- make[nzchar(make)][1]
  } else {
    make <- Sys.which("make")
  }

  if (is.na(make) || make == "") {
    stop(
      "`make` not found. Please install build tools ",
      if (os == "windows") "(Rtools)" else "",
      call. = FALSE
    )
  }

  make
}


#' Build AquaCrop from Source
#'
#' @description
#' Compiles AquaCrop executable from source code using make and Fortran compiler.
#'
#' @param source_dir Character. Path to AquaCrop source directory.
#' @param compiler Character. Fortran compiler. Default: "gfortran".
#' @param target Character. Build target: "bin" (default), "lib", or "all".
#'
#' @return Path to compiled executable.
#'
#' @examples
#' \dontrun{
#' exe_path <- build_source("AquaCrop")
#' exe_path <- build_source("AquaCrop", compiler = "ifort")
#' }
#'
#' @export
build_source <- function(
    source_dir,
    compiler = "gfortran",
    target = "bin",
    verbose = TRUE
) {

  # ---- validation ----
  if (!target %in% c("bin", "lib", "all")) {
    stop("target must be 'bin', 'lib', or 'all'", call. = FALSE)
  }

  source_dir <- fs::path_expand(source_dir)
  src_dir    <- fs::path(source_dir, "src")

  if (!fs::dir_exists(src_dir)) {
    stop("Source directory not found: ", src_dir, call. = FALSE)
  }

  os   <- get_os()
  make <- find_make(os)

  # ---- check compiler ----
  fc <- Sys.which(compiler)
  if (fc == "") {
    stop("Fortran compiler not found: ", compiler, call. = FALSE)
  }

  # ---- make arguments ----
  make_args <- c(
    target,
    paste0("FC=", compiler)
  )

  if (os == "windows") {
    make_args <- c(make_args, "CPPFLAGS=-D_WINDOWS")
  }

  if (verbose) {
    message(
      "Building AquaCrop (", os, ")\n",
      "  directory : ", src_dir, "\n",
      "  make      : ", make, "\n",
      "  compiler  : ", compiler, "\n",
      "  target    : ", target
    )
  }

  # ---- run build ----
  result <- system2(
    command = make,
    args    = make_args,
    stdout = TRUE,
    stderr = TRUE,
    wd     = src_dir
  )

  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "Build failed with exit code ", status, ":\n",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }

  # ---- locate executable ----
  exe_name <- if (os == "windows") "aquacrop.exe" else "aquacrop"
  exe_path <- fs::path(src_dir, exe_name)

  if (!fs::file_exists(exe_path)) {
    stop(
      "Build succeeded but executable not found:\n  ",
      exe_path,
      call. = FALSE
    )
  }

  if (verbose) {
    message("Build successful: ", exe_path)
  }

  exe_path
}
