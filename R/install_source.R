#' Detect Fortran Compiler via R CMD config FC
#'
#' Calls R CMD config FC to retrieve the Fortran compiler R itself was built
#' with. This is the canonical cross-platform approach: it reads directly from
#' R own Makeconf and requires no manual PATH handling.
#'
#' @return Character. Full path or name of the detected Fortran compiler.
#'   Stops with an informative message if none is found.
#'
#' @noRd
.detect_fc <- function() {
  compiler <- tryCatch(
    system2("R", args = c("CMD", "config", "FC"), stdout = TRUE, stderr = FALSE),
    error = function(e) ""
  )
  compiler <- compiler[nzchar(compiler)][1]

  if (is.na(compiler) || !nzchar(compiler)) {
    stop(
      "Could not detect a Fortran compiler via 'R CMD config FC'.\n",
      "Please specify one explicitly, e.g.: compiler = 'gfortran'",
      call. = FALSE
    )
  }

  compiler
}


#' Find GNU Make Executable
#'
#' Locates the make executable, accounting for platform differences.
#' On Windows, also checks for mingw32-make provided by Rtools.
#'
#' @return Character. Full path to the make executable.
#'   Stops with an informative message if not found.
#'
#' @noRd
find_make <- function() {
  os <- get_os()

  if (os == "windows") {
    candidates <- Sys.which(c("make", "mingw32-make"))
    make       <- candidates[nzchar(candidates)][1]
  } else {
    make <- Sys.which("make")
  }

  if (is.na(make) || !nzchar(make)) {
    stop(
      "'make' not found. Please install build tools",
      if (os == "windows") " (Rtools)" else "",
      ".",
      call. = FALSE
    )
  }

  make
}


#' Check System Dependencies for Building AquaCrop
#'
#' Verifies that required build tools (make and a Fortran compiler) are
#' available on the system before attempting to compile AquaCrop. The
#' Fortran compiler is auto-detected via R CMD config FC, which reads the
#' same compiler R itself was built with.
#'
#' Calling this function once and reusing its return value avoids redundant
#' calls to R CMD config FC across the install workflow.
#'
#' @return Invisibly returns a named list with two elements:
#'   \describe{
#'     \item{compiler}{Character. Detected Fortran compiler (full path or name).}
#'     \item{make}{Character. Full path to the make executable.}
#'   }
#'   Stops with an informative message if any dependency is missing.
#'
#' @noRd
check_sys_deps <- function() {

  deps_ok  <- TRUE
  make_out <- ""
  fc_out   <- ""

  # Check make
  make_out <- tryCatch(find_make(), error = function(e) "")
  if (!nzchar(make_out)) {
    message("error: GNU Make not found")
    deps_ok <- FALSE
  } else {
    make_version <- tryCatch(
      system2("make", "--version", stdout = TRUE, stderr = FALSE)[1],
      error = function(e) "unknown"
    )
    message("make: ", make_out)
    if (make_version != "unknown") message("  ", make_version)
  }

  # Detect Fortran compiler via R CMD config FC
  fc_out <- tryCatch(.detect_fc(), error = function(e) "")
  if (!nzchar(fc_out)) {
    message("error: no Fortran compiler detected via R CMD config FC")
    deps_ok <- FALSE
  } else {
    compiler_bin <- basename(fc_out)
    fc_version <- tryCatch(
      system2(compiler_bin, "--version", stdout = TRUE, stderr = FALSE)[1],
      error = function(e) "unknown"
    )
    message("ok: ", compiler_bin, ": ", Sys.which(compiler_bin))
    if (fc_version != "unknown") message("  ", fc_version)
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
      "  Install Rtools (includes make and gfortran)",
      call. = FALSE
    )
  }

  invisible(list(compiler = fc_out, make = make_out))
}


#' Download AquaCrop Source Code
#'
#' Downloads and extracts AquaCrop source code from the GitHub main branch.
#'
#' @param dest_dir Character. Directory where source will be extracted.
#'   Default: a temporary directory.
#' @param url Character. URL to download source code.
#'   Default: official GitHub repository main branch.
#' @param timeout Integer. Download timeout in seconds. Default: 120.
#'
#' @return Character. Path to the extracted AquaCrop source directory.
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
    url      = "https://github.com/KUL-RSDA/AquaCrop/archive/refs/heads/main.zip",
    timeout  = 120
) {

  fs::dir_create(dest_dir, recurse = TRUE)
  zip_file <- fs::path(dest_dir, "aquacrop_main.zip")

  # Apply timeout and restore on exit
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  message("Downloading from: ", url)
  tryCatch(
    utils::download.file(url, zip_file, mode = "wb", quiet = FALSE),
    error = function(e) {
      stop("Failed to download AquaCrop source: ", e$message, call. = FALSE)
    }
  )

  message("Extracting...")
  utils::unzip(zip_file, exdir = dest_dir)

  # GitHub appends -main suffix to the extracted folder
  extracted_name <- fs::path(dest_dir, "AquaCrop-main")
  final_dir      <- fs::path(dest_dir, "AquaCrop")

  if (fs::dir_exists(extracted_name)) {
    if (fs::dir_exists(final_dir)) fs::dir_delete(final_dir)
    fs::file_move(extracted_name, final_dir)
  }

  fs::file_delete(zip_file)

  if (!fs::dir_exists(final_dir)) {
    stop("Failed to locate extracted source directory.", call. = FALSE)
  }

  final_dir
}


#' Build AquaCrop from Source
#'
#' Compiles the AquaCrop executable from source code using make and a
#' Fortran compiler. Accepts a pre-resolved compiler to avoid redundant
#' calls to R CMD config FC when called from install_source.
#'
#' @param source_dir Character. Path to the AquaCrop source directory.
#' @param compiler Character or NULL. Fortran compiler binary name or full
#'   path. If NULL (default), auto-detected via R CMD config FC.
#' @param target Character. Build target passed to make. One of "bin"
#'   (default), "lib", or "all".
#' @param verbose Logical. If TRUE (default), prints build information.
#'
#' @return Character. Path to the compiled executable.
#'
#' @importFrom withr with_dir
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
    compiler = NULL,
    target   = "bin",
    verbose  = TRUE
) {

  if (!target %in% c("bin", "lib", "all")) {
    stop("target must be 'bin', 'lib', or 'all'.", call. = FALSE)
  }

  source_dir <- fs::path_expand(source_dir)
  src_dir    <- fs::path(source_dir, "src")

  if (!fs::dir_exists(src_dir)) {
    stop("Source directory not found: ", src_dir, call. = FALSE)
  }

  os   <- get_os()
  make <- find_make()

  # Use provided compiler or detect once
  if (is.null(compiler)) {
    compiler <- .detect_fc()
  }

  # basename handles full paths (e.g., /usr/local/bin/gfortran-13)
  # Sys.which validates availability; full path is passed to FC= in make
  compiler_bin <- basename(compiler)
  if (!nzchar(Sys.which(compiler_bin))) {
    stop("Fortran compiler not found: ", compiler_bin, call. = FALSE)
  }

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
      "  directory : ", src_dir,  "\n",
      "  make      : ", make,     "\n",
      "  compiler  : ", compiler, "\n",
      "  target    : ", target
    )
  }

  result <- withr::with_dir(src_dir, {
    system2(
      command = make,
      args    = make_args,
      stdout  = TRUE,
      stderr  = TRUE
    )
  })

  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "Build failed with exit code ", status, ":\n",
      paste(result, collapse = "\n"),
      call. = FALSE
    )
  }

  exe_name <- if (os == "windows") "aquacrop.exe" else "aquacrop"
  exe_path <- fs::path(src_dir, exe_name)

  if (!fs::file_exists(exe_path)) {
    stop(
      "Build succeeded but executable not found:\n  ", exe_path,
      call. = FALSE
    )
  }

  if (verbose) message("Build successful: ", exe_path)

  exe_path
}


#' Install AquaCrop from Source
#'
#' Complete workflow to download, compile, and install the AquaCrop executable
#' from source. Always compiles from the latest development version (main
#' branch). This function is called internally by
#' install_binaries(version = "dev").
#'
#' @param dest_dir Character. Destination directory for the AquaCrop
#'   executable.
#' @param compiler Character or NULL. Fortran compiler to use. If NULL
#'   (default), auto-detected once via R CMD config FC and reused across
#'   all steps. Can be set explicitly (e.g., "gfortran", "ifort", or a
#'   full path such as "/usr/local/bin/gfortran-13").
#' @param keep_source Logical. If TRUE, keeps source code after compilation.
#'   Default: FALSE.
#' @param force Logical. If TRUE, overwrites existing installation.
#'   Default: FALSE.
#'
#' @details
#' System requirements on Linux and macOS: GNU Make (>= 3.82) and a Fortran
#' compiler (gfortran >= 6.4.0 or ifort >= 18.0.1). On Windows, installing
#' Rtools is sufficient as it provides both make (mingw32-make) and gfortran.
#'
#' The function first checks for required build tools and detects the Fortran
#' compiler once via R CMD config FC, then downloads the latest source code
#' from the GitHub main branch, compiles the executable, installs the binary
#' to the destination directory, and cleans up source files unless
#' keep_source is TRUE.
#'
#' @return Invisibly returns "dev" as the version string.
#'
#' @examples
#' \dontrun{
#' # Usually called via install_binaries
#' install_binaries(version = "dev", path = "~/aquacrop")
#'
#' # Direct call with auto-detected compiler
#' install_source(dest_dir = "~/aquacrop")
#'
#' # Keep source code after compilation
#' install_source(dest_dir = "~/aquacrop", keep_source = TRUE)
#'
#' # Force reinstall with explicit compiler
#' install_source(dest_dir = "~/aquacrop", compiler = "gfortran", force = TRUE)
#' }
#'
#' @export
install_source <- function(
    dest_dir,
    compiler    = NULL,
    keep_source = FALSE,
    force       = FALSE
) {

  dest_dir  <- fs::path_expand(dest_dir)
  fs::dir_create(dest_dir, recurse = TRUE)

  exe_name  <- if (.Platform$OS.type == "windows") "aquacrop.exe" else "aquacrop"
  final_exe <- fs::path(dest_dir, exe_name)

  if (fs::file_exists(final_exe) && !force) {
    message("AquaCrop already installed at: ", final_exe)
    message("Use force = TRUE to reinstall.")
    return(invisible("dev"))
  }

  message("=== Installing AquaCrop from Source (Development Version) ===\n")

  # Step 1: check build tools and detect compiler once.
  # check_sys_deps() returns list(compiler, make) so subsequent steps
  # reuse the detected values without calling R CMD config FC again.
  message("Step 1/4: Checking system dependencies...")
  deps <- check_sys_deps()
  message("All dependencies satisfied\n")

  # Use user-provided compiler if given, otherwise reuse what check_sys_deps found
  if (is.null(compiler)) {
    compiler <- deps$compiler
    message("  Fortran compiler: ", compiler)
  }

  message("Step 2/4: Downloading source code from main branch...")
  temp_dir   <- fs::path_temp("aquacrop_build")
  source_dir <- download_source(dest_dir = temp_dir)
  message("Source downloaded\n")

  # Pass resolved compiler directly: build_source will not call R CMD config FC again
  message("Step 3/4: Compiling AquaCrop...")
  exe_path <- build_source(source_dir = source_dir, compiler = compiler)
  message("Compilation successful\n")

  message("Step 4/4: Installing binary...")
  if (fs::file_exists(exe_path)) {
    fs::file_copy(exe_path, final_exe, overwrite = TRUE)
    # file_chmod is Unix only; Windows manages permissions differently
    if (.Platform$OS.type != "windows") {
      fs::file_chmod(final_exe, "755")
    }
    message("Binary installed to: ", final_exe)
  } else {
    stop("Compilation succeeded but executable not found.", call. = FALSE)
  }

  if (!keep_source) {
    message("Cleaning up source files...")
    fs::dir_delete(temp_dir)
    message("Cleanup complete")
  } else {
    message("Source code kept at: ", source_dir)
  }

  message("\n=== Installation Complete ===")
  message("AquaCrop version: dev (latest from main branch)")
  message("Executable: ", final_exe)

  invisible("dev")
}