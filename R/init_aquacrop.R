#' Initialize AquaCrop Project Structure
#'
#' Creates a directory structure for AquaCrop crop water productivity model
#' simulations and installs the AquaCrop binary. After initialization,
#' you can run simulations directly in the project directory.
#'
#' @param path Character string specifying the root directory where the project
#'   should be created. Default is the current working directory (`"."`).
#' @param version AquaCrop version to install (NULL = latest, e.g. "7.1")
#' @param os Operating system ("windows", "linux", "macos", NULL = auto-detect)
#' @param force Force reinstall binary even if exists (default: FALSE)
#' @param overwrite Logical; if `TRUE`, overwrites existing directory structure.
#'   Default: `FALSE`.
#'
#' @return Invisibly returns the normalized path to the created directory.
#'
#' @details
#' The function creates the following directory structure:
#' \itemize{
#'   \item \code{CLIMATE/} - Climate input files (temperature, rainfall, ETo)
#'   \item \code{CROP/} - Crop parameter files
#'   \item \code{LIST/} - Project and simulation list files
#'   \item \code{MANAGEMENT/} - Field management practice files
#'   \item \code{OUTP/} - Simulation output files
#'   \item \code{PARAM/} - General parameter files
#'   \item \code{SIMUL/} - Simulation configuration files
#'   \item \code{SOIL/} - Soil profile and characteristic files
#' }
#'
#' The AquaCrop binary is automatically downloaded and installed in the project
#' directory. The appropriate executable for your operating system (Windows,
#' macOS, or Linux) is placed directly in the root of the project.
#'
#' @examples
#' \dontrun{
#' # Initialize new project
#' init_aquacrop("~/my-aquacrop-project")
#'
#' # Then work in that directory
#' setwd("~/my-aquacrop-project")
#' run_aquacrop()
#'
#' # Initialize with specific version
#' init_aquacrop("~/project", version = "7.1")
#'
#' # Overwrite existing structure
#' init_aquacrop("~/project", overwrite = TRUE, force = TRUE)
#' }
#'
#' @seealso
#' \code{\link{install_binaries}}, \code{\link{run_aquacrop}}
#'
#' @references
#' \url{https://www.fao.org/aquacrop/} for AquaCrop documentation
#'
#' @export
init_aquacrop <- function(path = ".",
                          version = NULL,
                          os = NULL,
                          force = FALSE,
                          overwrite = FALSE) {

  # Input validation
  stopifnot(
    "path must be a character string" = is.character(path) && length(path) == 1,
    "force must be logical" = is.logical(force) && length(force) == 1,
    "overwrite must be logical" = is.logical(overwrite) && length(overwrite) == 1
  )

  # Expand path (handle ~, relative paths, etc.)
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  # Check if directory exists
  if (dir.exists(path) && !overwrite) {
    # Check if it already has AquaCrop structure
    aquacrop_folders <- c(
      "CLIMATE", "CROP", "SOIL", "SIMUL", "OUTP",
      "MANAGEMENT", "PARAM", "LIST"
    )

    has_structure <- all(dir.exists(file.path(path, aquacrop_folders)))

    if (has_structure) {
      message("AquaCrop project structure already exists at: ", path)
      message("Use overwrite = TRUE to recreate directories")

      # Check if binary exists
      exe_name <- if (.Platform$OS.type == "windows") "aquacrop.exe" else "aquacrop"
      has_binary <- file.exists(file.path(path, exe_name))

      if (!has_binary) {
        message("\nBinary not found. Installing...")
        install_binaries(version = version, os = os, path = path, force = force)
      } else if (force) {
        message("\nReinstalling binary (force = TRUE)...")
        install_binaries(version = version, os = os, path = path, force = force)
      }

      message("\n", cli::symbol$info, " To work in this project, run:")
      message("  setwd(\"", path, "\")")
      message("  run_aquacrop()")

      return(invisible(path))
    } else {
      stop(
        "Directory '", path, "' already exists but is not an AquaCrop project.\n",
        "Use overwrite = TRUE to recreate, or choose a different path.",
        call. = FALSE
      )
    }
  }

  # Define directory structure
  dirs <- c(
    "CLIMATE", "CROP", "LIST", "MANAGEMENT",
    "OUTP", "PARAM", "SIMUL", "SOIL"
  )

  message(cli::symbol$arrow_right, " Creating AquaCrop project structure...")

  # Create root folder
  if (!dir.exists(path)) {
    tryCatch({
      dir.create(path, recursive = TRUE)
    }, error = function(e) {
      stop(
        "Failed to create directory: ", path, "\n",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
    })
  }

  # Install AquaCrop binary
  message("\n", cli::symbol$arrow_right, " Installing AquaCrop binary...")
  tryCatch({
    install_binaries(version = version, os = os, path = path, force = force)
  }, error = function(e) {
    warning(
      "Failed to install AquaCrop binary: ", conditionMessage(e), "\n",
      "You can install it manually later with: install_binaries(path = \"", path, "\")",
      call. = FALSE
    )
  })
  # Create subdirectories
  for (d in dirs) {
    subdir_path <- file.path(path, d)
    tryCatch({
      dir.create(subdir_path, showWarnings = FALSE, recursive = TRUE)
    }, error = function(e) {
      warning(
        "Failed to create subdirectory: ", d, "\n",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
    })
  }

  message(cli::symbol$tick, " Created directories: ", paste(dirs, collapse = ", "))

  .install_templates(path, overwrite = TRUE)



  # Create README
  .create_readme(path, dirs, version)

  message("\n", cli::symbol$tick, " AquaCrop project initialized at: ", path)
  message("\n", cli::symbol$info, " Next steps:")
  message("  1. Change to project directory: setwd(\"", path, "\")")
  message("  2. Add your input files (CLIMATE/, CROP/, SOIL/, etc.)")
  message("  3. Run simulation: run_aquacrop()")

  invisible(path)
}

#' Helper function to create README
#' @keywords internal
#'
.create_readme <- function(path, dirs, version = NULL) {
  readme_path <- file.path(path, "README.md")

  # Determine executable name
  exe_name <- if (.Platform$OS.type == "windows") "aquacrop.exe" else "aquacrop"

  # Version info
  version_info <- if (!is.null(version)) {
    paste0("AquaCrop version: ", version)
  } else {
    "AquaCrop version: latest"
  }

  readme_content <- c(
    "# AquaCrop Project",
    "",
    paste("Created:", Sys.Date()),
    paste("Executable:", exe_name),
    version_info,
    "",
    "## Directory Structure",
    "",
    "- **CLIMATE/** - Climate input files (temperature, rainfall, ETo)",
    "- **CROP/** - Crop parameter files",
    "- **LIST/** - Project and simulation list files",
    "- **MANAGEMENT/** - Field management practice files",
    "- **OUTP/** - Simulation output files",
    "- **PARAM/** - General parameter files",
    "- **SIMUL/** - Simulation configuration files",
    "- **SOIL/** - Soil profile and characteristic files",
    "",
    "## Getting Started",
    "",
    "1. Add your climate data to `CLIMATE/`",
    "2. Configure crop parameters in `CROP/`",
    "3. Define soil profiles in `SOIL/`",
    "4. Set up simulations in `SIMUL/`",
    "5. Create `.PRM` project files",
    "6. Run: `run_aquacrop()` in R",
    "7. Check outputs in `OUTP/`",
    "",
    "## Running Simulations",
    "",
    "From R:",
    "```r",
    paste0('setwd("', path, '")'),
    "run_aquacrop()",
    "```",
    "",
    "## Resources",
    "",
    "- AquaCrop documentation: https://www.fao.org/aquacrop/",
    "- User manual: https://www.fao.org/aquacrop/resources/",
    ""
  )

  tryCatch({
    writeLines(readme_content, readme_path)
    message(cli::symbol$tick, " Created README.md")
  }, error = function(e) {
    warning(
      "Failed to create README.md: ", conditionMessage(e),
      call. = FALSE
    )
  })
}

#' Install simulation template files
#' @keywords internal
.install_templates <- function(path, overwrite) {
  template_files <- c(
    "AggregationResults.SIM",
    "DailyResultsFullList.SIM",
    "MaunaLoa.CO2",
    "ParticularResultsFullList.SIM"
  )

  simul_dest_dir <- file.path(path, "SIMUL")
  if (!dir.exists(simul_dest_dir)) {
    dir.create(simul_dest_dir, recursive = TRUE)
  }

  for (template_file in template_files) {
    template_src <- path_to_file(template_file)

    if (!is.null(template_src) && nzchar(template_src) && file.exists(template_src)) {
      template_dest <- file.path(simul_dest_dir, template_file)

      if (!file.exists(template_dest) || overwrite) {
        tryCatch({
          invisible(file.copy(template_src, template_dest, overwrite = overwrite))
        }, error = function(e) {
          warning(
            "Failed to copy template: ", template_file, "\n",
            "Error: ", conditionMessage(e),
            call. = FALSE
          )
        })
      }
    } else {
      warning(
        "Template file not found in package: ", template_file,
        call. = FALSE
      )
    }
  }

  message(cli::symbol$tick, " Simulation templates prepared in SIMUL/")
  invisible(TRUE)
}
