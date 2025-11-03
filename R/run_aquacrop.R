#' Run AquaCrop simulation
#'
#' Execute AquaCrop in the current directory. The directory must contain
#' the AquaCrop executable (aquacrop.exe on Windows, aquacrop on Linux/macOS).
#'
#' @param verbose Print messages (default: TRUE)
#'
#' @return Exit status (0 = success)
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize project and run
#' init_aquacrop("~/my-project")
#' setwd("~/my-project")
#' run_aquacrop()
#' 
#' # Or from within project directory
#' setwd("~/my-project")
#' run_aquacrop()
#' }
run_aquacrop <- function(verbose = TRUE) {
  
  # Determine executable name based on OS
  if (.Platform$OS.type == "windows") {
    exe_name <- "aquacrop.exe"
    cmd <- "aquacrop.exe"
  } else {
    exe_name <- "aquacrop"
    cmd <- "./aquacrop"
  }
  
  # Check if executable exists in current directory
  if (!file.exists(exe_name)) {
    
    # Check if ALL typical AquaCrop folders are present
    aquacrop_folders <- c(
      "CLIMATE", "CROP", "SOIL", "SIMUL", "OUTP", 
      "MANAGEMENT", "PARAM", "LIST"
    )
    
    all_folders_present <- all(dir.exists(aquacrop_folders))
    
    if (all_folders_present) {
      # Complete AquaCrop project structure but binary is missing
      stop(
        "AquaCrop executable not found: ", exe_name, "\n",
        "This seems to be an AquaCrop project directory, but the binary is missing.\n\n",
        "Solution:\n",
        "  install_binaries(path = \".\")\n"
      )
    } else {
      # Not a complete AquaCrop project
      stop(
        "AquaCrop executable not found: ", exe_name, "\n",
        "You must run this function from a directory containing the AquaCrop binary.\n\n",
        "Solutions:\n",
        "  1. Initialize new project: init_aquacrop(path = \"my-project\")\n",
        "  2. If already in project: install_binaries(path = \".\")\n",
        "  3. Change to correct directory: setwd(\"/path/to/aquacrop/project\")\n"
      )
    }
  }
  
  # Ensure executable permissions on Unix
  if (.Platform$OS.type != "windows") {
    Sys.chmod(exe_name, mode = "0755")
  }
  
  # Run AquaCrop
  if (verbose) {
    message("Running AquaCrop in: ", getwd())
  }
  
  result <- system2(cmd, stdout = "", stderr = "")
  
  # Check result
  if (result != 0) {
    warning("AquaCrop exited with status: ", result)
  } else {
    if (verbose) {
      message("AquaCrop ran successfully!")
      message("Check results in OUTP/ directory")
    }
  }
  
  invisible(result)
}
