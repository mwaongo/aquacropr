#' Write AquaCrop Parameter (.PRM) File for Single Year
#'
#' @description
#' Creates an AquaCrop parameter file (.PRM) for a single station and year.
#' Includes the header (crop name and version) plus the year block.
#'
#' @param path Output directory path for PRM files. Default: `"LIST/"`
#' @param station_name Station identifier (used to find CLI, Tnx, ETo, PLU files)
#' @param year Year of cultivation
#' @param planting_doy Day of year for planting (1-365/366)
#' @param crop_name Crop name (e.g., "maize_90days")
#' @param crop_path Path to crop file directory
#' @param crop_duration Growing season length in days
#' @param climate_path Path to climate file directory (absolute or relative)
#' @param management_path Path to management file directory (absolute or relative)
#' @param soil_path Path to soil file directory (absolute or relative)
#' @param simulation_start_doy Day of year for simulation start (NULL = April 1st)
#' @param scenario Climate scenario: "hist", "rcp26", "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585"
#' @param write_header Logical. If TRUE (default), writes header. FALSE for batch mode.
#' @param eol End-of-line character style. Options: "windows","linux", or "macos". If `NULL` (default), eol is auto-detected.
#' @param use_standalone Logical. Whether running in standalone mode (default: TRUE)
#' @param base_path Character. Base absolute path (required for file existence checking)
#'
#' @importFrom purrr walk
#'
#' @return Invisibly returns the output file path
#' @keywords internal
#' @noRd
.write_prm <- function(
    station_name,
    path = "LIST/",
    year,
    planting_doy,
    crop_name,
    crop_path = "CROP/",
    crop_duration = 90,
    climate_path = "CLIMATE/",
    management_path = "MANAGEMENT",
    soil_path = "SOIL/",
    simulation_start_doy = NULL,
    scenario = "hist",
    write_header = TRUE,
    eol = NULL,
    use_standalone = TRUE,
    base_path = getwd()) {
  # Input validation
  stopifnot(
    is.character(path) && length(path) == 1,
    is.character(station_name) && length(station_name) == 1,
    is.numeric(year) && length(year) == 1,
    is.numeric(planting_doy) && length(planting_doy) == 1,
    is.numeric(crop_duration) && length(crop_duration) == 1,
    is.logical(write_header) && length(write_header) == 1,
    is.logical(use_standalone) && length(use_standalone) == 1,
    !missing(base_path) && is.character(base_path)
  )

  # Get EOL separator
  sep <- .get_eol(eol)

  # Build file paths
  cli_file <- fs::path(base_path, climate_path, paste0(station_name, ".CLI"))
  tnx_file <- fs::path(base_path, climate_path, paste0(station_name, ".Tnx"))
  eto_file <- fs::path(base_path, climate_path, paste0(station_name, ".ETo"))
  plu_file <- fs::path(base_path, climate_path, paste0(station_name, ".PLU"))
  cro_file <- fs::path(base_path, crop_path, paste0(crop_name, ".CRO"))
  man_file <- fs::path(base_path, management_path, paste0(station_name, ".MAN"))
  sol_file <- fs::path(base_path, soil_path, paste0(station_name, ".SOL"))
  sw0_file <- fs::path(base_path, soil_path, paste0(station_name, ".SW0"))

  # Validate all required files exist (single check for all files)
  .validate_files_exist(c(
    CLI = cli_file,
    Tnx = tnx_file,
    ETo = eto_file,
    PLU = plu_file,
    MAN = man_file,
    SOL = sol_file,
    SW0 = sw0_file
  ))

  # Set simulation start doy if not provided
  if (is.null(simulation_start_doy)) {
    simulation_start_doy <- ifelse(
      is_leap_year(year),
      92,
      91
    ) # April 1st
  }

  # Calculate planting end date
  planting_end_doy <- planting_doy + crop_duration - 1

  # Convert DOY to AquaCrop day numbers
  sim_start_day_number <- day_number(year, doy = simulation_start_doy)
  sim_end_day_number <- day_number(year, doy = planting_end_doy)
  crop_start_day_number <- day_number(year, doy = planting_doy)
  crop_end_day_number <- day_number(year, doy = planting_end_doy)

  # Get CO2 file based on scenario
  co2_file <- .get_co2_file(scenario)

  # Format date strings for comments
  sim_start_date_str <- .format_date_string(year, simulation_start_doy)
  crop_start_date_str <- .format_date_string(year, planting_doy)
  crop_end_date_str <- .format_date_string(year, planting_end_doy)

  # Create output directory if needed
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Prepare output file
  output_file <- file.path(path, paste0(station_name, ".PRM"))

  # Get paths for PRM file using path_for_prm()
  climate_path_prm <- path_for_prm(climate_path, use_standalone = use_standalone, base_path = base_path)
  crop_path_prm <- path_for_prm(crop_path, use_standalone = use_standalone, base_path = base_path)
  management_path_prm <- path_for_prm(management_path, use_standalone = use_standalone, base_path = base_path)
  soil_path_prm <- path_for_prm(soil_path, use_standalone = use_standalone, base_path = base_path)

  # Build the PRM content block
  prm_lines <- c(
    paste0(.format_string3(1, "%7.0f", 16), ": Year number of cultivation (Seeding/planting year)", sep),
    paste0(.format_string3(sim_start_day_number, "%7.0f", 16), ": First day of simulation period - ", sim_start_date_str, sep),
    paste0(.format_string3(sim_end_day_number, "%7.0f", 16), ": Last day of simulation period - ", crop_end_date_str, sep),
    paste0(.format_string3(crop_start_day_number, "%7.0f", 16), ": First day of cropping period - ", crop_start_date_str, sep),
    paste0(.format_string3(crop_end_day_number, "%7.0f", 16), ": Last day of cropping period - ", crop_end_date_str, sep),
    paste0("-- 1. Climate (CLI) file", sep),
    paste0(.format_string4(basename(cli_file), "%s", nchar(basename(cli_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm, "%s", nchar(climate_path_prm) + 3), sep),
    paste0("   1.1 Temperature (Tnx or TMP) file", sep),
    paste0(.format_string4(basename(tnx_file), "%s", nchar(basename(tnx_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm, "%s", nchar(climate_path_prm) + 3), sep),
    paste0("   1.2 Reference ET (ETo) file", sep),
    paste0(.format_string4(basename(eto_file), "%s", nchar(basename(eto_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm, "%s", nchar(climate_path_prm) + 3), sep),
    paste0("   1.3 Rain (PLU) filee", sep),
    paste0(.format_string4(basename(plu_file), "%s", nchar(basename(plu_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm, "%s", nchar(climate_path_prm) + 3), sep),
    paste0("   1.4 Atmospheric CO2 concentration (CO2) file", sep),
    paste0(.format_string4(co2_file, "%s", nchar(co2_file) + 3), sep),
    paste0(.format_string4(climate_path_prm, "%s", nchar(climate_path_prm) + 3), sep),
    paste0("-- 2. Calendar (CAL) file", sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0("-- 3. Crop (CRO) file", sep),
    paste0(.format_string4(paste0(crop_name, ".CRO"), "%s", nchar(paste0(crop_name, ".CRO")) + 3), sep),
    paste0(.format_string4(crop_path_prm, "%s", nchar(crop_path_prm) + 3), sep),
    paste0("-- 4. Irrigation management (IRR) file", sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0("   -- 5. Field management (MAN) file", sep),
    paste0(.format_string4(basename(man_file), "%s", nchar(basename(man_file)) + 3), sep),
    paste0(.format_string4(management_path_prm, "%s", nchar(management_path_prm) + 3), sep),
    paste0("   -- 6. Soil profile (SOL) file", sep),
    paste0(.format_string4(basename(sol_file), "%s", nchar(basename(sol_file)) + 3), sep),
    paste0(.format_string4(soil_path_prm, "%s", nchar(soil_path_prm) + 3), sep),
    paste0("-- 7. Groundwater table (GWT) file", sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0("   -- 8. Initial conditions (SW0) file", sep),
    paste0(.format_string4(basename(sw0_file), "%s", nchar(basename(sw0_file)) + 3), sep),
    paste0(.format_string4(soil_path_prm, "%s", nchar(soil_path_prm) + 3), sep),
    paste0("-- 9. Off-season conditions (OFF) file", sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0("-- 10. Field data (OBS) file", sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep),
    paste0(.format_string4("(None)", "%s", nchar("(None)") + 3), sep)
  )

  if (write_header) {
    # First write: write header
    .write_prm_header(path = path, crop = crop_name, station_name = station_name, eol = eol)
    # Then append year block line by line

    readr::write_file(
      x = paste(prm_lines, collapse = ""),
      file = output_file,
      append = TRUE
    )
    # purrr::walk(
    #   .x = prm_lines,
    #   .f = readr::write_file,
    #   file = output_file,
    #   append = TRUE
    # )
  } else {
    # Append only: no header, just year block
    readr::write_file(
      x = paste(prm_lines, collapse = ""),
      file = output_file,
      append = TRUE
    )
    # purrr::walk(
    #   .x = prm_lines,
    #   .f = readr::write_file,
    #   file = output_file,
    #   append = TRUE
    # )
  }

  invisible(output_file)
}


#' Write AquaCrop Parameter Files for Multiple Years (Batch)
#'
#' @description
#' Creates a single `.PRM` file with header once + multiple year blocks.
#'
#' @param path Output directory path
#' @param station_name Station identifier
#' @param planting_schedule data.frame with: year, planting_doy
#' @param crop_name Crop name (e.g., "maize_90days")
#' @param crop_path Path to crop file directory
#' @param crop_duration Growing season length in days
#' @param climate_path Path to climate file directory
#' @param management_path Path to management file directory
#' @param soil_path Path to soil file directory
#' @param simulation_start_doy Day of year for simulation start
#' @param scenario Climate scenario
#' @param eol End-of-line type
#' @param use_standalone Logical. Whether running in standalone mode
#' @param base_path Character. Base absolute path (required)
#' @family AquaCrop file writers
#' @return Invisibly returns the output file path
#'
#' @export
write_prm <- function(
    station_name,
    path = "LIST/",
    planting_schedule,
    crop_name,
    crop_path = "CROP/",
    crop_duration = 90,
    climate_path = "CLIMATE/",
    management_path = "MANAGEMENT/",
    soil_path = "SOIL/",
    simulation_start_doy = NULL,
    scenario = "hist",
    eol = "windows",
    use_standalone = TRUE,
    base_path = getwd()) {
  # Input validation
  stopifnot(
    is.character(path) && length(path) == 1,
    is.character(station_name) && length(station_name) == 1,
    is.data.frame(planting_schedule),
    all(c("year", "planting_doy") %in% names(planting_schedule)),
    nrow(planting_schedule) > 0,
    is.logical(use_standalone) && length(use_standalone) == 1,
    !missing(base_path) && is.character(base_path)
  )

  # Create output directory if needed
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Prepare output file path
  output_file <- file.path(path, paste0(station_name, ".PRM"))

  # Remove existing file if it exists (start fresh)
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # Process each year
  for (i in seq_len(nrow(planting_schedule))) {
    # Write header only for the first year
    write_header <- (i == 1)

    .write_prm(
      station_name = station_name,
      path = path,
      year = planting_schedule$year[i],
      planting_doy = planting_schedule$planting_doy[i],
      crop_name = crop_name,
      crop_path = crop_path,
      crop_duration = crop_duration,
      climate_path = climate_path,
      management_path = management_path,
      soil_path = soil_path,
      simulation_start_doy = simulation_start_doy,
      scenario = scenario,
      write_header = write_header,
      eol = eol,
      use_standalone = use_standalone,
      base_path = base_path
    )
  }

  invisible(output_file)
}
