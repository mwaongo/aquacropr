#' Write AquaCrop Parameter (.PRM) File for Single Year
#'
#' @description
#' Creates an AquaCrop parameter file (.PRM) for a single station and year.
#' Includes the header (crop name and version) plus the year block.
#'
#' Climate files (CLI, Tnx, ETo, PLU) and soil files (SOL, SW0) are always
#' required. All other sections are optional: if their path is NULL,
#' "(None)" is written in the PRM file.
#'
#' @param station_name Station identifier (used to find CLI, Tnx, ETo, PLU,
#'   SOL, SW0 and other files).
#' @param path Output directory path for PRM files. Default: "LIST/".
#' @param year Year of cultivation.
#' @param planting_doy Day of year for planting (1-365/366).
#' @param crop_name Crop name (e.g., "maize_90days").
#'   If NULL, section 3 is written as (None). Default: NULL.
#' @param crop_path Path to crop file directory. Default: "CROP/".
#'   If crop_name is NULL, crop_path is ignored and section 3 is written
#'   as (None).
#' @param crop_duration Growing season length in days. Default: 90.
#' @param climate_path Path to climate file directory. Default: "CLIMATE/".
#' @param management_path Path to field management file directory.
#'   Default: "MANAGEMENT/". If NULL, section 5 is written as (None).
#' @param irrigation_path Path to irrigation management file directory.
#'   Default: "MANAGEMENT/". If NULL, section 4 is written as (None).
#' @param soil_path Path to soil file directory. Required, cannot be NULL.
#'   Default: "SOIL/".
#' @param gwt_path Path to groundwater table file directory.
#'   Default: NULL. If NULL, section 7 is written as (None).
#' @param offseason_path Path to off-season conditions file directory.
#'   Default: NULL. If NULL, section 9 is written as (None).
#' @param obs_path Path to field data file directory.
#'   Default: NULL. If NULL, section 10 is written as (None).
#' @param simulation_start_doy Day of year for simulation start.
#'   If NULL (default), uses April 1st (DOY 91, or 92 for leap years).
#' @param scenario Climate scenario. One of: "hist", "rcp26", "rcp45",
#'   "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585".
#'   Default: "hist".
#' @param write_header Logical. If TRUE (default), writes the PRM header
#'   (crop name and version). Set to FALSE for multiple years mode.
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". If NULL (default), auto-detected from the system.
#' @param use_standalone Logical. Whether running AquaCrop in standalone mode.
#'   Affects how file paths are written in the PRM. Default: TRUE.
#' @param base_path Character. Base absolute path used for file existence
#'   checking. Default: getwd().
#'
#' @return Invisibly returns the output file path.
#' @keywords internal
#' @noRd
.write_prm <- function(
    station_name,
    path             = "LIST/",
    year,
    planting_doy,
    crop_name        = NULL,
    crop_path        = "CROP/",
    crop_duration    = 90,
    climate_path     = "CLIMATE/",
    management_path  = "MANAGEMENT/",
    irrigation_path  = "MANAGEMENT/",
    soil_path        = "SOIL/",
    gwt_path         = NULL,
    offseason_path   = NULL,
    obs_path         = NULL,
    simulation_start_doy = NULL,
    scenario         = "hist",
    write_header     = TRUE,
    eol              = NULL,
    use_standalone   = TRUE,
    base_path        = getwd()
) {

  # Input validation
  stopifnot(
    is.character(path)         && length(path)         == 1,
    is.character(station_name) && length(station_name) == 1,
    is.numeric(year)           && length(year)         == 1,
    is.numeric(planting_doy)   && length(planting_doy) == 1,
    is.numeric(crop_duration)  && length(crop_duration) == 1,
    is.logical(write_header)   && length(write_header) == 1,
    is.logical(use_standalone) && length(use_standalone) == 1
  )

  # crop_name and crop_path must both be provided or both be NULL
  if (is.null(crop_name) != is.null(crop_path)) {
    stop("crop_name and crop_path must both be provided or both be NULL.")
  }

  sep <- .get_eol(eol)

  # Always required files:
  # CLI, Tnx, ETo, PLU: climate data (section 1)
  # SOL, SW0: soil profile and initial conditions (sections 6 and 8)
  cli_file <- fs::path(base_path, climate_path, paste0(station_name, ".CLI"))
  tnx_file <- fs::path(base_path, climate_path, paste0(station_name, ".Tnx"))
  eto_file <- fs::path(base_path, climate_path, paste0(station_name, ".ETo"))
  plu_file <- fs::path(base_path, climate_path, paste0(station_name, ".PLU"))
  sol_file <- fs::path(base_path, soil_path,    paste0(station_name, ".SOL"))
  sw0_file <- fs::path(base_path, soil_path,    paste0(station_name, ".SW0"))

  .validate_files_exist(c(
    CLI = cli_file,
    Tnx = tnx_file,
    ETo = eto_file,
    PLU = plu_file,
    SOL = sol_file,
    SW0 = sw0_file
  ))

  # Dynamic sections (2 to 10).
  # Each entry corresponds to one section of the PRM file.
  # path: if NULL, the section is written as "(None)" with no file validation.
  # name: base name of the file; if absent, station_name is used.
  #       Only CRO uses crop_name instead of station_name.
  prm_sections <- list(
    list(n = "2",  label = "Calendar (CAL) file",              ext = "CAL", path = NULL),
    list(n = "3",  label = "Crop (CRO) file",                  ext = "CRO", path = if (is.null(crop_name)) NULL else crop_path, name = crop_name),
    list(n = "4",  label = "Irrigation management (IRR) file", ext = "IRR", path = irrigation_path),
    list(n = "5",  label = "Field management (MAN) file",      ext = "MAN", path = management_path),
    list(n = "6",  label = "Soil profile (SOL) file",          ext = "SOL", path = soil_path),
    list(n = "7",  label = "Groundwater table (GWT) file",     ext = "GWT", path = gwt_path),
    list(n = "8",  label = "Initial conditions (SW0) file",    ext = "SW0", path = soil_path),
    list(n = "9",  label = "Off-season conditions (OFF) file", ext = "OFF", path = offseason_path),
    list(n = "10", label = "Field data (OBS) file",            ext = "OBS", path = obs_path)
  )

  # Validate optional files that have a non-NULL path
  optional_files <- Filter(Negate(is.null), lapply(prm_sections, function(s) {
    if (is.null(s$path)) return(NULL)
    nm   <- if (!is.null(s$name)) s$name else station_name
    file <- fs::path(base_path, s$path, paste0(nm, ".", s$ext))
    stats::setNames(file, s$ext)
  }))
  if (length(optional_files) > 0) {
    .validate_files_exist(unlist(optional_files))
  }

  # Set simulation start DOY to April 1st if not provided
  if (is.null(simulation_start_doy)) {
    simulation_start_doy <- ifelse(is_leap_year(year), 92, 91)
  }

  planting_end_doy      <- planting_doy + crop_duration - 1

  sim_start_day_number  <- day_number(year, doy = simulation_start_doy)
  sim_end_day_number    <- day_number(year, doy = planting_end_doy)
  crop_start_day_number <- day_number(year, doy = planting_doy)
  crop_end_day_number   <- day_number(year, doy = planting_end_doy)

  sim_start_date_str    <- .format_date_string(year, simulation_start_doy)
  crop_start_date_str   <- .format_date_string(year, planting_doy)
  crop_end_date_str     <- .format_date_string(year, planting_end_doy)

  co2_file <- .get_co2_file(scenario)

  # Create output directory if needed
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  output_file <- file.path(path, paste0(station_name, ".PRM"))

  # Climate path formatted for the PRM file
  climate_path_prm <- path_for_prm(climate_path, use_standalone = use_standalone, base_path = base_path)

  # Date block (lines 1 to 5)
  date_lines <- c(
    paste0(.format_string3(1,                     "%7.0f", 16), ": Year number of cultivation (Seeding/planting year)", sep),
    paste0(.format_string3(sim_start_day_number,  "%7.0f", 16), ": First day of simulation period - ",  sim_start_date_str,  sep),
    paste0(.format_string3(sim_end_day_number,    "%7.0f", 16), ": Last day of simulation period - ",   crop_end_date_str,   sep),
    paste0(.format_string3(crop_start_day_number, "%7.0f", 16), ": First day of cropping period - ",    crop_start_date_str, sep),
    paste0(.format_string3(crop_end_day_number,   "%7.0f", 16), ": Last day of cropping period - ",     crop_end_date_str,   sep)
  )

  # Section 1: Climate (always required, written explicitly)
  # Contains CLI and sub-sections Tnx, ETo, PLU, CO2
  climate_lines <- c(
    paste0("-- 1. Climate (CLI) file", sep),
    paste0(.format_string4(basename(cli_file), "%s", nchar(basename(cli_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.1 Temperature (Tnx or TMP) file", sep),
    paste0(.format_string4(basename(tnx_file), "%s", nchar(basename(tnx_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.2 Reference ET (ETo) file", sep),
    paste0(.format_string4(basename(eto_file), "%s", nchar(basename(eto_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.3 Rain (PLU) file", sep),
    paste0(.format_string4(basename(plu_file), "%s", nchar(basename(plu_file)) + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep),
    paste0("   1.4 Atmospheric CO2 concentration (CO2) file", sep),
    paste0(.format_string4(co2_file,           "%s", nchar(co2_file)           + 3), sep),
    paste0(.format_string4(climate_path_prm,   "%s", nchar(climate_path_prm)   + 3), sep)
  )

  # Sections 2 to 10: generated dynamically from prm_sections.
  # For each section:
  #   if path is NULL: file = "(None)", path = "(None)"
  #   otherwise:       file = <n>.<ext>, path = path_for_prm(path)
  dynamic_lines <- unlist(lapply(prm_sections, function(s) {
    nm       <- if (!is.null(s$name)) s$name else station_name
    file_nm  <- if (is.null(s$path)) "(None)" else paste0(nm, ".", s$ext)
    path_prm <- if (is.null(s$path)) "(None)" else path_for_prm(s$path, use_standalone, base_path)

    c(
      paste0("-- ", s$n, ". ", s$label, sep),
      paste0(.format_string4(file_nm,  "%s", nchar(file_nm)  + 3), sep),
      paste0(.format_string4(path_prm, "%s", nchar(path_prm) + 3), sep)
    )
  }))

  prm_lines <- c(date_lines, climate_lines, dynamic_lines)

  # Write header (crop name and version) only when requested
  if (write_header) {
    .write_prm_header(path = path, crop = crop_name, station_name = station_name, eol = eol)
  }

  readr::write_file(
    x      = paste(prm_lines, collapse = ""),
    file   = output_file,
    append = TRUE
  )

  invisible(output_file)
}


#' Write AquaCrop Parameter Files for Multiple Years
#'
#' @description
#' Creates a single .PRM file with the header written once, followed by
#' one year block per row in planting_schedule.
#'
#' Climate files (CLI, Tnx, ETo, PLU) and soil files (SOL, SW0) are always
#' required. All other sections are optional: if their path is NULL,
#' "(None)" is written in the PRM file.
#'
#' @param station_name Station identifier.
#' @param path Output directory path for PRM files. Default: "LIST/".
#' @param planting_schedule data.frame with at least two columns:
#'   \describe{
#'     \item{year}{Numeric. Year of cultivation.}
#'     \item{planting_doy}{Numeric. Day of year for planting (1-365/366).}
#'   }
#' @param crop_name Crop name (e.g., "maize_90days").
#'   If NULL, section 3 is written as (None). Default: NULL.
#' @param crop_path Path to crop file directory. Default: "CROP/".
#'   If crop_name is NULL, crop_path is ignored and section 3 is written
#'   as (None).
#' @param crop_duration Growing season length in days. Default: 90.
#' @param climate_path Path to climate file directory. Default: "CLIMATE/".
#' @param management_path Path to field management file directory.
#'   Default: "MANAGEMENT/". If NULL, section 5 is written as (None).
#' @param irrigation_path Path to irrigation management file directory.
#'   Default: "MANAGEMENT/". If NULL, section 4 is written as (None).
#' @param soil_path Path to soil file directory. Required, cannot be NULL.
#'   Default: "SOIL/".
#' @param gwt_path Path to groundwater table file directory.
#'   Default: NULL. If NULL, section 7 is written as (None).
#' @param offseason_path Path to off-season conditions file directory.
#'   Default: NULL. If NULL, section 9 is written as (None).
#' @param obs_path Path to field data file directory.
#'   Default: NULL. If NULL, section 10 is written as (None).
#' @param simulation_start_doy Day of year for simulation start.
#'   If NULL (default), uses April 1st (DOY 91, or 92 for leap years).
#' @param scenario Climate scenario. One of: "hist", "rcp26", "rcp45",
#'   "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585".
#'   Default: "hist".
#' @param eol End-of-line character style. One of "windows", "linux",
#'   "macos". Default: "windows".
#' @param use_standalone Logical. Whether running AquaCrop in standalone mode.
#'   Default: TRUE.
#' @param base_path Character. Base absolute path used for file existence
#'   checking. Default: getwd().
#'
#' @family AquaCrop file writers
#' @return Invisibly returns the output file path.
#' @export
write_prm <- function(
    station_name,
    path             = "LIST/",
    planting_schedule,
    crop_name        = NULL,
    crop_path        = "CROP/",
    crop_duration    = 90,
    climate_path     = "CLIMATE/",
    management_path  = "MANAGEMENT/",
    irrigation_path  = "MANAGEMENT/",
    soil_path        = "SOIL/",
    gwt_path         = NULL,
    offseason_path   = NULL,
    obs_path         = NULL,
    simulation_start_doy = NULL,
    scenario         = "hist",
    eol              = "windows",
    use_standalone   = TRUE,
    base_path        = getwd()
) {

  # Input validation
  stopifnot(
    is.character(path)         && length(path)         == 1,
    is.character(station_name) && length(station_name) == 1,
    is.data.frame(planting_schedule),
    all(c("year", "planting_doy") %in% names(planting_schedule)),
    nrow(planting_schedule) > 0,
    is.logical(use_standalone) && length(use_standalone) == 1
  )

  # crop_name and crop_path must both be provided or both be NULL
  if (is.null(crop_name) != is.null(crop_path)) {
    stop("crop_name and crop_path must both be provided or both be NULL.")
  }

  # Create output directory if needed
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  # Remove existing file to start fresh
  output_file <- file.path(path, paste0(station_name, ".PRM"))
  if (file.exists(output_file)) file.remove(output_file)

  # Write one block per year.
  # The header (crop name and version) is written only for the first year.
  for (i in seq_len(nrow(planting_schedule))) {
    .write_prm(
      station_name         = station_name,
      path                 = path,
      year                 = planting_schedule$year[i],
      planting_doy         = planting_schedule$planting_doy[i],
      crop_name            = crop_name,
      crop_path            = crop_path,
      crop_duration        = crop_duration,
      climate_path         = climate_path,
      management_path      = management_path,
      irrigation_path      = irrigation_path,
      soil_path            = soil_path,
      gwt_path             = gwt_path,
      offseason_path       = offseason_path,
      obs_path             = obs_path,
      simulation_start_doy = simulation_start_doy,
      scenario             = scenario,
      write_header         = (i == 1),
      eol                  = eol,
      use_standalone       = use_standalone,
      base_path            = base_path
    )
  }

  invisible(output_file)
}
