write_prm <- function(
    site_name,
    path                 = "LIST/",
    planting_schedule    = NULL,
    crop_name            = NULL,
    crop_path            = "CROP/",
    crop_duration        = 90,
    climate_path         = "CLIMATE/",
    calendar_path        = NULL,
    management_path      = "MANAGEMENT/",
    irrigation_path      = "MANAGEMENT/",
    soil_path            = "SOIL/",
    groundwater_path     = NULL,
    offseason_path       = NULL,
    obs_path             = NULL,
    simulation_start_doy = NULL,
    scenario             = "hist",
    eol                  = "windows",
    use_standalone       = TRUE,
    base_path            = getwd()
) {
  stopifnot(
    is.character(path)         && length(path)         == 1,
    is.character(site_name)    && length(site_name)    == 1,
    is.logical(use_standalone) && length(use_standalone) == 1
  )

  # ---- crop_name / crop_path must be both provided or both NULL ------------
  if (is.null(crop_name) != is.null(crop_path)) {
    stop("crop_name and crop_path must both be provided or both be NULL.")
  }

  # ---- Warn if optional file paths are provided but files not found --------

  if (!is.null(irrigation_path)) {
    irr_file <- fs::path(base_path, irrigation_path, paste0(site_name, ".IRR"))
    if (!fs::file_exists(irr_file)) {
      warning("Irrigation file not found: ", irr_file, " — irrigation_path set to NULL.", call. = FALSE)
      irrigation_path <- NULL
    }
  }

  if (!is.null(obs_path)) {
    obs_file <- fs::path(base_path, obs_path, paste0(site_name, ".OBS"))
    if (!fs::file_exists(obs_file)) {
      warning("Observations file not found: ", obs_file, " — obs_path set to NULL.", call. = FALSE)
      obs_path <- NULL
    }
  }

  if (!is.null(offseason_path)) {
    off_file <- fs::path(base_path, offseason_path, paste0(site_name, ".OFF"))
    if (!fs::file_exists(off_file)) {
      warning("Off-season file not found: ", off_file, " — offseason_path set to NULL.", call. = FALSE)
      offseason_path <- NULL
    }
  }

  # ---- Calendar path → derive planting schedule ----------------------------
  if (!is.null(calendar_path)) {
    if (!is.null(planting_schedule)) {
      warning(
        "Both calendar_path and planting_schedule were provided. ",
        "calendar_path takes precedence; planting_schedule is ignored.",
        call. = FALSE
      )
    }
    onset <- find_onset(
      site_name    = site_name,
      cal_path     = calendar_path,
      climate_path = climate_path,
      base_path    = base_path
    )
    planting_schedule <- data.frame(year = onset$year, planting_doy = onset$onset_doy)
  }

  if (is.null(planting_schedule)) {
    stop("planting_schedule is required when calendar_path is NULL.", call. = FALSE)
  }
  if (
    !is.data.frame(planting_schedule) ||
    !all(c("year", "planting_doy") %in% names(planting_schedule)) ||
    nrow(planting_schedule) == 0
  ) {
    stop(
      "planting_schedule must be a data.frame with columns year and planting_doy.",
      call. = FALSE
    )
  }

  # ---- Write PRM file(s) ---------------------------------------------------
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  output_file <- file.path(path, paste0(site_name, ".PRM"))
  if (file.exists(output_file)) file.remove(output_file)

  for (i in seq_len(nrow(planting_schedule))) {
    .write_prm(
      site_name            = site_name,
      path                 = path,
      year                 = planting_schedule$year[i],
      planting_doy         = planting_schedule$planting_doy[i],
      crop_name            = crop_name,
      crop_path            = crop_path,
      crop_duration        = crop_duration,
      climate_path         = climate_path,
      calendar_path        = NULL,
      management_path      = management_path,
      irrigation_path      = irrigation_path,
      soil_path            = soil_path,
      groundwater_path     = groundwater_path,
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
