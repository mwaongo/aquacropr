# Write AquaCrop PRM files for Multiple Stations

Generate AquaCrop PRM files for multiple stations.

## Usage

``` r
write_prm_batch(
  site_name = NULL,
  crop_name = NULL,
  planting_schedule = NULL,
  path = "LIST/",
  crop_path = "CROP/",
  climate_path = "CLIMATE/",
  calendar_path = NULL,
  management_path = "MANAGEMENT/",
  irrigation_path = "MANAGEMENT/",
  soil_path = "SOIL/",
  groundwater_path = NULL,
  offseason_path = NULL,
  obs_path = NULL,
  crop_duration = NULL,
  simulation_start_doy = NULL,
  scenario = "hist",
  eol = NULL,
  use_standalone = TRUE,
  base_path = getwd(),
  verbose = TRUE,
  clean = FALSE
)
```

## Arguments

- site_name:

  Character vector or NULL. Names of stations to process. If NULL, all
  stations are automatically discovered from .CLI files in the climate
  directory. If a vector, only the specified stations will be processed;
  all must have corresponding climate files.

- crop_name:

  Character. Crop name (e.g., "maize", "wheat"). If NULL, section 3 is
  written as (None). Default: NULL.

- planting_schedule:

  Either a single data.frame with columns year and planting_doy, applied
  to all stations, or a list of data.frames (one per station), each with
  columns year and planting_doy. The length of the list must match the
  number of stations. Optional when calendar_path is provided: the
  planting schedule is then derived automatically from find_onset() for
  each station. If both are supplied, calendar_path takes precedence and
  planting_schedule is ignored with a warning.

- path:

  Output directory path for PRM files. Default: "LIST/".

- crop_path:

  Path to crop files directory. Default: "CROP/". If crop_name is NULL,
  crop_path is ignored and section 3 is written as (None).

- climate_path:

  Path to climate files directory. Default: "CLIMATE/".

- calendar_path:

  Path to calendar files directory. Default: NULL. When provided, the
  planting schedule is derived automatically via find_onset() for each
  station, and planting_schedule can be NULL. Section 2 is always
  written as (None): the CAL file is used only to compute onset dates,
  not referenced in the PRM.

- management_path:

  Path to field management files directory. Default: "MANAGEMENT/". If
  NULL, section 5 is written as (None).

- irrigation_path:

  Path to irrigation management files directory. Default: "MANAGEMENT/".
  If the .IRR file is not found for a station, a warning is issued and
  section 4 is written as (None) for that station. If NULL, section 4 is
  written as (None) for all stations.

- soil_path:

  Path to soil files directory. Required, cannot be NULL. Default:
  "SOIL/".

- groundwater_path:

  Path to groundwater table files directory. Default: NULL. If NULL,
  section 7 is written as (None).

- offseason_path:

  Path to off-season conditions files directory. Default: NULL. If the
  .OFF file is not found for a station, a warning is issued and section
  9 is written as (None) for that station.

- obs_path:

  Path to field data files directory. Default: NULL. If the .OBS file is
  not found for a station, a warning is issued and section 10 is written
  as (None) for that station.

- crop_duration:

  Integer or NULL. Crop duration in days. If NULL (default), read
  automatically from the .CRO file.

- simulation_start_doy:

  Integer. Day of year to start simulation. Default: NULL.

- scenario:

  Character. Scenario name. Default: "hist".

- eol:

  End-of-line character style. One of "windows", "linux", "macos". If
  NULL (default), eol is auto-detected.

- use_standalone:

  Logical. Use standalone mode? Default: TRUE.

- base_path:

  Base absolute path. Default: current working directory.

- verbose:

  Logical. If TRUE (default), prints progress messages. If FALSE, runs
  silently.

- clean:

  Logical. If TRUE, removes existing .PRM files from path before writing
  new files. Default: FALSE.

## Value

Invisibly returns NULL. The main effect is writing PRM files to the
specified directory.

## Details

The function validates that all specified stations have corresponding
climate files. If crop_name is not NULL, the crop file is also
validated. A single data.frame for planting_schedule will be
automatically applied to all stations.

Validation of optional file paths (irrigation, off-season, observations)
is delegated to
[`write_prm`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
which issues a warning and sets the corresponding path to NULL when the
file is not found.

## See also

[`write_prm`](https://mwaongo.github.io/aquacropr/reference/write_prm.md)
for single station PRM file generation.

Other batch operations:
[`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md),
[`write_irrig_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irrig_batch.md),
[`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
[`write_obs_batch()`](https://mwaongo.github.io/aquacropr/reference/write_obs_batch.md),
[`write_off_batch()`](https://mwaongo.github.io/aquacropr/reference/write_off_batch.md),
[`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: multiple stations, same planting schedule
plsch    <- data.frame(year = 1981:2020, planting_doy = 201)
stations <- c("grid_001", "grid_002")

write_prm_batch(
  site_name         = stations,
  crop_name         = "maize",
  planting_schedule = plsch,
  crop_duration     = 90
)

# Example 2: multiple stations, different planting schedules
plsch_list <- list(
  data.frame(year = 1981:2020, planting_doy = 201),
  data.frame(year = 1981:2020, planting_doy = 205)
)

write_prm_batch(
  site_name         = stations,
  crop_name         = "maize",
  planting_schedule = plsch_list,
  crop_duration     = 120,
  path              = "LIST/"
)

# Example 3: calendar-based onset, no planting_schedule needed
write_prm_batch(
  site_name        = NULL,
  crop_name        = "wheat",
  calendar_path    = "CAL/",
  groundwater_path = "GWT/",
  base_path        = "/my/project/path",
  verbose          = FALSE
)
} # }
```
