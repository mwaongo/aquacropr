# Write AquaCrop Observed Data (.OBS) Files for Multiple Stations

Generate AquaCrop observed data files for multiple stations.

## Usage

``` r
write_obs_batch(
  site_name = NULL,
  obs_data,
  path = "OBS/",
  climate_path = "CLIMATE/",
  soil_depth = 1,
  start_day = 1,
  start_month = 1,
  start_year = 1901,
  version = 7.1,
  eol = NULL,
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

- obs_data:

  Either a single data.frame applied to all stations, or a list of
  data.frames (one per station). Each data.frame must have columns: day,
  cc_mean, cc_std, biomass_mean, biomass_std, swc_mean, swc_std. Use
  -9.0 for missing values.

- path:

  Output directory path for OBS files. Default: "OBS/".

- climate_path:

  Path to climate files directory, used for station discovery. Default:
  "CLIMATE/".

- soil_depth:

  Numeric. Depth of sampled soil profile in meters. Default: 1.00.

- start_day:

  Integer. First day of observations. Default: 1.

- start_month:

  Integer. First month of observations. Default: 1.

- start_year:

  Integer. First year of observations. Use 1901 if not linked to a
  specific year. Default: 1901.

- version:

  Numeric. AquaCrop version number. Default: 7.1.

- eol:

  End-of-line character style. One of "windows", "linux", "macos". If
  NULL (default), auto-detected from the system.

- base_path:

  Base absolute path. Default: current working directory.

- verbose:

  Logical. If TRUE (default), prints progress messages. If FALSE, runs
  silently.

- clean:

  Logical. If TRUE, removes existing .OBS files from path before writing
  new files. Default: FALSE.

## Value

Invisibly returns NULL. The main effect is writing OBS files to the
specified directory.

## Details

The function validates that all specified stations have corresponding
climate files. A single data.frame for obs_data will be automatically
applied to all stations. The first line of every OBS file is always
"default" as AquaCrop ignores this label at runtime.

## See also

[`write_obs`](https://mwaongo.github.io/aquacropr/reference/write_obs.md)
for single station OBS file generation.

Other batch operations:
[`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md),
[`write_irrig_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irrig_batch.md),
[`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
[`write_off_batch()`](https://mwaongo.github.io/aquacropr/reference/write_off_batch.md),
[`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md),
[`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Only biomass measured, canopy cover and soil water content missing
obs <- data.frame(
  day          = c(144, 160, 169, 176, 184),
  cc_mean      = rep(-9.0, 5),
  cc_std       = rep(-9.0, 5),
  biomass_mean = c(0.045, 0.386, 1.039, 1.217, 2.390),
  biomass_std  = rep(-9.0, 5),
  swc_mean     = rep(-9.0, 5),
  swc_std      = rep(-9.0, 5)
)

# Same observations for all stations
write_obs_batch(
  site_name = c("grid_001", "grid_002"),
  obs_data     = obs,
  soil_depth   = 1.00,
  start_day    = 1,
  start_month  = 1,
  start_year   = 2014
)

# Different observations per station
write_obs_batch(
  site_name = c("grid_001", "grid_002"),
  obs_data     = list(obs, obs),
  start_year   = 2014
)
} # }
```
