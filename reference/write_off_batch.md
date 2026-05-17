# Write AquaCrop Off-Season Conditions (.OFF) Files for Multiple Stations

Generate AquaCrop off-season conditions files for multiple stations.

## Usage

``` r
write_off_batch(
  site_name = NULL,
  mulch_before = 0,
  mulch_after = 0,
  mulch_effect = 50,
  ecw_before = 0,
  ecw_after = 0,
  wet_surface = 100,
  irr_before = NULL,
  irr_after = NULL,
  path = "MANAGEMENT/",
  climate_path = "CLIMATE/",
  description = "Field and irrigation management conditions in the off-season",
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

- mulch_before:

  Integer. Percentage of ground surface covered by mulches before the
  growing period (0-100). Default: 0.

- mulch_after:

  Integer. Percentage of ground surface covered by mulches after the
  growing period (0-100). Default: 0.

- mulch_effect:

  Integer. Effect of mulches on reduction of soil evaporation in percent
  (10-100). Default: 50.

- ecw_before:

  Numeric. Electrical conductivity of irrigation water before the
  growing period (dS/m). Default: 0.

- ecw_after:

  Numeric. Electrical conductivity of irrigation water after the growing
  period (dS/m). Default: 0.

- wet_surface:

  Integer. Percentage of soil surface wetted by off-season irrigation
  (0-100). Default: 100.

- irr_before:

  Either a single data.frame applied to all stations, or a list of
  data.frames (one per station), or NULL (no events). Each data.frame
  must have columns day and depth. Maximum 5 rows. Default: NULL.

- irr_after:

  Either a single data.frame applied to all stations, or a list of
  data.frames (one per station), or NULL (no events). Each data.frame
  must have columns day and depth. Maximum 5 rows. Default: NULL.

- path:

  Output directory path for OFF files. Default: "MANAGEMENT/".

- climate_path:

  Path to climate files directory, used for station discovery. Default:
  "CLIMATE/".

- description:

  Character. Description written on the first line. Default: "Field and
  irrigation management conditions in the off-season".

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

  Logical. If TRUE, removes existing .OFF files from path before writing
  new files. Default: FALSE.

## Value

Invisibly returns NULL. The main effect is writing OFF files to the
specified directory.

## Details

The function validates that all specified stations have corresponding
climate files. A single data.frame for irr_before or irr_after will be
automatically applied to all stations. Mulch and ECw parameters are
scalar and applied uniformly to all stations.

## See also

[`write_off`](https://mwaongo.github.io/aquacropr/reference/write_off.md)
for single station OFF file generation.

Other batch operations:
[`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md),
[`write_irrig_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irrig_batch.md),
[`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
[`write_obs_batch()`](https://mwaongo.github.io/aquacropr/reference/write_obs_batch.md),
[`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md),
[`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- c("grid_001", "grid_002")

# Same conditions for all stations
write_off_batch(
  site_name = stations,
  mulch_after  = 70,
  mulch_effect = 50,
  ecw_before   = 1.5,
  ecw_after    = 3.0,
  irr_before   = data.frame(day = 10, depth = 40)
)

# Different irrigation events per station
write_off_batch(
  site_name = stations,
  ecw_before   = 1.5,
  irr_before   = list(
    data.frame(day = 10, depth = 40),
    data.frame(day = 15, depth = 35)
  )
)
} # }
```
