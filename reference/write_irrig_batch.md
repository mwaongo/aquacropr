# Write AquaCrop Irrigation (.IRR) Files for Multiple Stations

Generate AquaCrop irrigation files for multiple stations.

## Usage

``` r
write_irrig_batch(
  site_name = NULL,
  method,
  wet_surface,
  mode,
  irr_data,
  path = "MANAGEMENT/",
  climate_path = "CLIMATE/",
  version = 7.1,
  crop_length = NULL,
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

- method:

  Integer. Irrigation method applied to all stations. One of: 1 =
  Sprinkler, 2 = Basin, 3 = Border, 4 = Furrow, 5 = Drip.

- wet_surface:

  Numeric. Percentage of soil surface wetted (0-100), applied to all
  stations.

- mode:

  Integer. Irrigation mode applied to all stations. One of: 1 =
  Specified events, 2 = Generated schedule, 3 = Net requirement.

- irr_data:

  Either a single data.frame applied to all stations, or a list of
  data.frames (one per station). Structure depends on mode: mode 1:
  columns day and depth; mode 2: schedule rule columns; mode 3: net
  requirement columns.

- path:

  Output directory path for IRR files. Default: "MANAGEMENT/".

- climate_path:

  Path to climate files directory, used for station discovery. Default:
  "CLIMATE/".

- version:

  Numeric. AquaCrop version number. Default: 7.1.

- crop_length:

  Integer. Length of crop cycle in days. If provided, events or rules
  beyond crop_length will trigger a warning. Default: NULL.

- eol:

  End-of-line character style. One of "windows", "linux", "macos". If
  NULL (default), eol is auto-detected.

- base_path:

  Base absolute path. Default: current working directory.

- verbose:

  Logical. If TRUE (default), prints progress messages. If FALSE, runs
  silently.

- clean:

  Logical. If TRUE, removes existing .IRR files from path before writing
  new files. Default: FALSE.

## Value

Invisibly returns NULL. The main effect is writing IRR files to the
specified directory.

## Details

The function validates that all specified stations have corresponding
climate files. A single data.frame for irr_data will be automatically
applied to all stations.

The irrigation file for each station is named after the station (e.g.,
"grid_001.IRR").

## See also

[`write_irr`](https://mwaongo.github.io/aquacropr/reference/write_irr.md)
for single station IRR file generation.

Other batch operations:
[`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md),
[`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
[`write_obs_batch()`](https://mwaongo.github.io/aquacropr/reference/write_obs_batch.md),
[`write_off_batch()`](https://mwaongo.github.io/aquacropr/reference/write_off_batch.md),
[`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md),
[`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: multiple stations, same irrigation events (mode 1)
irr <- data.frame(day = c(20, 40, 60), depth = c(30, 30, 25))
stations <- c("grid_001", "grid_002")
write_irrig_batch(
  site_name = stations,
  method       = 1,
  wet_surface  = 100,
  mode         = 1,
  irr_data     = irr
)

# Example 2: multiple stations, different irrigation data
irr_list <- list(
  data.frame(day = c(20, 40), depth = c(30, 25)),
  data.frame(day = c(25, 45), depth = c(35, 30))
)
write_irrig_batch(
  site_name = stations,
  method       = 1,
  wet_surface  = 100,
  mode         = 1,
  irr_data     = irr_list,
  crop_length  = 90
)

# Example 3: auto-discover all stations, silent mode
write_irrig_batch(
  site_name = NULL,
  method       = 4,
  wet_surface  = 60,
  mode         = 2,
  irr_data     = irr,
  base_path    = "/my/project/path",
  verbose      = FALSE
)
} # }
```
