# Write AquaCrop SOL files in batch

Generate AquaCrop SOL (and optionally SW0) files for multiple sites in a
batch.

## Usage

``` r
write_sol_batch(
  site_name = NULL,
  params = NULL,
  path = "SOIL/",
  eol = NULL,
  verbose = TRUE,
  clean = FALSE,
  write_sw0 = TRUE,
  initial_water = "WP",
  salinity = "none",
  initial_cc = -9,
  initial_biomass = 0,
  initial_root_depth = -9,
  bund_water = 0,
  bund_ec = 0,
  swo_layers = NULL,
  climate_path = "CLIMATE/",
  base_path = getwd()
)
```

## Arguments

- site_name:

  Character vector or `NULL`. Names of sites to process:

  - If `NULL`, all sites are automatically discovered from `.CLI` files
    in the `CLIMATE/` directory.

  - If a vector, only the specified sites will be processed.

- params:

  Either:

  - A single named `list` of soil parameters (cn, rew, texture,
    thickness), applied to all sites, or

  - A `list` of named `list`s (one per site), each containing soil
    parameters. The length of the list must match the number of sites.
    Set to `NULL` or [`list()`](https://rdrr.io/r/base/list.html) to use
    all default values for all sites. Valid parameters: `cn`, `rew`,
    `texture`, `thickness`.

- path:

  Output directory path. Default: `"SOIL/"`.

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected.

- verbose:

  Logical. If `TRUE` (default), prints progress messages. If `FALSE`,
  runs silently.

- clean:

  Logical. If `TRUE`, removes existing `.SOL` and `.SW0` files from
  `path` before writing new files. Default: `FALSE`

- write_sw0:

  Logical; if `TRUE`, also write SW0 files. Default: `TRUE`.

- initial_water:

  Initial water content specification for SW0 files. Default: `"WP"`.

- salinity:

  Salinity specification for SW0 files. Default: `"none"`.

- initial_cc:

  Initial canopy cover for SW0 files. Default: `-9.00`.

- initial_biomass:

  Initial biomass for SW0 files. Default: `0.000`.

- initial_root_depth:

  Initial root depth for SW0 files. Default: `-9.00`.

- bund_water:

  Bund water for SW0 files. Default: `0.0`.

- bund_ec:

  Bund EC for SW0 files. Default: `0.00`.

- swo_layers:

  Optional layers for SW0 files. Default: `NULL`.

- climate_path:

  Path to climate file directory for auto-discovery. Default:
  `"CLIMATE/"`.

- base_path:

  Base absolute path. Default: current working directory.

## Value

Invisibly returns `NULL`. The main effect is writing SOL (and optionally
SW0) files to the specified directory.

## Details

When `site_name = NULL`, the function discovers sites from `.CLI` files
in the climate directory.

A single `list` of parameters will be automatically applied to all
sites.

Valid parameters in `params` include `cn`, `rew`, `texture`,
`thickness`. SW0-related parameters (`initial_water`, `salinity`, etc.)
are passed as direct arguments to `write_sol_batch()` and will be
applied to all sites.

See
[`write_sol`](https://mwaongo.github.io/aquacropr/reference/write_sol.md)
documentation for parameter descriptions and valid ranges.

Use this function when you need to create SOL files for **multiple sites
at once**.

## See also

[`write_sol`](https://mwaongo.github.io/aquacropr/reference/write_sol.md)
for single soil file generation,
[`write_swo`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
for initial water content files,
[`write_man_batch`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md)
for batch management file generation,
[`SoilWater`](https://mwaongo.github.io/aquacropr/reference/SoilWater.md)
for hydraulic property reference

Other batch operations:
[`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md),
[`write_irrig_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irrig_batch.md),
[`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
[`write_obs_batch()`](https://mwaongo.github.io/aquacropr/reference/write_obs_batch.md),
[`write_off_batch()`](https://mwaongo.github.io/aquacropr/reference/write_off_batch.md),
[`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Multiple sites, same soil profile
soil_params <- list(
  texture = c("sandy loam", "clay loam"),
  thickness = c(0.4, 0.6),
  cn = 65,
  rew = 7
)
sites <- c("grid_001", "grid_002", "grid_003")
write_sol_batch(
  site_name = sites,
  params = soil_params
)

# Example 2: Multiple sites, different soil profiles
params_list <- list(
  list(texture = "sandy loam", thickness = 1.0, cn = 55, rew = 5),
  list(texture = "loam", thickness = 1.0, cn = 70, rew = 6),
  list(texture = c("clay", "clay loam"), thickness = c(0.3, 0.7), cn = 80, rew = 7)
)
write_sol_batch(
  site_name = sites,
  params = params_list,
  path = "SOIL/",
  initial_water = "FC",
  salinity = "none"
)

# Example 3: Auto-discover all sites, default soil parameters
write_sol_batch(
  site_name = NULL,
  params = NULL,
  base_path = "/my/project/path",
  verbose = TRUE
)
} # }
```
