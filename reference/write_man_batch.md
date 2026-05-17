# Write AquaCrop MAN files in batch

Generate AquaCrop MAN files for multiple stations in a batch.

## Usage

``` r
write_man_batch(
  site_name = NULL,
  params = NULL,
  path = "MANAGEMENT/",
  eol = NULL,
  climate_path = "CLIMATE/",
  base_path = getwd(),
  verbose = TRUE,
  clean = FALSE
)
```

## Arguments

- site_name:

  Character vector or `NULL`. Names of stations to process:

  - If `NULL`, all stations are automatically discovered from `.CLI`
    files in the `CLIMATE/` directory.

  - If a vector, only the specified stations will be processed; all must
    have corresponding climate files.

- params:

  Either:

  - A single named `list` of management parameters (var_02 to var_21),
    applied to all stations, or

  - A `list` of named `list`s (one per station), each containing
    management parameters. The length of the list must match the number
    of stations. Set to `NULL` or
    [`list()`](https://rdrr.io/r/base/list.html) to use all default
    values for all stations. See
    [`ManData`](https://mwaongo.github.io/aquacropr/reference/ManData.md)
    documentation for further details.

- path:

  Output directory path. Default: `"MANAGEMENT/"`.

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected.

- climate_path:

  Path to climate file directory for auto-discovery. Default:
  `"CLIMATE/"`.

- base_path:

  Base absolute path. Default: current working directory.

- verbose:

  Logical. If `TRUE` (default), prints progress messages. If `FALSE`,
  runs silently.

- clean:

  Logical. If `TRUE`, removes existing `.MAN` files from `path` before
  writing new files. Default: `FALSE`.

## Value

Invisibly returns `NULL`. The main effect is writing MAN files to the
specified directory.

## Details

The function validates that all specified stations have corresponding
climate files.

A single `list` of parameters will be automatically applied to all
stations.

Valid parameters include var_02 through var_21. See
[`ManData`](https://mwaongo.github.io/aquacropr/reference/ManData.md)
documentation for further details on parameter descriptions and valid
ranges.

Use this function when you need to create MAN files for **multiple
stations at once**.

## See also

[`write_man`](https://mwaongo.github.io/aquacropr/reference/write_man.md)
for single management file generation,
[`write_prm_batch`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md)
for batch PRM file generation,
[`ManData`](https://mwaongo.github.io/aquacropr/reference/ManData.md)
for complete parameter reference

Other batch operations:
[`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md),
[`write_irrig_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irrig_batch.md),
[`write_obs_batch()`](https://mwaongo.github.io/aquacropr/reference/write_obs_batch.md),
[`write_off_batch()`](https://mwaongo.github.io/aquacropr/reference/write_off_batch.md),
[`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md),
[`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Multiple stations, same management parameters
base_params <- list(var_03 = 20, var_04 = 60, var_05 = 50)
stations <- c("grid_001", "grid_002")
write_man_batch(
  site_name = stations,
  params = base_params
)

# Example 2: Multiple stations, different management parameters
params_list <- list(
  list(var_03 = 0, var_04 = 0, var_05 = 30), # Low fertility, no mulch
  list(var_03 = 60, var_04 = 80, var_05 = 70) # High fertility, heavy mulch
)
write_man_batch(
  site_name = stations,
  params = params_list,
  path = "MANAGEMENT/"
)

# Example 3: Auto-discover all stations, default parameters
write_man_batch(
  site_name = NULL,
  params = NULL,
  base_path = "/my/project/path",
  verbose = FALSE
)

# Example 4: Station-specific fertility gradient
params_list <- list(
  list(var_05 = 30), # Station 1: low fertility
  list(var_05 = 60), # Station 2: medium fertility
  list(var_05 = 90) # Station 3: high fertility
)
write_man_batch(
  site_name = c("site_A", "site_B", "site_C"),
  params = params_list
)
} # }
```
