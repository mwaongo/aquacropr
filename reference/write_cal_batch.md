# Write AquaCrop Calendar (.CAL) Files for Multiple Sites

Batch version of
[`write_cal`](https://mwaongo.github.io/aquacropr/reference/write_cal.md).
Writes one .CAL file per row of a parameter data frame or per element of
a list. A progress bar is shown when more than one file is written.

## Usage

``` r
write_cal_batch(
  params,
  path = "CAL/",
  version = 7.1,
  eol = NULL,
  climate_path = "CLIMATE/",
  base_path = getwd(),
  verbose = TRUE,
  clean = FALSE
)
```

## Arguments

- params:

  A data frame or list of named lists, each containing the arguments to
  pass to
  [`write_cal`](https://mwaongo.github.io/aquacropr/reference/write_cal.md)
  for one site. Required columns/names: `site_name`, `onset`. All other
  [`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md)
  arguments may be supplied; defaults from
  [`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md)
  apply for any column that is absent or `NA`.

- path:

  Output directory path for CAL files. Default: `"CAL/"`. Overridden per
  row if a `path` column is present in `params`.

- version:

  Numeric. AquaCrop version number. Default: 7.1. Overridden per row if
  a `version` column is present in `params`.

- eol:

  End-of-line character style. One of `"windows"`, `"linux"`, `"macos"`.
  If `NULL` (default), auto-detected. Overridden per row if an `eol`
  column is present in `params`.

- climate_path:

  Character. Path to climate files directory. Used for auto-discovery of
  stations when `site_name` is absent from `params`. Default:
  `"CLIMATE/"`.

- base_path:

  Character. Base absolute path. Default:
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- verbose:

  Logical. If `TRUE` (default), prints progress messages.

- clean:

  Logical. If `TRUE`, removes existing .CAL files from `path` before
  writing. Default: `FALSE`.

## Value

Invisibly returns a character vector of output file paths, one per site.

## See also

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
[`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md),
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md),
[`write_man()`](https://mwaongo.github.io/aquacropr/reference/write_man.md),
[`write_obs()`](https://mwaongo.github.io/aquacropr/reference/write_obs.md),
[`write_off()`](https://mwaongo.github.io/aquacropr/reference/write_off.md),
[`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_ppn()`](https://mwaongo.github.io/aquacropr/reference/write_ppn.md),
[`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
params <- data.frame(
  site_name       = c("site_A", "site_B"),
  onset           = "rainfall",
  window_start    = 121L,
  window_length   = 92L,
  criterion       = 2L,
  preset_value    = 30,
  successive_days = 3L,
  occurrences     = 1L,
  stringsAsFactors = FALSE
)
write_cal_batch(params)

# Fuzzy logic (criterion 7) for multiple sites
params_fuz <- data.frame(
  site_name       = c("site_C", "site_D"),
  onset           = "rainfall",
  window_start    = 121L,
  window_length   = 92L,
  criterion       = 7L,
  preset_value    = 20,   # cum_rain_lower
  successive_days = 3L,   # accum_days
  cum_rain_upper  = 40,
  wet_days_lower  = 1L,
  wet_days_upper  = 3L,
  dry_spell_lower = 7L,
  dry_spell_upper = 15L,
  fuzzy_threshold = 0.5,
  stringsAsFactors = FALSE
)
write_cal_batch(params_fuz)
} # }
```
