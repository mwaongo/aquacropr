# Write AquaCrop Observed Data (.OBS) File

Creates an AquaCrop observed data file (.OBS) containing measured canopy
cover, dry biomass, and soil water content for model calibration and
validation. Missing values must be set to -9.0. Entire columns can be
-9.0 when a variable was not measured.

## Usage

``` r
write_obs(
  site_name,
  obs_data,
  path = "OBS/",
  soil_depth = 1,
  start_day = 1,
  start_month = 1,
  start_year = 1901,
  version = 7.1,
  eol = NULL
)
```

## Arguments

- site_name:

  Character. Name of the OBS file (without extension). Typically the
  station name.

- obs_data:

  data.frame. Observed data with columns:

  day

  :   Integer. Day number from reference date.

  cc_mean

  :   Numeric. Mean canopy cover in percent. Use -9.0 if missing.

  cc_std

  :   Numeric. Standard deviation of canopy cover. Use -9.0 if missing.

  biomass_mean

  :   Numeric. Mean dry biomass in ton/ha. Use -9.0 if missing.

  biomass_std

  :   Numeric. Standard deviation of dry biomass. Use -9.0 if missing.

  swc_mean

  :   Numeric. Mean soil water content in mm. Use -9.0 if missing.

  swc_std

  :   Numeric. Standard deviation of soil water content. Use -9.0 if
      missing.

- path:

  Output directory path for OBS files. Default: "OBS/".

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

## Value

Invisibly returns the output file path.

## See also

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
[`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md),
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md),
[`write_man()`](https://mwaongo.github.io/aquacropr/reference/write_man.md),
[`write_off()`](https://mwaongo.github.io/aquacropr/reference/write_off.md),
[`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_ppn()`](https://mwaongo.github.io/aquacropr/reference/write_ppn.md),
[`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

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

write_obs(
  site_name    = "Ottawa",
  obs_data    = obs,
  soil_depth  = 1.00,
  start_day   = 1,
  start_month = 1,
  start_year  = 2014
)
} # }
```
