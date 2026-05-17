# Write AquaCrop Groundwater Table (.GWT) File

Creates an AquaCrop groundwater table file (.GWT) describing the depth
and salinity of the groundwater table. Three modes are supported: no
groundwater table (code 0), constant depth and salinity (code 1), and
variable depth and salinity over time (code 2).

## Usage

``` r
write_gwt(
  site_name,
  code,
  gwt_data = NULL,
  path = "MANAGEMENT/",
  start_day = 1,
  start_month = 1,
  start_year = 1901,
  description = NULL,
  version = 7.1,
  eol = NULL
)
```

## Arguments

- site_name:

  Character. Name of the GWT file (without extension). Typically the
  station name.

- code:

  Integer. Groundwater table mode: 0 = no groundwater table, 1 =
  constant depth and salinity, 2 = variable depth and salinity.

- gwt_data:

  For code 0: ignored, can be NULL. For code 1: a data.frame with one
  row and columns:

  day

  :   Integer. Day number (must be 1 for constant mode).

  depth

  :   Numeric. Depth below soil surface in meters.

  ecw

  :   Numeric. Electrical conductivity in dS/m.

  For code 2: a data.frame with columns:

  day

  :   Integer. Day number from reference date.

  depth

  :   Numeric. Depth below soil surface in meters.

  ecw

  :   Numeric. Electrical conductivity in dS/m.

- path:

  Output directory path for GWT files. Default: "MANAGEMENT/".

- start_day:

  Integer. First day of observations (code 2 only). Default: 1.

- start_month:

  Integer. First month of observations (code 2 only). Default: 1.

- start_year:

  Integer. First year of observations (code 2 only). Use 1901 if not
  linked to a specific year. Default: 1901.

- description:

  Character. Description written on the first line. Default:
  auto-generated based on code and data.

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
# Code 0: no groundwater table
write_gwt(site_name = "station_01", code = 0)

# Code 1: constant depth and salinity
write_gwt(
  site_name = "station_01",
  code     = 1,
  gwt_data = data.frame(day = 1, depth = 1.50, ecw = 1.5)
)

# Code 2: variable depth and salinity
write_gwt(
  site_name    = "station_01",
  code        = 2,
  gwt_data    = data.frame(
    day   = c(50, 100, 200, 300),
    depth = c(1.00, 2.00, 3.00, 1.50),
    ecw   = c(1.0,  2.0,  3.0,  1.7)
  ),
  start_day   = 1,
  start_month = 1,
  start_year  = 2000
)
} # }
```
