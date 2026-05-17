# Write AquaCrop Off-Season Conditions (.OFF) File

Creates an AquaCrop off-season conditions file (.OFF) describing field
management (mulches) and irrigation events before and after the growing
cycle.

## Usage

``` r
write_off(
  site_name,
  mulch_before = 0,
  mulch_after = 0,
  mulch_effect = 50,
  ecw_before = 0,
  ecw_after = 0,
  wet_surface = 100,
  irr_before = NULL,
  irr_after = NULL,
  path = "MANAGEMENT/",
  description = "Field and irrigation management conditions in the off-season",
  version = 7.1,
  eol = NULL
)
```

## Arguments

- site_name:

  Character. Name of the OFF file (without extension). Typically the
  station name.

- mulch_before:

  Integer. Percentage of ground surface covered by mulches before the
  growing period (0-100). Default: 0.

- mulch_after:

  Integer. Percentage of ground surface covered by mulches after the
  growing period (0-100). Default: 0.

- mulch_effect:

  Integer. Effect of mulches on reduction of soil evaporation in
  percent. Indicative values: 100 for synthetic plastic mulches, 50 for
  organic mulches, 10-100 for user-specified. Default: 50.

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

  data.frame or NULL. Irrigation events before the growing period. Must
  have columns:

  day

  :   Integer. Number of days after start of simulation period.

  depth

  :   Integer. Net irrigation application depth in mm.

  Maximum 5 rows. Default: NULL (no events).

- irr_after:

  data.frame or NULL. Irrigation events after the growing period. Must
  have columns:

  day

  :   Integer. Number of days after end of growing period.

  depth

  :   Integer. Net irrigation application depth in mm.

  Maximum 5 rows. Default: NULL (no events).

- path:

  Output directory path for OFF files. Default: "MANAGEMENT/".

- description:

  Character. Description written on the first line. Default: "Field and
  irrigation management conditions in the off-season".

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
[`write_obs()`](https://mwaongo.github.io/aquacropr/reference/write_obs.md),
[`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_ppn()`](https://mwaongo.github.io/aquacropr/reference/write_ppn.md),
[`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# No mulches, one irrigation event before season
write_off(
  site_name   = "station_01",
  mulch_after = 70,
  mulch_effect = 50,
  ecw_before = 1.5,
  ecw_after  = 3.0,
  irr_before = data.frame(day = 10, depth = 40)
)

# Mulches and irrigation events both before and after
write_off(
  site_name     = "station_01",
  mulch_before = 50,
  mulch_after  = 70,
  mulch_effect = 100,
  ecw_before   = 1.5,
  ecw_after    = 3.0,
  irr_before   = data.frame(day = c(5, 10), depth = c(30, 40)),
  irr_after    = data.frame(day = c(5),     depth = c(25))
)
} # }
```
