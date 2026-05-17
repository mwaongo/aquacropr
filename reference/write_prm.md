# Write AquaCrop Parameter (.PRM) Files

Creates a `.PRM` file for one site across multiple years. The planting
schedule can be supplied directly or derived from a `.CAL` file via
[`find_onset`](https://mwaongo.github.io/aquacropr/reference/find_onset.md).

## Usage

``` r
write_prm(
  site_name,
  path = "LIST/",
  planting_schedule = NULL,
  crop_name = NULL,
  crop_path = "CROP/",
  crop_duration = NULL,
  climate_path = "CLIMATE/",
  calendar_path = NULL,
  management_path = "MANAGEMENT/",
  irrigation_path = NULL,
  soil_path = "SOIL/",
  groundwater_path = NULL,
  offseason_path = NULL,
  obs_path = NULL,
  simulation_start_doy = NULL,
  scenario = "hist",
  eol = "windows",
  use_standalone = TRUE,
  base_path = getwd()
)
```

## Arguments

- site_name:

  Character. Station name used to locate climate, soil, management, and
  optional auxiliary files.

- path:

  Output directory path for PRM files. Default: `"LIST/"`.

- planting_schedule:

  A `data.frame` with columns `year` and `planting_doy`. Required when
  `calendar_path` is `NULL`.

- crop_name:

  Character. Crop name (without extension).

- crop_path:

  Character. Path to the crop file directory. Default: `"CROP/"`.

- crop_duration:

  Integer. Growing season length in days. Default: 90.

- climate_path:

  Character. Path to climate file directory. Default: `"CLIMATE/"`.

- calendar_path:

  Character or NULL. Path to the CAL directory. When provided,
  [`find_onset()`](https://mwaongo.github.io/aquacropr/reference/find_onset.md)
  is called to derive `planting_schedule`; any supplied
  `planting_schedule` is ignored with a warning.

- management_path:

  Character. Path to management file directory. Default:
  `"MANAGEMENT/"`.

- irrigation_path:

  Character or NULL. Path to the IRR file directory. If NULL (default),
  the section is written as `(None)`

- soil_path:

  Character. Path to soil file directory. Default: `"SOIL/"`.

- groundwater_path:

  Character or NULL. Path to the GWT file directory. Default: `NULL`.

- offseason_path:

  Character or NULL. Path to the OFF file directory. If the `.OFF` file
  is not found, a warning is issued and the section is written as
  `(None)`. Default: `NULL`.

- obs_path:

  Character or NULL. Path to the OBS file directory. If the `.OBS` file
  is not found, a warning is issued and the section is written as
  `(None)`. Default: `NULL`.

- simulation_start_doy:

  Integer or NULL. First DOY of simulation. Default: `NULL` (April 1st).

- scenario:

  Character. Climate scenario identifier. Default: `"hist"`.

- eol:

  Character. End-of-line style: `"windows"`, `"linux"`, or `"macos"`.
  Default: `"windows"`.

- use_standalone:

  Logical. Whether paths are formatted for AquaCrop standalone mode.
  Default: `TRUE`.

- base_path:

  Character. Base absolute path. Default:
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

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
[`write_off()`](https://mwaongo.github.io/aquacropr/reference/write_off.md),
[`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_ppn()`](https://mwaongo.github.io/aquacropr/reference/write_ppn.md),
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
