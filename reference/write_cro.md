# Write AquaCrop Crop File

Write an AquaCrop v7.2 (August 2024) crop (.CRO) file containing
crop-specific parameters. This file specifies physiological
characteristics, growth parameters, water stress responses, and other
crop-specific properties that will be used during the AquaCrop
simulation.

## Usage

``` r
write_cro(
  path = "CROP/",
  crop_name = "generic-crop-name",
  eol = "windows",
  params = NULL
)
```

## Arguments

- path:

  Directory path where the output .CRO file will be written. Default =
  "CROP/"

- crop_name:

  Name identifier for the crop (used in output filename). Default =
  "generic-crop-name"

- eol:

  End-of-line character style for the output file. Options: "windows",
  "linux", "macos". If `NULL` (default), eol is auto-detected.

- params:

  Named list of crop parameter values to override defaults from
  CropData. Parameter names should be var_02 through var_84 (e.g.,
  `list(var_02 = 7.0, var_35 = 1.05)`). Unspecified parameters use
  default values from CropData.

## Value

Invisibly returns the full path to the created .CRO file. Creates a
colon-delimited .CRO file in the specified directory with the format:
\<\<crop_name\>\>.CRO containing crop parameters and descriptions as
required by AquaCrop v7.2.

## See also

[`CropData`](https://mwaongo.github.io/aquacropr/reference/CropData.md)
for complete list of all 83 crop parameters with descriptions and valid
ranges

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
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
# Write default crop file
write_cro(path = "crops/", crop_name = "maize")

# Write custom crop with modified parameters
write_cro(
  path = "crops/",
  crop_name = "maize-irrigated",
  params = list(
    var_35 = 1.10, # Higher crop coefficient
    var_50 = 0.85, # Max canopy cover
    var_64 = 35 # Reference harvest index
  )
)

# Write crop with phenology parameters
write_cro(
  path = "crops/",
  crop_name = "wheat",
  params = list(
    var_52 = 10, # Days to emergence
    var_53 = 120, # Days to max rooting
    var_54 = 100, # Days to senescence
    var_55 = 130 # Days to maturity
  )
)
} # }
```
