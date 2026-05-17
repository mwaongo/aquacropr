# Build a Crop Parameter Set

Generate a complete set of crop parameters for AquaCrop by combining
crop phenology, harvest index timing, plant density, and other key
parameters. This is a convenience function for creating custom crop
files.

## Usage

``` r
build_crop_parameters(
  crop_cycle = 90,
  flowering_date = 35,
  plant_spacing = 0.5,
  row_spacing = 0.8,
  var_28 = 40,
  var_38 = 1,
  var_08 = 8,
  var_09 = 30
)

crop_params(
  crop_cycle = 90,
  flowering_date = 35,
  plant_spacing = 0.5,
  row_spacing = 0.8,
  var_28 = 40,
  var_38 = 1,
  var_08 = 8,
  var_09 = 30
)
```

## Arguments

- crop_cycle:

  Integer. Total crop cycle length in days. Default = 90

- flowering_date:

  Integer. Days from sowing to flowering. Default = 35

- plant_spacing:

  Numeric. Distance between plants within rows (m). Default = 0.5

- row_spacing:

  Numeric. Distance between rows (m). Default = 0.8

- var_28:

  Numeric. Minimum canopy cover (%) below which yield formation stops.
  Default = 40

- var_38:

  Numeric. Soil fertility stress coefficient (0-1). Default = 1 (no
  stress)

- var_08:

  Numeric. Base temperature (deg C) below which no growth occurs.
  Default = 8.0

- var_09:

  Numeric. Upper temperature (deg C) above which growth is optimal.
  Default = 30

## Value

Named list of crop parameters suitable for passing to
[`write_cro`](https://mwaongo.github.io/aquacropr/reference/write_cro.md)

## Details

This function combines outputs from:

- [`calculate_crop_stages`](https://mwaongo.github.io/aquacropr/reference/calculate_crop_stages.md):
  Phenological stages (var_52-var_57)

- [`calculate_harvest_index_timing`](https://mwaongo.github.io/aquacropr/reference/calculate_harvest_index_timing.md):
  HI timing (var_60, var_64)

- [`calculate_plant_density`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md):
  Plant population (var_45)

- Additional key parameters provided as arguments

All parameters are sorted by variable name (var_XX) for consistency with
AquaCrop file format.

## See also

- [`calculate_crop_stages`](https://mwaongo.github.io/aquacropr/reference/calculate_crop_stages.md)
  for phenology parameters

- [`calculate_harvest_index_timing`](https://mwaongo.github.io/aquacropr/reference/calculate_harvest_index_timing.md)
  for HI parameters

- [`calculate_plant_density`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md)
  for plant population

- [`write_cro`](https://mwaongo.github.io/aquacropr/reference/write_cro.md)
  for writing crop files

## Examples

``` r
# Default maize parameters
build_crop_parameters()
#> $var_08
#> [1] 8
#> 
#> $var_09
#> [1] 30
#> 
#> $var_28
#> [1] 40
#> 
#> $var_38
#> [1] 1
#> 
#> $var_45
#> [1] 25000
#> 
#> $var_52
#> [1] 8
#> 
#> $var_53
#> [1] 90
#> 
#> $var_54
#> [1] 65
#> 
#> $var_55
#> [1] 90
#> 
#> $var_56
#> [1] 52
#> 
#> $var_57
#> [1] 20
#> 
#> $var_60
#> [1] 35
#> 
#> $var_64
#> [1] 30
#> 

# Custom wheat parameters
build_crop_parameters(
  crop_cycle = 150,
  flowering_date = 90,
  plant_spacing = 0.05,
  row_spacing = 0.20,
  var_08 = 0,
  var_09 = 26
)
#> $var_08
#> [1] 0
#> 
#> $var_09
#> [1] 26
#> 
#> $var_28
#> [1] 40
#> 
#> $var_38
#> [1] 1
#> 
#> $var_45
#> [1] 1000000
#> 
#> $var_52
#> [1] 13
#> 
#> $var_53
#> [1] 150
#> 
#> $var_54
#> [1] 108
#> 
#> $var_55
#> [1] 150
#> 
#> $var_56
#> [1] 87
#> 
#> $var_57
#> [1] 33
#> 
#> $var_60
#> [1] 90
#> 
#> $var_64
#> [1] 85
#> 

# Use with write_cro
if (FALSE) { # \dontrun{
params <- build_crop_parameters(
  crop_cycle = 120,
  flowering_date = 60
)

write_cro(
  crop_name = "custom-maize",
  params = params
)
} # }
```
