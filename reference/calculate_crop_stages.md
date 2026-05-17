# Get Crop Stages Lengths and Calendar

Compute crop stage lengths and calendar from crop cycle length. This
function calculates the duration of different phenological stages based
on the total crop cycle length, scaled from a reference 90-day cycle.

## Usage

``` r
calculate_crop_stages(crop_cycle = 90)
```

## Arguments

- crop_cycle:

  Positive integer representing the total length of the crop cycle in
  days. Must be a whole number \> 0. Default = 90 days.

## Value

A named list of six integer values (var_52 through var_57) representing
crop stage durations in days. This list can be passed directly to
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md)
via the `params` argument to create a crop file with custom phenology.

## Details

The function calculates six crop stage variables based on proportions
from a reference 90-day crop cycle:

- **var_52**: Days from sowing to emergence (8/90 of cycle = ~9%)

- **var_53**: Days from sowing to maximum canopy cover (100% of cycle)

- **var_54**: Days from sowing to start of canopy senescence (65/90 of
  cycle = ~72%)

- **var_55**: Days from sowing to maturity (100% of cycle)

- **var_56**: Days from sowing to start of yield formation (52/90 of
  cycle = ~58%)

- **var_57**: Duration of yield formation period (20/90 of cycle = ~22%)

All values are rounded to the nearest integer day.

## Examples

``` r
# Default 90-day cycle
calculate_crop_stages()
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
#> attr(,"description")
#>                     var_52                     var_53 
#>        "Days to emergence"   "Days to maximum canopy" 
#>                     var_54                     var_55 
#>       "Days to senescence"         "Days to maturity" 
#>                     var_56                     var_57 
#>  "Days to yield formation" "Yield formation duration" 

# 120-day maize cycle
maize_stages <- calculate_crop_stages(crop_cycle = 120)

# Use with write_cro to create a crop file
if (FALSE) { # \dontrun{
stages <- calculate_crop_stages(crop_cycle = 130)
write_cro(
  path = "crop/",
  crop_name = "maize-130d",
  params = stages
)
} # }
```
