# Convert Salinity Level to ECe Value

Convert qualitative salinity descriptions to quantitative electrical
conductivity (ECe) values in dS/m. Uses standard salinity classification
ranges.

## Usage

``` r
salinity_to_ece(salinity)
```

## Arguments

- salinity:

  Character string or numeric. Options:

  - **"none"** or **"non-saline"**: 0 dS/m

  - **"very slight"**: 1 dS/m

  - **"slight"** or **"slightly saline"**: 3 dS/m

  - **"moderate"** or **"moderately saline"**: 6 dS/m

  - **"strong"** or **"strongly saline"**: 12 dS/m

  - **"very strong"** or **"very strongly saline"**: 20 dS/m

  - **Numeric value**: Used directly (0-100 dS/m)

## Value

Numeric value representing electrical conductivity in dS/m

## Details

Standard salinity classification:

- 0-2 dS/m: Non-saline (most crops unaffected)

- 2-4 dS/m: Slightly saline (sensitive crops affected)

- 4-8 dS/m: Moderately saline (many crops affected)

- 8-16 dS/m: Strongly saline (only tolerant crops grow)

- \>16 dS/m: Very strongly saline (few crops tolerate)

## See also

Other utility functions:
[`calculate_plant_density()`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md),
[`is_leap_year()`](https://mwaongo.github.io/aquacropr/reference/is_leap_year.md),
[`path_to_file()`](https://mwaongo.github.io/aquacropr/reference/path_to_file.md),
[`round_to()`](https://mwaongo.github.io/aquacropr/reference/round_to.md),
[`to_aquacrop_day()`](https://mwaongo.github.io/aquacropr/reference/to_aquacrop_day.md)

## Examples

``` r
salinity_to_ece("none") # 0
#> [1] 0
salinity_to_ece("slightly saline") # 3
#> [1] 3
salinity_to_ece("moderate") # 6
#> [1] 6
salinity_to_ece(5.5) # 5.5
#> [1] 5.5
```
