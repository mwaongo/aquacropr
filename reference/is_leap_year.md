# Check if a Year is a Leap Year

Check if a Year is a Leap Year

## Usage

``` r
is_leap_year(x)
```

## Arguments

- x:

  A numeric scalar representing a year

## Value

A logical scalar (TRUE if leap year, FALSE otherwise)

## See also

Other utility functions:
[`calculate_plant_density()`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md),
[`path_to_file()`](https://mwaongo.github.io/aquacropr/reference/path_to_file.md),
[`round_to()`](https://mwaongo.github.io/aquacropr/reference/round_to.md),
[`salinity_to_ece()`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md),
[`to_aquacrop_day()`](https://mwaongo.github.io/aquacropr/reference/to_aquacrop_day.md)

## Examples

``` r
is_leap_year(2020)
#> [1] TRUE
is_leap_year(2021)
#> [1] FALSE
```
