# Get Salinity Description from ECe Value

Convert electrical conductivity (ECe) values to qualitative salinity
descriptions.

## Usage

``` r
ece_to_salinity(ece)
```

## Arguments

- ece:

  Numeric. Electrical conductivity in dS/m

## Value

Character string describing the salinity level

## Examples

``` r
ece_to_salinity(0) # "Non-saline"
#> [1] "Non-saline"
ece_to_salinity(3.5) # "Slightly saline"
#> [1] "Slightly saline"
ece_to_salinity(10) # "Strongly saline"
#> [1] "Strongly saline"
```
