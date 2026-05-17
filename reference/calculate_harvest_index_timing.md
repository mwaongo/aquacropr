# Calculate Harvest Index Building Parameters

Calculate the timing parameters for harvest index (HI) building in
AquaCrop. The harvest index represents the ratio of harvested yield to
total above-ground biomass.

## Usage

``` r
calculate_harvest_index_timing(flowering_date = 35)
```

## Arguments

- flowering_date:

  Integer. Number of days from sowing/planting to flowering. Must be
  positive. Default = 35 days

## Value

Named list with two elements:

- var_60:

  Days to start of HI building

- var_64:

  Days to start of linear HI increase

## Details

This function calculates two key AquaCrop parameters:

- **var_60**: Start of harvest index building (days from sowing) Set
  equal to flowering date

- **var_64**: Start of linear harvest index increase (days from sowing)
  Set 5 days before flowering (flowering_date - 5)

The harvest index typically starts building around flowering and
increases linearly during grain filling until maturity.

## Examples

``` r
# Default maize flowering (35 days)
calculate_harvest_index_timing(35)
#> $var_60
#> [1] 35
#> 
#> $var_64
#> [1] 30
#> 
# Returns: list(var_60 = 35, var_64 = 30)

# Wheat flowering (90 days)
calculate_harvest_index_timing(90)
#> $var_60
#> [1] 90
#> 
#> $var_64
#> [1] 85
#> 
# Returns: list(var_60 = 90, var_64 = 85)
```
