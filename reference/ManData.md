# AquaCrop Management Parameters

Reference dataset containing all management parameters for AquaCrop v7.0
(August 2022). This dataset defines the structure and default values for
management (.MAN) files, which specify field and crop management
practices.

## Usage

``` r
ManData
```

## Format

A data frame with 20 rows and 5 columns:

- name:

  Parameter name (var_02 to var_21)

- value:

  Default parameter value

- description:

  Description of the parameter

- fmt:

  Format string for output (printf-style)

- width:

  Column width for formatted output

## Source

AquacropV7.0 (Août 2022) .FAO 2022. Rome, Italy

## Details

### Parameter Categories

**General Settings (var_02)**

- var_02: AquaCrop Version (7.0)

**Mulch Management (var_03-var_04)**

- var_03: Ground surface covered by mulches (0-100%)

- var_04: Effect of mulches on reducing soil evaporation (0-100%)

**Soil Fertility (var_05)**

- var_05: Degree of soil fertility stress (0-100%, 0 = no stress,
  crop-specific effect)

**Field Surface Practices (var_06-var_08)**

- var_06: Height of soil bunds (m, 0+ for no bunds)

- var_07: Surface runoff not affected by practices (0-100%)

- var_08: Surface runoff completely prevented (0-100%)

**Weed Management (var_09-var_12)**

- var_09: Relative cover of weeds at canopy closure (0-100%)

- var_10: Increase of relative weed cover in mid-season (0+ %)

- var_11: Shape factor of CC expansion function in weed-infested field

- var_12: Replacement by weeds of self-thinned CC part for perennials
  (0-100%)

**Multiple Cuttings (var_13-var_19)**

- var_13: Multiple cuttings consideration (0 = not considered, 1 =
  considered)

- var_14: Canopy cover after cutting (0-100%)

- var_15: Increase of Canopy Growth Coefficient after cutting (0+ %)

- var_16: First day of cutting window (1+ days, 1 = start of growth
  cycle)

- var_17: Number of days in cutting window (-9 = total cycle)

- var_18: Timing of multiple cuttings

- var_19: Time criterion for cuttings

**Harvest Management (var_20-var_21)**

- var_20: Final harvest at maturity (0 = not considered, 1 = considered)

- var_21: Start of growing cycle in cuttings list (1+ or -9)

### Default Values:

The default values represent a baseline management scenario with:

- No mulching (var_03 = 0%)

- Moderate soil fertility stress (var_05 = 42%)

- No soil bunds (var_06 = 0 m)

- Moderate weed pressure (var_09 = 25%)

- No multiple cuttings (var_13 = 0)

- No special harvest timing (var_20 = 0)

### Special Values:

- `-9`: Indicates "not applicable" or "use default" for certain
  parameters

- `0`: Typically means "not considered" or "no effect" for management
  practices

- `1`: Start of growth cycle for timing parameters

## See also

[`write_man`](https://mwaongo.github.io/aquacropr/reference/write_man.md)
for writing management files using these parameters,
[`CropData`](https://mwaongo.github.io/aquacropr/reference/CropData.md)
for crop parameter metadata

## Examples

``` r
# View available management parameters
data("ManData")
head(ManData)
#> # A tibble: 6 × 5
#>   name   value description                                           fmt   width
#>   <chr>  <dbl> <chr>                                                 <chr> <dbl>
#> 1 var_02     7 AquaCrop Version (August 2022)                        %.1f     15
#> 2 var_03     0 percentage (%) of ground surface covered by mulches … %.0f     15
#> 3 var_04    50 effect (%) of mulches on reduction of soil evaporati… %.0f     15
#> 4 var_05    42 Degree of soil fertility stress (%) - Effect is crop… %.0f     15
#> 5 var_06     0 height (m) of soil bunds                              %.2f     15
#> 6 var_07     0 surface runoff NOT affected by field surface practic… %.0f     15

# Find specific parameters
ManData[ManData$name == "var_05", ] # Soil fertility stress
#> # A tibble: 1 × 5
#>   name   value description                                           fmt   width
#>   <chr>  <dbl> <chr>                                                 <chr> <dbl>
#> 1 var_05    42 Degree of soil fertility stress (%) - Effect is crop… %.0f     15
ManData[ManData$name == "var_03", ] # Mulch coverage
#> # A tibble: 1 × 5
#>   name   value description                                           fmt   width
#>   <chr>  <dbl> <chr>                                                 <chr> <dbl>
#> 1 var_03     0 percentage (%) of ground surface covered by mulches … %.0f     15

# See all mulch-related parameters
ManData[3:4, ]
#> # A tibble: 2 × 5
#>   name   value description                                           fmt   width
#>   <chr>  <dbl> <chr>                                                 <chr> <dbl>
#> 1 var_04    50 effect (%) of mulches on reduction of soil evaporati… %.0f     15
#> 2 var_05    42 Degree of soil fertility stress (%) - Effect is crop… %.0f     15

# See all weed-related parameters
ManData[8:11, ]
#> # A tibble: 4 × 5
#>   name    value description                                          fmt   width
#>   <chr>   <dbl> <chr>                                                <chr> <dbl>
#> 1 var_09  25    relative cover of weeds at canopy closure (%)        %.0f     15
#> 2 var_10   0    increase of relative cover of weeds in mid-season (… %.0f     15
#> 3 var_11  -0.01 shape factor of the CC expansion function in a weed… %.2f     15
#> 4 var_12 100    replacement (%) by weeds of the self-thinned part o… %.0f     15

# See all cutting-related parameters
ManData[13:19, ]
#> # A tibble: 7 × 5
#>   name   value description                                           fmt   width
#>   <chr>  <dbl> <chr>                                                 <chr> <dbl>
#> 1 var_14    30 Canopy cover (%) after cutting - not considered       %.0f     15
#> 2 var_15    20 Increase (%) of Canopy Growth Coefficient (CGC) afte… %.0f     15
#> 3 var_16     1 First day of window for multiple cuttings (1 = start… %.0f     15
#> 4 var_17    -9 Number of days in window for multiple cuttings (-9 =… %.0f     15
#> 5 var_18    -9 Timing of multiple cuttings: Not Applicable           %.0f     15
#> 6 var_19     0 Time criterion: Not Applicable                        %.0f     15
#> 7 var_20     0 final harvest at crop maturity is not considered      %.0f     15
```
