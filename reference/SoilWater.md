# Soil Water Characteristics Based on Soil Texture

Soil hydraulic properties and water retention characteristics for 13
soil texture classes. These values are used by
[`write_sol`](https://mwaongo.github.io/aquacropr/reference/write_sol.md)
to create AquaCrop soil profile (.SOL) files with appropriate hydraulic
parameters for each texture class.

## Usage

``` r
SoilWater
```

## Format

A tibble with 13 rows and 9 variables:

- sat:

  Numeric. Saturation point (volumetric water content at saturation,
  vol%)

- fc:

  Numeric. Field capacity (volumetric water content at -33 kPa, vol%)

- wp:

  Numeric. Wilting point (volumetric water content at -1500 kPa, vol%)

- ksat:

  Numeric. Saturated hydraulic conductivity (mm/day)

- penetrability:

  Numeric. Root zone expansion rate (% per day, typically 100)

- gravel:

  Numeric. Soil gravel fraction (vol%, typically 0 for standard
  textures)

- cra:

  Numeric. Coefficient a for capillary rise calculation (dimensionless)

- crb:

  Numeric. Coefficient b for capillary rise calculation (dimensionless)

- description:

  Character. USDA soil texture class name (lowercase)

## Source

AquaCrop v7.0 (August 2022). FAO, Rome, Italy.

Values based on standard USDA soil texture classification and typical
hydraulic properties for each texture class.

## Details

### Soil Texture Classes:

The 12 standard USDA soil texture classes plus one special class:

**Coarse Textures (High drainage, low water retention):**

- sand: SAT=36%, FC=13%, WP=6%, Ksat=3000 mm/day

- loamy sand: SAT=38%, FC=16%, WP=8%, Ksat=2200 mm/day

- sandy loam: SAT=41%, FC=22%, WP=10%, Ksat=1200 mm/day

**Medium Textures (Moderate drainage and retention):**

- loam: SAT=46%, FC=31%, WP=15%, Ksat=500 mm/day

- silt loam: SAT=46%, FC=33%, WP=13%, Ksat=575 mm/day

- silt: SAT=43%, FC=33%, WP=9%, Ksat=500 mm/day

**Fine Textures (Low drainage, high water retention):**

- sandy clay loam: SAT=47%, FC=32%, WP=20%, Ksat=225 mm/day

- clay loam: SAT=50%, FC=39%, WP=23%, Ksat=125 mm/day

- silty clay loam: SAT=52%, FC=44%, WP=23%, Ksat=150 mm/day

- sandy clay: SAT=50%, FC=39%, WP=27%, Ksat=35 mm/day

- silty clay: SAT=54%, FC=50%, WP=32%, Ksat=100 mm/day

- clay: SAT=55%, FC=54%, WP=39%, Ksat=35 mm/day

**Special Class:**

- impermeable: SAT=0.5%, FC=0.3%, WP=0.1%, Ksat=0 mm/day (for
  restrictive layers)

### Capillary Rise Coefficients:

The cra and crb coefficients are used in AquaCrop's capillary rise
calculations to estimate water movement from groundwater tables to the
root zone. These are texture-specific empirical parameters.

## See also

- [`SWOData`](https://mwaongo.github.io/aquacropr/reference/SWOData.md)
  for soil initial water content defaults

- [`write_sol`](https://mwaongo.github.io/aquacropr/reference/write_sol.md)
  for creating soil profile files using this data

- [`write_swo`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
  for creating soil initial water content files

## Examples

``` r
# View all soil texture data
data(SoilWater)
SoilWater
#> # A tibble: 13 × 9
#>      sat    fc    wp  ksat penetrability gravel    cra    crb description    
#>    <dbl> <dbl> <dbl> <dbl>         <dbl>  <dbl>  <dbl>  <dbl> <chr>          
#>  1  36    13     6    3000           100      0 -0.341  0.441 sand           
#>  2  38    16     8    2200           100      0 -0.333  0.366 loamy sand     
#>  3  41    22    10    1200           100      0 -0.323  0.219 sandy loam     
#>  4  46    31    15     500           100      0 -0.454  0.837 loam           
#>  5  46    33    13     575           100      0 -0.447  0.904 silt loam      
#>  6  43    33     9     500           100      0 -0.454  0.837 silt           
#>  7  47    32    20     225           100      0 -0.577 -0.511 sandy clay loam
#>  8  50    39    23     125           100      0 -0.573 -0.860 clay loam      
#>  9  52    44    23     150           100      0 -0.517  1.62  silty clay loam
#> 10  50    39    27      35           100      0 -0.569 -1.61  sandy clay     
#> 11  54    50    32     100           100      0 -0.557  1.34  silty clay     
#> 12  55    54    39      35           100      0 -0.609  0.595 clay           
#> 13   0.5   0.3   0.1     0           100      0 -9     -9     impermeable    

# Get properties for loam soil
SoilWater[SoilWater$description == "loam", ]
#> # A tibble: 1 × 9
#>     sat    fc    wp  ksat penetrability gravel    cra   crb description
#>   <dbl> <dbl> <dbl> <dbl>         <dbl>  <dbl>  <dbl> <dbl> <chr>      
#> 1    46    31    15   500           100      0 -0.454 0.837 loam       

# Calculate available water capacity (FC - WP)
SoilWater$awc <- SoilWater$fc - SoilWater$wp

# Find textures with high Ksat (> 500 mm/day)
SoilWater[SoilWater$ksat > 500, c("description", "ksat")]
#> # A tibble: 4 × 2
#>   description  ksat
#>   <chr>       <dbl>
#> 1 sand         3000
#> 2 loamy sand   2200
#> 3 sandy loam   1200
#> 4 silt loam     575
```
