# Soil Initial Water Content Default Values

Default wilting point (WP) and electrical conductivity (ECe) values for
12 standard USDA soil texture classes. These values are used by
[`write_swo`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
to automatically set initial soil water conditions based on soil texture
when creating soil initial water content (.SW0) files.

## Usage

``` r
SWOData
```

## Format

A data frame with 12 rows and 3 variables:

- texture:

  Character. USDA soil texture class name (lowercase)

- wp:

  Numeric. Wilting point water content (volumetric %, vol%)

- ece:

  Numeric. Electrical conductivity (dS/m, default 0 for non-saline
  soils)

## Source

AquaCrop v7.0 (August 2022). FAO, Rome, Italy.

Wilting point values represent typical volumetric water content at -1500
kPa soil water potential for each USDA soil texture class.

## Details

### Soil Texture Classes and Wilting Points:

The 12 standard USDA soil texture classes with their respective wilting
points:

**Coarse Textures (Low water retention at WP):**

- sand: WP = 6%

- loamy sand: WP = 8%

- sandy loam: WP = 10%

**Medium Textures (Moderate water retention at WP):**

- loam: WP = 15%

- silt loam: WP = 13%

- silt: WP = 9%

**Fine Textures (High water retention at WP):**

- sandy clay loam: WP = 20%

- clay loam: WP = 23%

- silty clay loam: WP = 23%

- sandy clay: WP = 27%

- silty clay: WP = 32%

- clay: WP = 39%

### Water Content Relationships:

From the wilting point (WP), other soil moisture levels can be
estimated:

- **Wilting Point (WP)**: Minimum water for plant survival (-1500 kPa)

- **Field Capacity (FC)**: WP × 1.5 (approximate, -33 kPa)

- **Saturation (SAT)**: WP × 2.5 (approximate, 0 kPa)

### Electrical Conductivity:

Default ECe values are 0 dS/m (non-saline). Typical ECe ranges:

- 0-2 dS/m: Non-saline (most crops unaffected)

- 2-4 dS/m: Slightly saline (sensitive crops affected)

- 4-8 dS/m: Moderately saline (many crops affected)

- 8-16 dS/m: Strongly saline (only tolerant crops)

- \>16 dS/m: Very strongly saline (few crops tolerate)

## See also

- [`SoilWater`](https://mwaongo.github.io/aquacropr/reference/SoilWater.md)
  for complete soil hydraulic properties

- [`write_swo`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
  for creating soil initial water content files using this data

- [`salinity_to_ece`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md)
  for converting salinity descriptions to ECe values

## Examples

``` r
# View all soil texture data
data(SWOData)
SWOData
#>            texture   wp ece
#> 1             sand  6.0   0
#> 2       loamy sand  8.0   0
#> 3       sandy loam 10.0   0
#> 4             loam 15.0   0
#> 5        silt loam 13.0   0
#> 6             silt  9.0   0
#> 7  sandy clay loam 20.0   0
#> 8        clay loam 23.0   0
#> 9  silty clay loam 23.0   0
#> 10      sandy clay 27.0   0
#> 11      silty clay 32.0   0
#> 12            clay 39.0   0
#> 13     impermeable  0.1   0

# Get wilting point for loam
SWOData[SWOData$texture == "loam", "wp"]
#> [1] 15

# Calculate field capacity for all textures
SWOData$fc_approx <- SWOData$wp * 1.5

# Find textures with WP > 20%
SWOData[SWOData$wp > 20, ]
#>            texture wp ece fc_approx
#> 8        clay loam 23   0      34.5
#> 9  silty clay loam 23   0      34.5
#> 10      sandy clay 27   0      40.5
#> 11      silty clay 32   0      48.0
#> 12            clay 39   0      58.5

# Compare with SoilWater data
data(SoilWater)
merge(SWOData, SoilWater[, c("description", "fc", "sat")],
  by.x = "texture", by.y = "description"
)
#>            texture   wp ece fc_approx   fc  sat
#> 1             clay 39.0   0     58.50 54.0 55.0
#> 2        clay loam 23.0   0     34.50 39.0 50.0
#> 3      impermeable  0.1   0      0.15  0.3  0.5
#> 4             loam 15.0   0     22.50 31.0 46.0
#> 5       loamy sand  8.0   0     12.00 16.0 38.0
#> 6             sand  6.0   0      9.00 13.0 36.0
#> 7       sandy clay 27.0   0     40.50 39.0 50.0
#> 8  sandy clay loam 20.0   0     30.00 32.0 47.0
#> 9       sandy loam 10.0   0     15.00 22.0 41.0
#> 10            silt  9.0   0     13.50 33.0 43.0
#> 11       silt loam 13.0   0     19.50 33.0 46.0
#> 12      silty clay 32.0   0     48.00 50.0 54.0
#> 13 silty clay loam 23.0   0     34.50 44.0 52.0
```
