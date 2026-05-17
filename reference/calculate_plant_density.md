# Calculate Plant Population Density

Calculate the number of plants per hectare based on plant spacing and
row spacing. This is useful for determining planting density in AquaCrop
simulations and field planning.

## Usage

``` r
calculate_plant_density(plant_spacing = 0.3, row_spacing = 0.8)

plant_density(plant_spacing = 0.3, row_spacing = 0.8)
```

## Arguments

- plant_spacing:

  Numeric. Distance between plants within a row (in meters). Must be
  positive (\> 0). Default = 0.3 m

- row_spacing:

  Numeric. Distance between rows (in meters). Must be positive (\> 0).
  Default = 0.8 m

## Value

Integer representing the number of plants per hectare (plants/ha)

## Details

The calculation uses the formula: \$\$plants/ha =
\frac{10000}{plant\\spacing \times row\\spacing}\$\$

Where 10,000 m² = 1 hectare.

The result is rounded up (ceiling) to ensure you have at least the
calculated number of plants per hectare.

### Common Planting Densities:

**Maize:**

- Low density: 30,000-40,000 plants/ha (0.75m × 0.40m)

- Medium density: 50,000-70,000 plants/ha (0.75m × 0.20m)

- High density: 80,000-100,000 plants/ha (0.50m × 0.20m)

**Wheat:**

- Typical: 250-400 plants/m² = 2,500,000-4,000,000 plants/ha

**Soybean:**

- Low: 250,000-350,000 plants/ha (0.75m × 0.05m)

- High: 400,000-500,000 plants/ha (0.50m × 0.05m)

## See also

Other utility functions:
[`is_leap_year()`](https://mwaongo.github.io/aquacropr/reference/is_leap_year.md),
[`path_to_file()`](https://mwaongo.github.io/aquacropr/reference/path_to_file.md),
[`round_to()`](https://mwaongo.github.io/aquacropr/reference/round_to.md),
[`salinity_to_ece()`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md),
[`to_aquacrop_day()`](https://mwaongo.github.io/aquacropr/reference/to_aquacrop_day.md)

## Examples

``` r
# Default maize spacing (0.3m × 0.8m)
calculate_plant_density()
#> [1] 41667
# Returns: 41,667 plants/ha

# Using alias
plant_density(plant_spacing = 0.40, row_spacing = 0.75)
#> [1] 33334
# Returns: 33,334 plants/ha

# Dense maize planting
calculate_plant_density(plant_spacing = 0.20, row_spacing = 0.60)
#> [1] 83334
# Returns: 83,334 plants/ha

# Cotton spacing
plant_density(0.15, 1.0)
#> [1] 66667
# Returns: 66,667 plants/ha

# Calculate for multiple scenarios
spacings <- data.frame(
  plant_spacing = c(0.20, 0.30, 0.40),
  row_spacing = c(0.75, 0.75, 0.75)
)
spacings$plants_per_ha <- mapply(
  calculate_plant_density,
  spacings$plant_spacing,
  spacings$row_spacing
)
```
