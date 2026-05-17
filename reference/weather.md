# Example Weather Data for AquaCrop

Daily weather data from Wakanda (fictional location) Automatic Weather
Station. Contains 30 years of climate records (1976-2005) suitable for
testing and demonstrating aquacropr functions.

## Usage

``` r
weather
```

## Format

A tibble with 10,958 rows and 8 variables:

- year:

  Year of observation (1976-2005)

- month:

  Month of observation (1-12, January-December)

- day:

  Day of observation (1-31, depending on month)

- tmin:

  Minimum daily temperature (°C)

- tmax:

  Maximum daily temperature (°C)

- rain:

  Daily rainfall amount (mm)

- et0:

  Daily reference evapotranspiration - ETo (mm/day)

## Details

This dataset provides complete daily weather records for AquaCrop
simulations, including:

- Temperature data for calculating growing degree days and thermal
  stress

- Rainfall data for water balance calculations

- Reference evapotranspiration (ETo) for crop water requirements

The data covers a full 30-year period, allowing for:

- Long-term climate analysis

- Multiple growing season simulations

- Climate variability assessments

## Note

- All temperature values are in degrees Celsius (°C)

- Rainfall and ETo values are in millimeters (mm or mm/day)

- ETo represents reference evapotranspiration calculated using
  standardized methods

- Data is continuous with no missing values

## See also

[`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md)
for writing rainfall files,
[`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md)
for writing ETo files,
[`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
for writing temperature files

## Examples

``` r
# Load the weather data
data("weather")
head(weather)
#> # A tibble: 6 × 8
#>   station  year month   day  tmin  tmax  rain   et0
#>   <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 wakanda  1976     1     1   8.9  28.2     0   5.7
#> 2 wakanda  1976     1     2   8.6  28       0   5.9
#> 3 wakanda  1976     1     3  12    26.5     0   6.9
#> 4 wakanda  1976     1     4   9.4  22.5     0   6.4
#> 5 wakanda  1976     1     5   7.6  23.9     0   6  
#> 6 wakanda  1976     1     6   7.9  25.5     0   6  

# Summary statistics
summary(weather[, c("tmin", "tmax", "rain", "et0")])
#>       tmin            tmax            rain             et0        
#>  Min.   : 6.50   Min.   :22.50   Min.   : 0.000   Min.   : 1.300  
#>  1st Qu.:14.90   1st Qu.:32.90   1st Qu.: 0.000   1st Qu.: 5.400  
#>  Median :19.30   Median :35.40   Median : 0.000   Median : 6.200  
#>  Mean   :18.46   Mean   :35.35   Mean   : 1.358   Mean   : 6.337  
#>  3rd Qu.:22.20   3rd Qu.:38.00   3rd Qu.: 1.000   3rd Qu.: 7.100  
#>  Max.   :29.80   Max.   :43.80   Max.   :72.900   Max.   :11.600  

# Filter data for a specific year
weather_2000 <- weather[weather$year == 2000, ]

# Calculate annual rainfall
annual_rain <- aggregate(rain ~ year, data = weather, FUN = sum)
```
