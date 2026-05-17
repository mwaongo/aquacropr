# Convert Dates to AquaCrop Day Numbers

Convert calendar dates to AquaCrop's internal day numbering system,
which counts days elapsed since January 1, 1901 (day 1). This generic
function has methods for different input types: numeric components,
character strings, and Date objects.

## Usage

``` r
to_aquacrop_day(x, ...)

# S3 method for class 'numeric'
to_aquacrop_day(x, month, day, ...)

# S3 method for class 'character'
to_aquacrop_day(x, ...)

# S3 method for class 'Date'
to_aquacrop_day(x, ...)

# S3 method for class 'POSIXt'
to_aquacrop_day(x, ...)

# Default S3 method
to_aquacrop_day(x, ...)
```

## Arguments

- x:

  Input to convert. Can be:

  - Numeric: year (when using with month and day parameters)

  - Character: date string in format "yyyy-mm-dd"

  - Date: Date object from base R or lubridate

- ...:

  Additional arguments passed to methods

- month:

  Integer representing the month (1-12)

- day:

  Integer representing the day of month (1-31)

## Value

Integer representing the AquaCrop day number (days since Jan 1, 1901)

## Details

AquaCrop uses a continuous day numbering system starting from January 1,
1901 (day 1). The calculation accounts for leap years using a 365.25-day
average year length.

## See also

Other utility functions:
[`calculate_plant_density()`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md),
[`is_leap_year()`](https://mwaongo.github.io/aquacropr/reference/is_leap_year.md),
[`path_to_file()`](https://mwaongo.github.io/aquacropr/reference/path_to_file.md),
[`round_to()`](https://mwaongo.github.io/aquacropr/reference/round_to.md),
[`salinity_to_ece()`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md)

## Examples

``` r
# Method 1: Numeric components
to_aquacrop_day(2023, 6, 15)
#> [1] 44726

# Method 2: Character string
to_aquacrop_day("2023-06-15")
#> [1] 44726

# Method 3: Date object
to_aquacrop_day(as.Date("2023-06-15"))
#> [1] 44726
to_aquacrop_day(Sys.Date())
#> [1] 45793
```
