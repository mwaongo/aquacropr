# Compute AquaCrop day-number from a given date

Calculate AquaCrop day-number from various date formats. The day number
represents days elapsed since January 1, 1901.

## Usage

``` r
day_number(year, month = NULL, day = NULL, doy = NULL)

DayNumber(year, doy)
```

## Arguments

- year:

  Year (numeric 1901+), date string (yyyy-mm-dd), or Date object

- month:

  Month number (1-12). Optional if using doy or date string

- day:

  Day number (1-31). Optional if using doy or date string

- doy:

  Day of year (1-366). Optional, alternative to month/day

## Value

A numeric representing AquaCrop day number

## Details

Flexible input handling:

- `day_number(2023, 4, 30)` — year, month, day

- `day_number(2023, doy = 120)` — year and day-of-year

- `day_number("2023-04-30")` — date string

- `day_number(as.Date("2023-04-30"))` — Date object

## Functions

- `DayNumber()`: Alias for backward compatibility

## Examples

``` r
day_number(2023, 4, 30)
#> [1] 44680
day_number(2023, doy = 120)
#> [1] 44680
day_number("2023-04-30")
#> [1] 44680
```
