# Round to Nearest Multiple

Round numeric values to the nearest multiple of a specified number.
Useful for standardizing parameter values or creating rounded intervals.

## Usage

``` r
round_to(x, to = 5, threshold = to/2)
```

## Arguments

- x:

  Numeric vector to round

- to:

  Numeric value specifying the multiple to round to. Default = 5

- threshold:

  Numeric value specifying the threshold for rounding up. Values where
  `x %% to >= threshold` will round up, otherwise round down. Default =
  `to/2` (standard rounding)

## Value

Numeric vector of the same length as `x` with values rounded to the
nearest multiple of `to`.

## Details

The function rounds values to the nearest multiple of `to`:

- If the remainder (`x %% to`) is greater than or equal to `threshold`,
  the value rounds up to the next multiple

- Otherwise, it rounds down to the previous multiple

By default, `threshold = to/2` provides standard rounding behavior
(e.g., 47.5 rounds to 50 when `to = 5`).

## See also

Other utility functions:
[`calculate_plant_density()`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md),
[`is_leap_year()`](https://mwaongo.github.io/aquacropr/reference/is_leap_year.md),
[`path_to_file()`](https://mwaongo.github.io/aquacropr/reference/path_to_file.md),
[`salinity_to_ece()`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md),
[`to_aquacrop_day()`](https://mwaongo.github.io/aquacropr/reference/to_aquacrop_day.md)

## Examples

``` r
# Round to nearest 5
round_to(47, to = 5) # Returns 45
#> [1] 45
round_to(48, to = 5) # Returns 50
#> [1] 50
round_to(50, to = 5) # Returns 50
#> [1] 50
round_to(52, to = 5) # Returns 50
#> [1] 50

# Round to nearest 10
round_to(23, to = 10) # Returns 20
#> [1] 20
round_to(27, to = 10) # Returns 30
#> [1] 30
round_to(25, to = 10) # Returns 30 (standard rounding)
#> [1] 30

# Round to nearest integer
round_to(47.8, to = 1) # Returns 48
#> [1] 48
round_to(47.3, to = 1) # Returns 47
#> [1] 47

# Round to nearest 0.5
round_to(3.2, to = 0.5) # Returns 3.0
#> [1] 3
round_to(3.7, to = 0.5) # Returns 3.5
#> [1] 3.5

# Works with vectors
round_to(c(12, 17, 23, 28), to = 5) # Returns c(10, 15, 25, 30)
#> [1] 10 15 25 30

# Custom threshold (round up at 3 instead of 2.5 for to = 5)
round_to(47, to = 5, threshold = 3) # Returns 45
#> [1] 45
round_to(48, to = 5, threshold = 3) # Returns 50
#> [1] 50
```
