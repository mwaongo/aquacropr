# Read an AquaCrop Tnx File

Reads and parses an AquaCrop Tnx (temperature) file into a tidy data
frame.

## Usage

``` r
read_tnx(file)
```

## Arguments

- file:

  Character. Path to the AquaCrop Tnx file.

## Value

A tibble with columns:

- station: Character, station/grid name

- year: Integer, year

- month: Integer, month (1-12)

- day: Integer, day of month

- tmin: Numeric, minimum temperature (°C). -9 values are converted to
  NA.

- tmax: Numeric, maximum temperature (°C). -9 values are converted to
  NA.

## See also

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md)

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md)

## Examples

``` r
if (FALSE) { # \dontrun{
temp_data <- read_tnx("data/grid_005.Tnx")
head(temp_data)
} # }
```
