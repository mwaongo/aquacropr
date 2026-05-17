# Read an AquaCrop PLU File

Reads and parses an AquaCrop PLU (rainfall) file into a tidy data frame.

## Usage

``` r
read_plu(file)
```

## Arguments

- file:

  Character. Path to the AquaCrop PLU file.

## Value

A tibble with columns:

- station: Character, station name

- year: Integer, year

- month: Integer, month (1-12)

- day: Integer, day of month

- rain: Numeric, rainfall value (mm). -9 values are converted to NA.

## See also

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rainfall <- read_plu("data/station1.plu")
head(rainfall)
} # }
```
