# Read an AquaCrop ETo File

Reads and parses an AquaCrop ETo (reference evapotranspiration) file
into a tidy data frame.

## Usage

``` r
read_eto(file)

read_et0(file)
```

## Arguments

- file:

  Character. Path to the AquaCrop ETo file.

## Value

A tibble with columns:

- station: Character, station/grid name

- year: Integer, year

- month: Integer, month (1-12)

- day: Integer, day of month

- eto: Numeric, reference evapotranspiration (mm/day). -9 values are
  converted to NA.

## See also

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
eto_data <- read_eto("data/grid_003.ETo")
head(eto_data)
} # }
```
