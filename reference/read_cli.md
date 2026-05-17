# Read an AquaCrop CLI File

Reads and parses an AquaCrop CLI (climate) file, locates the referenced
weather files (.Tnx, .ETo, .PLU), reads them, and joins them into a
single tidy data frame.

## Usage

``` r
read_cli(file)

read_climate(file)
```

## Arguments

- file:

  Character. Path to the AquaCrop CLI file.

## Value

A tibble with columns:

- station: Character, station/grid name (from temperature file)

- year: Integer, year

- month: Integer, month (1-12)

- day: Integer, day of month

- tmin: Numeric, minimum temperature (°C)

- tmax: Numeric, maximum temperature (°C)

- eto: Numeric, reference evapotranspiration (mm/day)

- rain: Numeric, rainfall (mm)

## Details

The CLI file structure:

- Line 1: Description/name

- Line 2: AquaCrop version

- Line 3: Temperature file (.Tnx)

- Line 4: ETo file (.ETo)

- Line 5: Rainfall file (.PLU)

- Line 6: CO2 file (.CO2)

## See also

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read CLI file and get complete weather data
climate <- read_cli("data/default.CLI")
head(climate)
} # }
```
