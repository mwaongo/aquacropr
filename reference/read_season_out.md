# Read an AquaCrop Season Output File

Parses an AquaCrop `PRMSeason.OUT` file into one or two tidy data
frames. Seasonal totals (rows starting with `Tot`) are always returned.
Intermediate rows (`10Day` or `Month`) are optionally returned as a
second data frame.

## Usage

``` r
read_season_out(file, intermediate = FALSE)
```

## Arguments

- file:

  Character. Path to the `PRMSeason.OUT` file.

- intermediate:

  Logical. If `TRUE`, returns a named list with two elements: `season`
  (seasonal totals) and `intermediate` (decadal or monthly rows). If
  `FALSE` (default), returns only the seasonal totals data frame.

## Value

When `intermediate = FALSE`: a `data.frame` of seasonal totals. When
`intermediate = TRUE`: a named list with:

- season:

  data.frame of seasonal totals (`Tot` rows).

- intermediate:

  data.frame of decadal or monthly rows (`10Day` or `Month` rows), or
  `NULL` if none found.

## Details

Column names are derived from the header line. The `RunNr` column is
parsed as integer. The `prm_file` column contains the station/PRM file
name from the last field of each data row. Columns `E/Ex`, `Tr/Trx`,
`Y(dry)`, `Y(fresh)` and `Brelative` are renamed to valid R names:
`E_Ex`, `Tr_Trx`, `Y_dry`, `Y_fresh`, `Brelative`.

## See also

Other AquaCrop readers:
[`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Seasonal totals only
season <- read_season_out("LIST/C1PRMSeason.OUT")

# With daily or decadal or monthly intermediates
out <- read_season_out("LIST/C1PRMSeason.OUT", intermediate = TRUE)
out$season
out$intermediate
} # }
```
