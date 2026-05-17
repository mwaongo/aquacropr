# Read an AquaCrop day.out file

Auto-detects columns and multi-run structure. Handles duplicated column
names (suffixed `_1`, `_2`, ...) and space-separated numeric suffixes
(e.g. `"WC 2"` becomes `WC2`).

## Usage

``` r
read_day_out(file, na = .DAY_NA_VALS, verbose = FALSE)
```

## Arguments

- file:

  Path to the `day.out` file.

- na:

  Numeric sentinel values coerced to `NA`. Default:
  `c(-9, -9.9, -99, -99.9, -999, -999.9)`.

- verbose:

  If `TRUE`, prints column count and duplicate names.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with a
trailing `prm_file` column (base name of the `.PRM` file inferred from
`file`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Single-run file
df <- read_day_out("path/to/day.out")
df

# Multi-run file with diagnostic info
df <- read_day_out("path/to/day.out", verbose = TRUE)

# Filter one year after reading
library(dplyr)
df |> filter(Year == 1976)
} # }
```
