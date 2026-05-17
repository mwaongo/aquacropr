# Write an AquaCrop Simulation Settings File

Writes either `AggregationResults.SIM` or `DailyResults.SIM` to the
`SIMUL/` subdirectory.

## Usage

``` r
write_sim(
  code,
  what = c("AggregationResults", "DailyResults"),
  path = "SIMUL/",
  eol = NULL
)
```

## Arguments

- code:

  Integer or integer vector.

  - For `"AggregationResults"`: single value, 0-3. 0 = none, 1 = daily,
    2 = 10-daily, 3 = monthly.

  - For `"DailyResults"`: one or more values from 1-7. 1 = soil water
    balance, 2 = crop development, 3 = soil water content profile, 4 =
    soil salinity profile, 5 = soil water content at depths, 6 = soil
    salinity at depths, 7 = climate inputs.

- what:

  Character. One of `"AggregationResults"` or `"DailyResults"`.

- path:

  Character. Output directory. Default: `"SIMUL/"`.

- eol:

  Character. End-of-line style: `"windows"`, `"linux"`, or `"macos"`. If
  `NULL` (default), auto-detected.

## Value

Invisibly returns the output file path.
