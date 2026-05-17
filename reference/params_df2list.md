# Convert data.frame to parameter list for batch functions

Converts a data.frame where each row represents one site/station into a
list of parameter lists suitable for batch writing functions like
[`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md)
or
[`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md).

## Usage

``` r
params_df2list(
  df,
  target = c("write_sol_batch", "write_man_batch"),
  id_col = NULL,
  validate = TRUE
)
```

## Arguments

- df:

  Data.frame where each row represents one site/station

- target:

  Character string specifying the target function. One of:

  - `"write_sol_batch"`: For soil parameters (texture, thickness, cn,
    rew)

  - `"write_man_batch"`: For management parameters (var_02 to var_21)

- id_col:

  Name of the column containing site/station identifiers. Default:
  `"site"` for soil, `"station"` for management. Set to `NULL` to
  exclude from parameters.

- validate:

  Logical. If `TRUE` (default), validates parameter values.

## Value

List of parameter lists suitable for the target batch function.
