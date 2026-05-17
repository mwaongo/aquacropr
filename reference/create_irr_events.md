# Create Irrigation Events Data

Helper function to create properly formatted irrigation events data for
use with
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md)
in mode 1 (specification of irrigation events).

## Usage

``` r
create_irr_events(day, depth, ecw, crop_length = NULL)
```

## Arguments

- day:

  Integer vector. Day after sowing/planting for each irrigation event
  (1-366)

- depth:

  Numeric vector. Net irrigation application depth in mm for each event.
  This is the net amount only - do not include conveyance losses.

- ecw:

  Numeric vector. Electrical conductivity of irrigation water in dS/m
  for each event

- crop_length:

  Integer. Optional. Length of crop cycle in days for validation. If
  provided, warns about irrigation events beyond crop harvest.

## Value

A tibble with columns `day`, `depth`, `ecw`, ready for use with
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed irrigation schedule for dry season tomato
events <- create_irr_events(
  day = c(10, 20, 30, 40, 50, 60, 70, 80),
  depth = c(40, 40, 50, 50, 50, 40, 40, 30),
  ecw = rep(0.5, 8),
  crop_length = 120  # Optional: validate against 120-day cycle
)

# Variable water quality scenario
events_variable <- create_irr_events(
  day = c(15, 30, 45, 60),
  depth = c(50, 50, 50, 50),
  ecw = c(0.5, 0.8, 1.2, 1.5)  # Increasing salinity
)
} # }
```
