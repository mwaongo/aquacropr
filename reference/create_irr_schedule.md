# Create Irrigation Schedule Rules Data

Helper function to create properly formatted irrigation schedule rules
for use with
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md)
in mode 2 (generation of an irrigation schedule).

## Usage

``` r
create_irr_schedule(
  from_day,
  time_crit,
  depth_crit,
  ecw,
  time_crit_code,
  depth_crit_code,
  crop_length = NULL
)
```

## Arguments

- from_day:

  Integer vector. Day after sowing/planting from which each rule is
  valid. First value must be 1.

- time_crit:

  Numeric vector. Time criterion value. Meaning depends on
  `time_crit_code`:

  - Code 1: Fixed interval in days between irrigations

  - Code 2: Allowable depletion in mm before irrigation

  - Code 3: Allowable depletion as % of RAW before irrigation

- depth_crit:

  Numeric vector. Depth criterion value. Meaning depends on
  `depth_crit_code`:

  - Code 1: Extra water (mm) on top of amount to reach FC (can be 0,
    positive, or negative)

  - Code 2: Fixed net irrigation application depth in mm

- ecw:

  Numeric vector. Electrical conductivity of irrigation water in dS/m

- time_crit_code:

  Integer. Time criterion type (applies to all rules):

  - 1 = Fixed interval (days)

  - 2 = Allowable depletion (mm water)

  - 3 = Allowable depletion (% of RAW)

- depth_crit_code:

  Integer. Depth criterion type (applies to all rules):

  - 1 = Back to Field Capacity (with optional adjustment)

  - 2 = Fixed net application depth

- crop_length:

  Integer. Optional. Length of crop cycle in days for validation. If
  provided, warns about schedule rules starting beyond crop harvest.

## Value

A tibble with columns `from_day`, `time_crit`, `depth_crit`, `ecw` and
attributes `time_crit_code` and `depth_crit_code`, ready for use with
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md)

## Details

Rules remain valid from their `from_day` until the next rule's
`from_day` or until the end of the cropping period.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fixed 7-day interval irrigation
schedule_fixed <- create_irr_schedule(
  from_day = c(1, 30, 90),
  time_crit = c(999, 7, 999),      # Irrigate every 7 days from day 30-89
  depth_crit = c(0, 40, 0),        # 40mm each time
  ecw = c(0.5, 0.5, 0.5),
  time_crit_code = 1,              # Fixed interval
  depth_crit_code = 2,             # Fixed depth
  crop_length = 120                # Optional validation
)

# RAW-based irrigation (back to FC)
schedule_raw <- create_irr_schedule(
  from_day = c(1, 40),
  time_crit = c(50, 40),           # Trigger at 50% RAW, then 40% RAW
  depth_crit = c(0, 5),            # Back to FC, then FC + 5mm
  ecw = c(0.6, 0.8),
  time_crit_code = 3,              # % RAW threshold
  depth_crit_code = 1              # Back to FC
)

# Depletion in mm criterion
schedule_mm <- create_irr_schedule(
  from_day = 1,
  time_crit = 30,                  # Irrigate when 30mm depleted
  depth_crit = -5,                 # Return to FC - 5mm (slight deficit)
  ecw = 0.5,
  time_crit_code = 2,              # Depletion in mm
  depth_crit_code = 1              # Back to FC (with adjustment)
)
} # }
```
