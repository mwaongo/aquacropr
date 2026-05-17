# Write AquaCrop Irrigation File

Creates an AquaCrop v7.0+ irrigation (.IRR) file that defines irrigation
management for simulation. The irrigation file specifies the irrigation
method, percentage of soil surface wetted, and irrigation scheduling
approach.

## Usage

``` r
write_irr(
  path = "MANAGEMENT/",
  site_name,
  method,
  wet_surface,
  mode,
  irr_data,
  version = 7.1,
  eol = NULL,
  crop_length = NULL
)
```

## Arguments

- path:

  Character. Directory path where the .IRR file will be created. The
  directory is created automatically if it doesn't exist. Default:
  `"MANAGEMENT/"`.

- site_name:

  Character. Name identifier for the irrigation scenario. This name is
  used as the filename (e.g., `"drip-irrigation"` creates
  `"drip-irrigation.IRR"`).

- method:

  Integer. Irrigation method:

  - 1 = Sprinkler irrigation

  - 2 = Surface irrigation: Basin

  - 3 = Surface irrigation: Border

  - 4 = Surface irrigation: Furrow

  - 5 = Drip irrigation

- wet_surface:

  Integer. Percentage of soil surface wetted by irrigation (0-100). See
  Details for typical values by irrigation method.

- mode:

  Integer. Irrigation mode:

  - 1 = Specification of irrigation events

  - 2 = Generation of an irrigation schedule

  - 3 = Determination of net irrigation water requirement

- irr_data:

  Data.frame or numeric. Irrigation data. Structure depends on `mode`:

  - Mode 1: data.frame with columns `day`, `depth`, `ecw`. See
    [`create_irr_events()`](https://mwaongo.github.io/aquacropr/reference/create_irr_events.md)

  - Mode 2: data.frame with columns `from_day`, `time_crit`,
    `depth_crit`, `ecw` and attributes `time_crit_code`,
    `depth_crit_code`. See
    [`create_irr_schedule()`](https://mwaongo.github.io/aquacropr/reference/create_irr_schedule.md)

  - Mode 3: Single numeric value representing allowable depletion of RAW
    (%, 0-100)

- version:

  Numeric. AquaCrop version. Default: 7.1

- eol:

  Character. End-of-line character style. Options: "windows", "linux",
  or "macos". If `NULL` (default), eol is auto-detected.

- crop_length:

  Integer. Optional. Length of crop cycle in days for validation. If
  provided, warns about irrigation events beyond crop harvest.

## Value

Invisibly returns the full path to the created .IRR file as a character
string. The main effect is writing the file to disk.

## Details

### Typical Wetted Surface by Irrigation Method

The `wet_surface` parameter represents the percentage of soil surface
wetted by irrigation. Typical values by irrigation method:

|                                   |                         |
|-----------------------------------|-------------------------|
| Irrigation Method                 | Typical wet_surface (%) |
| Sprinkler                         | 100                     |
| Basin                             | 100                     |
| Border                            | 100                     |
| Furrow (every furrow, narrow bed) | 60-100                  |
| Furrow (every furrow, wide bed)   | 40-60                   |
| Furrow (alternated furrows)       | 30-50                   |
| Drip/Micro irrigation             | 15-40                   |
| Subsurface drip                   | 0                       |

### Irrigation Modes

**Mode 1 - Specified Events**: Define exact irrigation dates, depths,
and water quality. Use
[`create_irr_events()`](https://mwaongo.github.io/aquacropr/reference/create_irr_events.md)
to create properly formatted data.

**Mode 2 - Generated Schedule**: Define rules that generate irrigation
events automatically based on time or depletion criteria. Use
[`create_irr_schedule()`](https://mwaongo.github.io/aquacropr/reference/create_irr_schedule.md)
to create properly formatted data with required attributes.

**Mode 3 - Net Requirement**: Calculate total irrigation water needed to
maintain soil water above a specified threshold (% RAW). Provide a
single numeric value.

## See also

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
[`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md),
[`write_man()`](https://mwaongo.github.io/aquacropr/reference/write_man.md),
[`write_obs()`](https://mwaongo.github.io/aquacropr/reference/write_obs.md),
[`write_off()`](https://mwaongo.github.io/aquacropr/reference/write_off.md),
[`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_ppn()`](https://mwaongo.github.io/aquacropr/reference/write_ppn.md),
[`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Fixed irrigation schedule (Mode 1)
events <- create_irr_events(
  day = c(10, 20, 30, 40, 50),
  depth = c(50, 50, 50, 50, 50),
  ecw = c(1.0, 1.0, 1.2, 1.4, 1.6)
)

write_irr(
  path = "MANAGEMENT/",
  site_name = "ouaga-fixed",
  method = 5,        # Drip irrigation
  wet_surface = 30,  # 30% surface wetted
  mode = 1,          # Specified events
  irr_data = events,
  crop_length = 120  # Optional: validate against 120-day cycle
)

# Example 2: Generated schedule with fixed interval (Mode 2)
schedule <- create_irr_schedule(
  from_day = c(1, 41, 116),
  time_crit = c(999, 7, 999),      # 7-day interval from day 41-115
  depth_crit = c(0, 40, 0),        # 40mm application depth
  ecw = c(0.4, 0.6, 0.8),
  time_crit_code = 1,              # Fixed interval
  depth_crit_code = 2              # Fixed depth
)

write_irr(
  path = "MANAGEMENT/",
  site_name = "bobo-schedule",
  method = 1,         # Sprinkler
  wet_surface = 100,  # 100% surface wetted
  mode = 2,           # Generated schedule
  irr_data = schedule
)

# Example 3: RAW-based automatic irrigation (Mode 2)
schedule_raw <- create_irr_schedule(
  from_day = c(1, 30, 60),
  time_crit = c(50, 40, 30),       # % RAW depletion threshold
  depth_crit = c(0, 0, 5),         # Back to FC (+ 5mm on last period)
  ecw = c(0.5, 0.6, 0.7),
  time_crit_code = 3,              # % RAW criterion
  depth_crit_code = 1              # Back to field capacity
)

write_irr(
  path = "MANAGEMENT/",
  site_name = "auto-raw",
  method = 5,
  wet_surface = 30,
  mode = 2,
  irr_data = schedule_raw
)

# Example 4: Net irrigation requirement (Mode 3)
write_irr(
  path = "MANAGEMENT/",
  site_name = "koudougou-netreq",
  method = 1,         # Sprinkler
  wet_surface = 100,
  mode = 3,           # Net requirement
  irr_data = 50       # Do not deplete below 50% RAW
)
} # }
```
