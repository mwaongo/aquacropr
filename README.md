
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aquacropr <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

<!-- badges: end -->

R tools and wrappers for running FAO AquaCrop in reproducible workflows.

# Overview

AquaCrop is the FAO reference model for simulating crop yield response
to water stress (Steduto et al., 2009; Raes et al., 2009). Its strong
scientific foundation and relatively low data requirements have made it
a reference tool for research, operational studies, and climate impact
assessments worldwide.

Running AquaCrop at scale, however, can be tedious. The model relies on
a file-based workflow — each simulation requires manually preparing a
set of formatted input files for climate, soil, crop, irrigation, and
management parameters, then invoking the executable, and finally parsing
the output files one by one. This becomes a bottleneck as soon as
simulations span multiple sites, years, or scenarios.

**aquacropr** solves this by providing a lightweight R interface that
automates the entire workflow. It handles input file generation, batch
simulation orchestration, and output parsing — all from R, all
scriptable, and all compatible with standard tidyverse and spatial
workflows. The model engine itself is never touched: aquacropr always
delegates computation to the official FAO AquaCrop binary, ensuring full
scientific validity and consistency with published results.

## What you can do with aquacropr

- **Generate input files programmatically** — convert R data frames
  directly into AquaCrop-compatible climate, soil, crop, irrigation, and
  management files, with no manual editing.
- **Run batch simulations at scale** — automate simulations across
  station networks, climate scenarios, planting date calendars, or
  multi-year periods.
- **Parse and tidy outputs** — read AquaCrop result files into
  structured data frames ready for analysis, visualization, or export.
- **Build reproducible workflows** — script the full chain from raw data
  to final results, enabling version control, traceability, and
  collaboration.

## Who it is for

aquacropr is aimed at agronomists, climate scientists, hydrologists, and
data analysts working on crop-water-climate interactions — whether for
regional yield assessments, climate change impact studies, irrigation
optimization, operational crop monitoring, or research and capacity
building.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("mwaongo/aquacropr")
```

## Getting started

The first step is to set up a dedicated project directory.
`init_aquacrop()` creates the standard AquaCrop folder structure and
downloads the official FAO binary appropriate for your operating system.

``` r
library(aquacropr)

init_aquacrop(path = "my-project", version = "7.2")
```

This will open by default a new clean RStudio project. Consult
`readme.txt` to understand the directory structure.

The workflow below walks through a complete single-station simulation:
**climate → crop → soil → management → irrigation → onset calendar →
project file → run**. Every step produces one or more AquaCrop-formatted
files directly from R objects, with no manual editing.

### Climate

aquacropr ships with a built-in `weather` dataset containing daily
observations for a single station. Inspect it first, then a single call
to `write_climate()` generates all four climate files required by
AquaCrop.

``` r
head(weather)
#>   year month day rain  et0 tmin tmax
#> 1 1976     1   1  0.0 3.21  9.2 24.1
#> 2 1976     1   2  0.0 3.45 10.1 25.3
#> 3 1976     1   3  2.4 2.87  8.7 21.6

write_climate(
  data      = weather,
  site_name = "wakanda"
)
# → CLIMATE/wakanda.PLU  (rainfall)
# → CLIMATE/wakanda.Tnx  (min/max temperature)
# → CLIMATE/wakanda.ETo  (reference evapotranspiration)
# → CLIMATE/wakanda.CLI  (master file linking the three above)
```

If you need finer control over file content or naming, individual
writers (`write_plu()`, `write_tnx()`, `write_eto()`, `write_cli()`) are
also available.

### Crop

`write_cro()` generates the crop parameter file. Default parameters are
provided for all crops supported by AquaCrop. Individual parameters can
be overridden via `params = list(...)`, where each element is named by
its AquaCrop variable identifier.

``` r
write_cro(
  crop_name = "maize",
  params = list(
    var_35 = 1.10,  # higher crop coefficient
    var_50 = 0.85,  # maximum canopy cover
    var_64 = 35     # reference harvest index
  )
)
# → CROP/maize.CRO
```

### Soil

`write_sol()` generates the soil profile and initial water content from
layer-wise descriptors. Pedotransfer functions (Saxton & Rawls, 2006)
are applied automatically to derive hydraulic properties from texture
when measured values are unavailable. Salinity class and curve number
can be specified per layer.

``` r
write_sol(
  site_name     = "wakanda",
  cn            = c(65, 70),
  rew           = c(7, 5),
  texture       = c("sandy loam", "clay loam"),
  thickness     = c(0.4, 0.6),
  initial_water = "WP",
  salinity      = "very slight"
)
# → SOIL/wakanda.SOL
# → SOIL/wakanda.SW0
```

### Management

`write_man()` generates the field management file. Soil fertility level,
mulch cover, and evaporation reduction from mulch are controlled via
`params`.

``` r
write_man(
  path      = "MANAGEMENT/",
  site_name = "wakanda",
  params = list(
    var_05 = 35,  # 35% fertility level
    var_03 = 30,  # 30% mulch cover
    var_04 = 50   # 50% evaporation reduction from mulch
  )
)
# → MANAGEMENT/wakanda.MAN
```

### Irrigation

aquacropr supports all AquaCrop irrigation modes. For automatic
irrigation (mode 2), `create_irr_schedule()` first builds the schedule
data frame — defining trigger thresholds and application depths for
successive crop periods — which is then passed to `write_irr()`.

``` r
schedule_raw <- create_irr_schedule(
  from_day        = c(1, 30, 60),
  time_crit       = c(50, 40, 30),   # % RAW depletion threshold per period
  depth_crit      = c(0, 0, 5),      # back to FC (+ 5 mm on last period)
  ecw             = c(0.5, 0.6, 0.7),
  time_crit_code  = 3,               # criterion: % RAW
  depth_crit_code = 1                # application: back to field capacity
)

write_irr(
  path        = "MANAGEMENT/",
  site_name   = "wakanda",
  method      = 5,
  wet_surface = 30,
  mode        = 2,
  irr_data    = schedule_raw
)
# → MANAGEMENT/wakanda.IRR
```

### Onset calendar

When planting dates depend on seasonal rainfall onset rather than a
fixed calendar date — the standard situation in rainfed systems —
`write_cal()` defines the onset criterion. `find_onset()` is then called
internally by `write_prm()` to derive planting dates for each year in
the climate record.

``` r
write_cal(
  site_name     = "wakanda",
  onset         = "rainfall",
  window_start  = 121,   # search starts at DOY 121 (May 1)
  window_length = 92,    # 92-day search window
  criterion     = 4,     # cumulative rainfall criterion
  preset_value  = 50,    # 50 mm threshold
  occurrences   = 1
)
# → CAL/wakanda.CAL
```

### Project file

`write_prm()` assembles all inputs into a single `.PRM` file. When
`calendar_path` is provided, `find_onset()` is called internally to
derive planting dates from the climate series and the `.CAL` criterion —
no `planting_schedule` argument is needed. Fixed planting dates can
alternatively be supplied directly as a data frame.

``` r
write_prm(
  site_name       = "wakanda",
  crop_name       = "maize",
  irrigation_path = "MANAGEMENT",
  calendar_path   = "CAL/"
  # or, for fixed dates:
  # planting_schedule = data.frame(year = 1976:2005, planting_doy = 121)
)
# → LIST/wakanda.PRM
```

### Running the simulation

``` r
run_aquacrop()
```

## Reading input files

All climate and calendar input files can be read back into tidy data
frames for inspection or further processing.

``` r
read_climate("CLIMATE/wakanda.CLI")  # master file + data
read_plu("CLIMATE/wakanda.PLU")
read_tnx("CLIMATE/wakanda.Tnx")
read_eto("CLIMATE/wakanda.ETo")      # alias: read_et0()
read_cal("CAL/wakanda.CAL")          # onset parameters
```

## Key functions

| File type        | Writer                  | Batch writer        |
|------------------|-------------------------|---------------------|
| Climate (all)    | `write_climate()`       | —                   |
| Rainfall         | `write_plu()`           | —                   |
| Temperature      | `write_tnx()`           | —                   |
| Reference ET     | `write_eto()`           | —                   |
| Crop             | `write_cro()`           | —                   |
| Soil profile     | `write_sol()`           | `write_sol_batch()` |
| Management       | `write_man()`           | `write_man_batch()` |
| Irrigation       | `write_irr()`           | `write_irr_batch()` |
| Irr. schedule    | `create_irr_schedule()` | —                   |
| Calendar (onset) | `write_cal()`           | `write_cal_batch()` |
| Project file     | `write_prm()`           | `write_prm_batch()` |
