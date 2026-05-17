# Write AquaCrop Climate File

Write an AquaCrop v7.0 (August 2022) climate (.CLI) file that references
the temperature, ETo, and rainfall files, along with the appropriate CO2
concentration file for the simulation scenario.

## Usage

``` r
write_cli(
  path = "weather/",
  site_name = "station",
  eol = "windows",
  scenario = "hist",
  check_files = TRUE
)
```

## Arguments

- path:

  Directory path where climate files are located and where .CLI will be
  written. Default = "weather/"

- site_name:

  Station name or identifier. The .Tnx, .ETo, and .PLU files with this
  station name must exist in the path. Default = "station"

- eol:

  End-of-line character style for the output file. Options: "windows",
  "unix", "linux", or "macOS". Default = "windows"

- scenario:

  CO2 scenario to use. Options: "hist" (historical/Mauna Loa), "rcp26",
  "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245", "ssp370",
  "ssp585". Default = "hist"

- check_files:

  Logical. If TRUE, checks that all three required climate files (.Tnx,
  .ETo, .PLU) exist before writing .CLI file. Default = TRUE

## Value

Invisibly returns the full path to the created .CLI file. The function
also copies the appropriate CO2 file to the specified path if it doesn't
already exist there.

## Details

The .CLI file is the main climate file that references:

- Temperature file (.Tnx)

- Reference evapotranspiration file (.ETo)

- Rainfall file (.PLU)

- CO2 concentration file (.CO2)

**Important**: By default, the function validates that all three climate
files (.Tnx, .ETo, .PLU) exist before creating the .CLI file. Set
`check_files = FALSE` to skip this validation (not recommended unless
you're sure the files will be created later).

### CO2 Scenarios:

- **hist**: Historical data from Mauna Loa Observatory (MaunaLoa.CO2)

- **RCP scenarios**: Representative Concentration Pathways

  - rcp26: RCP 2.6 (low emissions)

  - rcp45: RCP 4.5 (intermediate emissions)

  - rcp60: RCP 6.0 (intermediate-high emissions)

  - rcp85: RCP 8.5 (high emissions)

- **SSP scenarios**: Shared Socioeconomic Pathways

  - ssp119: SSP1-1.9 (very low emissions)

  - ssp126: SSP1-2.6 (low emissions)

  - ssp245: SSP2-4.5 (intermediate emissions)

  - ssp370: SSP3-7.0 (high emissions)

  - ssp585: SSP5-8.5 (very high emissions)

## See also

[`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md)
for writing rainfall files,
[`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md)
for writing ETo files,
[`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
for writing temperature files

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
[`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md),
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md),
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
# First, create the required climate files
data("weather")
write_plu(data = weather, site_name = "Wakanda", path = "weather/")
write_eto(data = weather, site_name = "Wakanda", path = "weather/")
write_tnx(data = weather, site_name = "Wakanda", path = "weather/")

# Then write CLI file with historical CO2
write_cli(
  path = "weather/",
  site_name = "Wakanda",
  scenario = "hist"
)

# Write CLI file with RCP 4.5 scenario
write_cli(
  path = "weather/",
  site_name = "Wakanda",
  scenario = "rcp45"
)

# Skip validation (not recommended)
write_cli(
  path = "weather/",
  site_name = "Wakanda",
  scenario = "hist",
  check_files = FALSE
)
} # }
```
