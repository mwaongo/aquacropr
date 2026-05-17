# Write AquaCrop Reference Evapotranspiration File

Write an AquaCrop v7.0 (August 2022) reference evapotranspiration (.ETo)
file containing daily ETo data for use in crop simulations.

## Usage

``` r
write_eto(
  path = "CLIMATE/",
  site_name = NULL,
  data = NULL,
  var_name = "et0",
  syear = NULL,
  eyear = NULL,
  eol = NULL,
  record_type = 1,
  first_day = 1,
  first_month = 1
)

write_et0(
  path = "CLIMATE/",
  site_name = NULL,
  data = NULL,
  var_name = "et0",
  syear = NULL,
  eyear = NULL,
  eol = NULL,
  record_type = 1,
  first_day = 1,
  first_month = 1
)
```

## Arguments

- path:

  Directory path where the output .ETo file will be written. Default =
  "CLIMATE/"

- site_name:

  Station name or identifier for the weather station. Default = NULL
  (extracted from data if available)

- data:

  Data frame containing daily ETo data. Can have either:

  - Columns: station, year, month, day, et0 (legacy format), or

  - Columns: year, month, day, et0 (new format)

- var_name:

  Name of the ETo column in the data frame. Default = "et0"

- syear:

  Start year of the data period (extracted from data if not provided).
  Default = NULL

- eyear:

  End year of the data period (extracted from data if not provided).
  Default = NULL

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected. Options: "windows", "unix",
  "linux", or "macOS". Default = "windows"

- record_type:

  Type of temporal aggregation. Options: 1 = daily, 2 = 10-daily, 3 =
  monthly. Default = 1

- first_day:

  First day of record. For 10-daily: 1, 11, or 21; For monthly: 1.
  Default = 1

- first_month:

  First month of record (1-12). Default = 1

## Value

Invisibly returns the full path to the created .ETo file. Creates a
formatted .ETo file in the specified directory with the format:
\<\<site_name\>\>.ETo containing daily reference evapotranspiration data
as required by AquaCrop v7.0.

## See also

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
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
# Load example weather data
data("weather")

# Write ETo file with default column name
write_eto(
  path = "weather/",
  site_name = "Wakanda_Station",
  data = weather
)

# Write ETo file with custom column name
write_eto(
  path = "weather/",
  site_name = "Wakanda_Station",
  data = weather,
  var_name = "eto_calculated"
)

# Legacy format (backward compatibility)
write_eto(data = weather_with_station, path = "climate/")
} # }
```
