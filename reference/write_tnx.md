# Write AquaCrop Temperature File

Write an AquaCrop v7.0 (August 2022) temperature (.Tnx) file containing
daily minimum and maximum temperature data for use in crop simulations.

## Usage

``` r
write_tnx(
  path = "CLIMATE/",
  site_name = NULL,
  data = NULL,
  var_name_min = "tmin",
  var_name_max = "tmax",
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

  Directory path where the output .Tnx file will be written. Default =
  "CLIMATE/"

- site_name:

  Station name or identifier for the weather station. Default = NULL
  (extracted from data if available)

- data:

  Data frame containing daily temperature data. Can have either:

  - Columns: station, year, month, day, tmin, tmax (legacy format), or

  - Columns: year, month, day, tmin, tmax (new format)

- var_name_min:

  Name of the minimum temperature column in the data frame. Default =
  "tmin"

- var_name_max:

  Name of the maximum temperature column in the data frame. Default =
  "tmax"

- syear:

  Start year of the data period (extracted from data if not provided).
  Default = NULL

- eyear:

  End year of the data period (extracted from data if not provided).
  Default = NULL

- eol:

  End-of-line character style for the output file. Options: "windows",
  "unix", "linux", or "macOS". Default = "windows"

- record_type:

  Type of temporal aggregation. Options: 1 = daily, 2 = 10-daily, 3 =
  monthly. Default = 1

- first_day:

  First day of record. For 10-daily: 1, 11, or 21; For monthly: 1.
  Default = 1

- first_month:

  First month of record (1-12). Default = 1

## Value

Invisibly returns the full path to the created .Tnx file. Creates a
formatted .Tnx file in the specified directory with the format:
\<\<site_name\>\>.Tnx containing daily minimum and maximum temperature
data as required by AquaCrop v7.0.

## See also

[`weather`](https://mwaongo.github.io/aquacropr/reference/weather.md)
for example weather data,
[`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md)
for writing rainfall files,
[`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md)
for writing ETo files

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
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
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example weather data
data("weather")

# Write temperature file with default column names
write_tnx(
  path = "weather/",
  site_name = "Wakanda_Station",
  data = weather
)

# Legacy format (backward compatibility)
write_tnx(data = weather_with_station, path = "climate/")
} # }
```
