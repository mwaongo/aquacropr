# Write Complete AquaCrop Climate Dataset

Convenience wrapper function that writes all required AquaCrop climate
files (.PLU, .ETo, .Tnx, and .CLI) in a single call. This ensures all
files are created consistently with the same station name and settings.

## Usage

``` r
write_climate(
  data = NULL,
  path = "CLIMATE/",
  site_name = NULL,
  var_name_rain = "rain",
  var_name_et0 = "et0",
  var_name_tmin = "tmin",
  var_name_tmax = "tmax",
  scenario = "hist",
  eol = NULL,
  syear = NULL,
  eyear = NULL
)
```

## Arguments

- data:

  Data frame containing complete weather data with columns:

  - year, month, day (date information)

  - rain or var_name_rain (rainfall, mm)

  - et0 or var_name_et0 (reference evapotranspiration, mm/day)

  - tmin or var_name_tmin (minimum temperature, deg C)

  - tmax or var_name_tmax (maximum temperature, deg C)

  - station (optional, station name)

- path:

  Directory path where all climate files will be written. Default =
  "weather/" (relative to current working directory). See
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md),
  and
  [`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
  for details.

- site_name:

  Station name or identifier. Default = NULL (extracted from data if
  available, otherwise uses "station"). See
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md),
  and
  [`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
  for details.

- var_name_rain:

  Name of rainfall column in `data`. Default = "rain". Passed to
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md)
  as `var_name`.

- var_name_et0:

  Name of ETo column in `data`. Default = "et0". Passed to
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md)
  as `var_name`.

- var_name_tmin:

  Name of minimum temperature column in `data`. Default = "tmin". Passed
  to
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  as `var_name_min`.

- var_name_tmax:

  Name of maximum temperature column in `data`. Default = "tmax". Passed
  to
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  as `var_name_max`.

- scenario:

  CO2 scenario for .CLI file. Options: "hist" (historical/Mauna Loa),
  "rcp26", "rcp45", "rcp60", "rcp85", "ssp119", "ssp126", "ssp245",
  "ssp370", "ssp585". Default = "hist". See
  [`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
  for details on scenarios.

- eol:

  End-of-line character style for the output file. Options: "windows",
  "linux", "macos". If `NULL` (default), eol is auto-detected.

- syear:

  Start year of the data period (extracted from `data` if not provided).
  Default = NULL. See
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  and
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  for details.

- eyear:

  End year of the data period (extracted from `data` if not provided).
  Default = NULL. See
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  and
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  for details.

## Value

Invisibly returns a named list with paths to all created files:

- cli:

  Path to .CLI file (from
  [`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md))

- plu:

  Path to .PLU file (from
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md))

- eto:

  Path to .ETo file (from
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md))

- tnx:

  Path to .Tnx file (from
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md))

## Details

This function creates a complete set of AquaCrop climate files in the
correct order:

1.  Creates output directory if it doesn't exist

2.  Writes .PLU file (rainfall data) using
    [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md)

3.  Writes .ETo file (reference evapotranspiration data) using
    [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md)

4.  Writes .Tnx file (temperature data) using
    [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

5.  Writes .CLI file (climate file that references the above files)
    using
    [`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)

6.  Copies appropriate CO2 file based on `scenario`

All files will use the same station name, ensuring consistency. The
function passes appropriate parameters to each underlying write
function.

### Parameter Mapping:

Arguments are passed to the underlying functions as follows:

- `data` passed to
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

- `path` passed to all write functions

- `site_name` passed to all write functions

- `var_name_rain` passed to
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md)
  as `var_name`

- `var_name_et0` passed to
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md)
  as `var_name`

- `var_name_tmin` passed to
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  as `var_name_min`

- `var_name_tmax` passed to
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  as `var_name_max`

- `scenario` passed to
  [`write_cli`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)

- `eol` passed to all write functions

- `syear`, `eyear` passed to
  [`write_plu`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  [`write_tnx`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example weather data
data("weather")

# Write all climate files with defaults
files <- write_climate(
  data = weather,
  path = "weather/",
  site_name = "Wakanda"
)

# Access individual file paths
files$cli
files$plu

# Write climate files with RCP 4.5 scenario
write_climate(
  data = weather,
  path = "weather/",
  site_name = "Wakanda_Station",
  scenario = "rcp45"
)

# Write climate files with custom column names
write_climate(
  data = my_weather_data,
  path = "climate/",
  site_name = "MyStation",
  var_name_rain = "precipitation",
  var_name_et0 = "pet",
  var_name_tmin = "temp_min",
  var_name_tmax = "temp_max"
)

# Using data with station column (backward compatibility)
write_climate(data = weather_with_station, path = "climate/")

# Specify year range explicitly
write_climate(
  data = weather,
  path = "weather/",
  site_name = "Wakanda",
  syear = 2000,
  eyear = 2010
)
} # }
```
