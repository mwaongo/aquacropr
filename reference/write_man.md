# Write AquaCrop Management File

Creates an AquaCrop v7.0+ management (.MAN) file that defines field and
crop management practices for simulation. Management files specify
practices such as mulch coverage, soil fertility stress, weed
competition, and harvesting strategies.

## Usage

``` r
write_man(path = "MANAGEMENT/", site_name, eol = NULL, params = list())
```

## Arguments

- path:

  Character. Directory path where the .MAN file will be created. The
  directory is created automatically if it doesn't exist. Default:
  `"MANAGEMENT/"`.

- site_name:

  Character. Name identifier for the management scenario. This name is
  used as the filename (e.g., `"high-fertility"` creates
  `"high-fertility.MAN"`). Default: `"generic-management"`.

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected.

- params:

  Named list of management parameters to override defaults. Parameter
  names must be `var_02` through `var_21`. Any parameters not specified
  will use default values. Set to `NULL` or
  [`list()`](https://rdrr.io/r/base/list.html) to use all defaults. See
  [`ManData`](https://mwaongo.github.io/aquacropr/reference/ManData.md)
  for complete parameter descriptions, valid ranges, and default values.

## Value

Invisibly returns the full path to the created .MAN file as a character
string. The main effect is writing the file to disk.

## Details

### Management Parameters

Management practices are controlled by 20 parameters (`var_02` to
`var_21`) covering:

- Surface management (mulching, bunds, runoff)

- Soil fertility stress

- Weed competition

- Harvest management (single or multiple cuttings)

For detailed descriptions of all parameters, their valid ranges, units,
and default values, see
[`ManData`](https://mwaongo.github.io/aquacropr/reference/ManData.md).

You only need to specify parameters that differ from defaults.
Unspecified parameters automatically use default values from
[`ManData`](https://mwaongo.github.io/aquacropr/reference/ManData.md).

### File Format

The function creates a colon-delimited text file compatible with
AquaCrop v7.0+:

- Filename: `<site_name>.MAN`

- Format: `value : description`

- Each parameter on a separate line

- Header contains management scenario name and key parameters

## See also

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
[`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md),
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md),
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
# Example 1: Use all default values
write_man(
  path = "MANAGEMENT/",
  site_name = "default"
)

# Example 2: High fertility with moderate mulching
# See ?ManData for parameter details
write_man(
  path = "MANAGEMENT/",
  site_name = "high-fertility",
  params = list(
    var_05 = 10, # Low fertility stress (high fertility)
    var_03 = 30, # 30% mulch cover
    var_04 = 50 # 50% evaporation reduction
  )
)

# Example 3: Low fertility, no mulch
write_man(
  path = "MANAGEMENT/",
  site_name = "low-fertility",
  params = list(
    var_05 = 70, # High fertility stress (poor soil)
    var_03 = 0, # No mulch
    var_04 = 0 # No mulch effect
  )
)

# Example 4: Weed competition scenario
# Check ?ManData for weed-related parameters
write_man(
  path = "MANAGEMENT/",
  site_name = "with-weeds",
  params = list(
    var_05 = 50, # Moderate fertility stress
    var_09 = 20, # Weed cover at closure
    var_10 = 10, # Weed increase in mid-season
    var_12 = 50 # Weed replacement
  )
)

# Example 5: Multiple cutting management (e.g., forage)
write_man(
  path = "MANAGEMENT/",
  site_name = "multiple-cuts",
  params = list(
    var_13 = 1, # Enable multiple cuttings
    var_14 = 10, # Canopy after cut
    var_15 = 20, # CGC increase after cut
    var_16 = 45, # First cutting day
    var_17 = 30 # Cutting window
  )
)

# Example 6: Complete management scenario
# Consult ?ManData to understand each parameter
irrigation_params <- list(
  var_03 = 40, # Mulch cover
  var_04 = 70, # Mulch effect
  var_05 = 20, # Fertility stress
  var_06 = 0.15, # Bund height
  var_07 = 10, # Runoff not affected
  var_08 = 80 # Runoff prevented
)

write_man(
  path = "MANAGEMENT/",
  site_name = "irrigated-field",
  params = irrigation_params,
  eol = "linux"
)

# Example 7: Create station-specific management
station <- "grid_001"
write_man(
  path = "MANAGEMENT/",
  site_name = station,
  params = list(var_05 = 35)
)
} # }

```
