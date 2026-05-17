# Write AquaCrop Soil Initial Water Content File

Write an AquaCrop v7.0 (August 2022) and higher soil initial water
content (.SW0) file that specifies the initial water content for each
soil layer at the start of the simulation. This file defines the soil
moisture status at planting or simulation start.

## Usage

``` r
write_swo(
  path = "SOIL/",
  soil_name = "default-soil-init",
  texture = "loam",
  eol = NULL,
  initial_cc = -9,
  initial_biomass = 0,
  initial_root_depth = -9,
  bund_water = 0,
  bund_ec = 0,
  initial_water = "WP",
  salinity = "none",
  layer_thickness = 1,
  layers = NULL
)

write_sw0(
  path = "SOIL/",
  soil_name = "default-soil-init",
  texture = "loam",
  eol = NULL,
  initial_cc = -9,
  initial_biomass = 0,
  initial_root_depth = -9,
  bund_water = 0,
  bund_ec = 0,
  initial_water = "WP",
  salinity = "none",
  layer_thickness = 1,
  layers = NULL
)
```

## Arguments

- path:

  Directory path where the .SW0 file will be written. Default = "SOIL/"

- soil_name:

  Name identifier for the soil initial conditions file (used in
  filename). Default = "default-soil-init"

- texture:

  Character vector of soil texture names for each layer. Valid textures:
  "sand", "loamy sand", "sandy loam", "loam", "silt loam", "silt",
  "sandy clay loam", "clay loam", "silty clay loam", "sandy clay",
  "silty clay", "clay". Default = "loam"

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected. Options: "windows", "unix",
  "linux", or "macOS". Default = "windows"

- initial_cc:

  Numeric. Initial canopy cover that can be reached without water stress
  (%). Use -9.00 for automatic calculation by AquaCrop. Default = -9.00

- initial_biomass:

  Numeric. Biomass produced before simulation start (ton/ha). Default =
  0.000

- initial_root_depth:

  Numeric. Initial effective rooting depth without water stress (m). Use
  -9.00 for automatic calculation. Default = -9.00

- bund_water:

  Numeric. Water layer stored between soil bunds if present (mm).
  Default = 0.0

- bund_ec:

  Numeric. Electrical conductivity of water between bunds (dS/m).
  Default = 0.00

- initial_water:

  Character string or numeric. Specifies initial water content for all
  layers. Options:

  - **"WP"** or **"wilting_point"**: Sets to wilting point based on soil
    texture

  - **"FC"** or **"field_capacity"**: Sets to field capacity (1.5 × WP)

  - **"SAT"** or **"saturation"**: Sets to saturation (2.5 × WP)

  - **Numeric value**: Sets all layers to specified value (0-100%)

  Only used if layers parameter is not provided. Default = "WP"

- salinity:

  Character string or numeric specifying soil salinity level for all
  layers. Options: "none", "very slight", "slight", "moderate",
  "strong", "very strong", or numeric ECe value (0-100 dS/m). Only used
  if layers parameter is not provided. Default = "none"

- layer_thickness:

  Numeric vector specifying thickness of each soil layer in meters. Only
  used if layers parameter is not provided. Default = 1.0 (single 1m
  layer)

- layers:

  Data frame with soil layer information. If provided, overrides
  initial_water. Must contain columns:

  - **thickness**: Layer thickness in meters (m)

  - **water_content**: Volumetric water content (vol%)

  - **ece**: Electrical conductivity (dS/m). Optional, defaults to 0

## Value

Invisibly returns the full path to the created .SW0 file.

## Details

The .SW0 file defines the initial soil water conditions for each layer
at the start of the AquaCrop simulation. These conditions significantly
affect early crop growth and development.

### Initial Conditions:

- **Canopy cover**: Set to -9.00 to let AquaCrop calculate, or specify
  actual value (0-100%)

- **Biomass**: Usually 0 at planting, but can specify pre-existing
  biomass

- **Root depth**: Set to -9.00 for automatic, or specify initial depth
  in meters

### Soil Textures and Default Wilting Points:

The function uses the
[`SWOData`](https://mwaongo.github.io/aquacropr/reference/SWOData.md)
dataset which contains wilting point values for 12 standard USDA soil
textures. See `data(SWOData)` for details.

### Water Content Levels:

- **Wilting Point (WP)**: Minimum water for plant survival
  (texture-specific)

- **Field Capacity (FC)**: WP × 1.5 (approximate)

- **Saturation (SAT)**: WP × 2.5 (approximate)

### Salinity Levels:

Use
[`salinity_to_ece`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md)
to convert qualitative descriptions to ECe values:

- "none": 0 dS/m (non-saline)

- "slight": 3 dS/m (slightly saline)

- "moderate": 6 dS/m (moderately saline)

- "strong": 12 dS/m (strongly saline)

- "very strong": 20 dS/m (very strongly saline)

## See also

- [`SWOData`](https://mwaongo.github.io/aquacropr/reference/SWOData.md)
  for soil texture wilting point data

- [`salinity_to_ece`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md)
  for salinity level conversions

- [`write_sol`](https://mwaongo.github.io/aquacropr/reference/write_sol.md)
  for writing soil profile characteristics

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
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple: Single layer at wilting point, no salinity
write_swo(
  path = "soil/",
  soil_name = "loam-wp",
  texture = "loam",
  initial_water = "WP"
)

# Single layer at field capacity
write_swo(
  soil_name = "sandy-loam-fc",
  texture = "sandy loam",
  initial_water = "FC"
)

# With salinity
write_swo(
  soil_name = "saline-soil",
  texture = "clay loam",
  initial_water = "WP",
  salinity = "moderate"
)

# Multiple layers with different textures
write_swo(
  soil_name = "layered-profile",
  texture = c("sandy loam", "loam", "clay loam"),
  layer_thickness = c(0.3, 0.4, 0.3),
  initial_water = "FC",
  salinity = "slight"
)

# Mid-season with existing crop
write_swo(
  soil_name = "mid-season-maize",
  texture = "loam",
  initial_water = "FC",
  initial_cc = 60,
  initial_biomass = 3.5,
  initial_root_depth = 0.7
)

# Custom numeric values
write_swo(
  soil_name = "custom",
  texture = "loam",
  layer_thickness = c(0.5, 0.5),
  initial_water = 28,
  salinity = 4.5
)

# Manual layers (full control)
layers <- data.frame(
  thickness = c(0.3, 0.4, 0.3),
  water_content = c(20, 25, 30),
  ece = c(0, 3, 6)
)

write_swo(
  soil_name = "manual-layers",
  texture = c("Sandy Loam", "Loam", "Clay Loam"),
  layers = layers
)
} # }
```
