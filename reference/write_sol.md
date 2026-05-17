# Write AquaCrop Soil Profile File (.SOL)

Writes an AquaCrop v7.0 (August 2022) soil profile file (`.SOL`)
containing hydraulic and physical properties for each soil horizon.
Optionally, when `write_sw0 = TRUE`, also writes the matching initial
soil-water content file (`.SW0`) by calling
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md).

## Usage

``` r
write_sol(
  site_name = "default-site",
  path = "SOIL/",
  cn = 46,
  rew = 5,
  texture = c("loam", "silt loam"),
  thickness = c(0.5, 1),
  eol = NULL,
  write_sw0 = TRUE,
  initial_water = "WP",
  salinity = "none",
  initial_cc = -9,
  initial_biomass = 0,
  initial_root_depth = -9,
  bund_water = 0,
  bund_ec = 0,
  swo_layers = NULL
)

write_soil(
  site_name = "default-site",
  path = "SOIL/",
  cn = 46,
  rew = 5,
  texture = c("loam", "silt loam"),
  thickness = c(0.5, 1),
  eol = NULL,
  write_sw0 = TRUE,
  initial_water = "WP",
  salinity = "none",
  initial_cc = -9,
  initial_biomass = 0,
  initial_root_depth = -9,
  bund_water = 0,
  bund_ec = 0,
  swo_layers = NULL
)
```

## Arguments

- site_name:

  Name of the soil profile (used in the filename). Default:
  `"default-site"`.

- path:

  Directory where the `.SOL` file will be written. Default: `"SOIL/"`.

- cn:

  Curve number for runoff estimation (0–100). Default: `46`.

- rew:

  Readily evaporable water (mm) from the top soil layer. Default: `5`.

- texture:

  Character vector of soil texture names (USDA classes). Must match
  entries in the `SoilWater` dataset.

- thickness:

  Numeric vector of horizon thicknesses (m). Must have the same length
  as `texture`.

- eol:

  End-of-line character style. Options: "windows","linux", or "macos".
  If `NULL` (default), eol is auto-detected.

- write_sw0:

  Logical; if `TRUE`, also write the corresponding `.SW0` file by
  calling
  [`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md).
  Default: `TRUE`.

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

- swo_layers:

  Optional `data.frame` to pass as `layers` to
  [`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md).
  If `NULL` (default),
  [`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
  will construct layers from `texture` and `thickness`.

## Value

Invisibly returns the path to the written `.SOL` file.

## Details

### File content

The `.SOL` file defines hydraulic and physical parameters for each soil
horizon, including:

- **SAT**: Saturation (vol %)

- **FC**: Field capacity (vol %)

- **WP**: Wilting point (vol %)

- **Ksat**: Saturated hydraulic conductivity (mm day⁻¹)

- **Penetrability, Gravel, CRa, CRb**: Additional physical
  characteristics

The number of horizons is inferred from the lengths of `texture` and
`thickness`.

### Writing the `.SW0` file

When `write_sw0 = TRUE`,
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
is called using the same `path`, `site_name` (as `soil_name`),
`texture`, `thickness` (as `layer_thickness`), and `eol`. The following
arguments are passed through: `initial_water`, `salinity`, `initial_cc`,
`initial_biomass`, `initial_root_depth`, `bund_water`, `bund_ec`, and
`swo_layers` (as `layers`).

## See also

[SoilWater](https://mwaongo.github.io/aquacropr/reference/SoilWater.md)
for hydraulic property data,
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md)
for initial water content files (.SW0),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md)
for crop files,
[`write_man()`](https://mwaongo.github.io/aquacropr/reference/write_man.md)
for management files.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic soil profile (single horizon)
write_sol(
  path = "soil/",
  site_name = "field1",
  texture = "loam",
  thickness = 1.0
)

# Two horizons with automatic SW0 generation (field capacity)
write_sol(
  path = "soil/",
  site_name = "layered",
  cn = 65,
  rew = 7,
  texture = c("sandy loam", "clay loam"),
  thickness = c(0.4, 0.6),
  initial_water = "FC",
  salinity = "none"
)

# Write only the .SOL file (no SW0)
write_sol(
  site_name = "soil_only",
  texture = "loam",
  thickness = 1.0,
  write_sw0 = FALSE
)
} # }
```
