# Initialize AquaCrop Project Structure

Creates a directory structure for AquaCrop crop water productivity model
simulations and installs the AquaCrop binary. After initialization, you
can run simulations directly in the project directory.

## Usage

``` r
init_aquacrop(
  path = ".",
  version = NULL,
  os = NULL,
  force = FALSE,
  overwrite = FALSE,
  use_rproject = TRUE
)
```

## Arguments

- path:

  Character string specifying the root directory where the project
  should be created. Default is the current working directory (`"."`).

- version:

  AquaCrop version to install (NULL = latest, e.g. "7.1")

- os:

  Operating system ("windows", "linux", "macos", NULL = auto-detect)

- force:

  Force reinstall binary even if exists (default: FALSE)

- overwrite:

  Logical; if TRUE, overwrites existing directory structure. Default:
  FALSE.

- use_rproject:

  Logical. If TRUE, creates an RStudio project file (.Rproj) in the
  project directory. Default is TRUE.

## Value

Invisibly returns the normalized path to the created directory.

## Details

The function creates the following directory structure:

- CLIMATE/:

  Climate input files (temperature, rainfall, ETo)

- CROP/:

  Crop parameter files

- GWT/:

  Groundwater table files

- IRR/:

  Irrigation schedule files

- LIST/:

  Project simulation files `*.PRM`

- MANAGEMENT/:

  Field management practice files `*.MAN`

- OBS/:

  Field observation files

- OUTP/:

  Simulation output files

- PARAM/:

  Program parameters files `*.PPn`

- SIMUL/:

  Simulation configuration files

- SOIL/:

  Soil profile and water content characteristic files

The AquaCrop binary is automatically downloaded and installed in the
project directory. The appropriate executable for your operating system
(Windows, macOS, or Linux) is placed directly in the root of the
project.

## References

<https://www.fao.org/aquacrop/> for AquaCrop documentation

## See also

[install_binaries](https://mwaongo.github.io/aquacropr/reference/install_binaries.md),
[run_aquacrop](https://mwaongo.github.io/aquacropr/reference/run_aquacrop.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize new project
init_aquacrop("~/my-aquacrop-project")

# Then work in that directory
setwd("~/my-aquacrop-project")
run_aquacrop()

# Initialize with specific version
init_aquacrop("~/project", version = "7.1")

# Overwrite existing structure
init_aquacrop("~/project", overwrite = TRUE, force = TRUE)
} # }
```
