# Run AquaCrop simulation

Execute AquaCrop in the current directory. The directory must contain
the AquaCrop executable (aquacrop.exe on Windows, aquacrop on
Linux/macOS).

## Usage

``` r
run_aquacrop(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. Print messages (default: TRUE).

## Value

Invisibly returns the exit status (0 = success, non-zero = error).

## Details

This function must be run from a directory containing the AquaCrop
executable. The directory should also contain the standard AquaCrop
folder structure (CLIMATE/, CROP/, SOIL/, MANAGEMENT/, LIST/, OUTP/,
etc.).

The OUTP/ directory is cleaned before running to avoid mixing results
from different runs. Temporary files (AllDone.OUT,
ListProjectsLoaded.OUT) are removed after execution.

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize project and run
init_aquacrop("~/my-project")
setwd("~/my-project")
run_aquacrop()

# Silent mode
run_aquacrop(verbose = FALSE)
} # }
```
