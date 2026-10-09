# Run an AquaCrop simulation

Executes the AquaCrop executable in the current working directory. The
directory must contain the AquaCrop executable (`aquacrop.exe` on
Windows, `aquacrop` on Linux/macOS) together with a valid AquaCrop
project structure.

## Usage

``` r
run_aquacrop(verbose = TRUE)
```

## Arguments

- verbose:

  Logical. Should progress messages be printed? Defaults to `TRUE`.

## Value

Invisibly returns the AquaCrop exit status (`0` indicates success).

## Details

Before execution, all files in the `OUTP/` directory are removed to
prevent mixing outputs from previous simulations. Temporary files
created by AquaCrop (`AllDone.OUT` and `ListProjectsLoaded.OUT`) are
automatically removed after execution.

## See also

[`init_aquacrop()`](https://mwaongo.github.io/aquacropr/reference/init_aquacrop.md),
[`install_binaries()`](https://mwaongo.github.io/aquacropr/reference/install_binaries.md)

## Examples

``` r
if (FALSE) { # \dontrun{
init_aquacrop("~/my-project")
setwd("~/my-project")

run_aquacrop()
run_aquacrop(verbose = FALSE)
} # }
```
