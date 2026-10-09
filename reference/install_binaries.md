# Install AquaCrop Binary

Downloads and installs the AquaCrop executable for the requested
operating system. The function automatically detects the current
platform, resolves the requested AquaCrop version, caches downloaded
archives, and installs the executable in the selected directory.

## Usage

``` r
install_binaries(
  version = NULL,
  os = NULL,
  path = getwd(),
  force = FALSE,
  compiler = NULL,
  keep_source = FALSE
)
```

## Arguments

- version:

  Character. AquaCrop version to install. If `NULL`, installs the latest
  supported binary release. Accepted formats include `"7.1"`, `"v7.1"`,
  and `"7.1.0"`. Use `"dev"` to compile from source.

- os:

  Character. Operating system: `"windows"`, `"linux"`, or `"macos"`. If
  `NULL`, detected automatically.

- path:

  Character. Installation directory. Defaults to the current working
  directory.

- force:

  Logical. If `TRUE`, reinstalls an existing executable. Default:
  `FALSE`.

- compiler:

  Character or `NULL`. Fortran compiler used only when
  `version = "dev"`. If `NULL`, detected automatically.

- keep_source:

  Logical. Keep source code after compilation when `version = "dev"`.
  Default: `FALSE`.

## Value

Invisibly returns the installed AquaCrop version.

## Details

Use `version = "dev"` to compile the latest development version from
source.

## Examples

``` r
if (FALSE) { # \dontrun{
install_binaries()
install_binaries(version = "7.1", path = "~/aquacrop")
install_binaries(version = "dev", path = "~/aquacrop")
install_binaries(force = TRUE)
} # }
```
