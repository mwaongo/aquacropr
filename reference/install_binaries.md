# Install AquaCrop Binary

Downloads and installs the AquaCrop executable for the current operating
system. Automatically detects OS, manages versioning with intelligent
fallbacks, and caches downloads to avoid repeated transfers.

## Usage

``` r
install_binaries(
  version = NULL,
  os = NULL,
  path = getwd(),
  force = FALSE,
  compiler = "gfortran",
  keep_source = FALSE
)
```

## Arguments

- version:

  Character string specifying the AquaCrop version to install. If NULL
  (default), installs the latest available version. Version must be \>=
  7.0. Accepts formats like "7.1", "v7.1", or "7.1.0". Use "dev" to
  compile from latest source code. If requested version is \< 7.0 or \>
  latest, falls back to latest version.

- os:

  Character string specifying the operating system: "windows", "linux",
  or "macos". If NULL (default), automatically detects current OS.

- path:

  Character string specifying the installation directory path where the
  AquaCrop executable will be installed. Current working directory by
  default.

- force:

  Logical. If TRUE, reinstalls even if executable already exists.
  Default: FALSE.

- compiler:

  Character. Fortran compiler for dev builds. Default: "gfortran". Only
  used when version = "dev".

- keep_source:

  Logical. Keep source code after compilation. Default: FALSE. Only used
  when version = "dev".

## Value

Invisibly returns the installed version number as a character string.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install latest version
install_binaries(path = "~/aquacrop")

# Install specific version
install_binaries(version = "7.1", path = "~/aquacrop")

# Install dev version (compile from source)
install_binaries(version = "dev", path = "~/aquacrop")

# Force reinstall
install_binaries(version = "7.1", path = "~/aquacrop", force = TRUE)
} # }
```
