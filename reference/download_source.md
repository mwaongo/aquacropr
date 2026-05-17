# Download AquaCrop Source Code

Downloads and extracts AquaCrop source code from the GitHub main branch.

## Usage

``` r
download_source(
  dest_dir = fs::path_temp("aquacrop_source"),
  url = "https://github.com/KUL-RSDA/AquaCrop/archive/refs/heads/main.zip",
  timeout = 120,
  verbose = TRUE
)
```

## Arguments

- dest_dir:

  Character. Directory where source will be extracted. Default: a
  temporary directory.

- url:

  Character. URL to download source code. Default: official GitHub
  repository main branch.

- timeout:

  Integer. Download timeout in seconds. Default: 120.

- verbose:

  Logical. If TRUE (default), prints download progress.

## Value

Character. Path to the extracted AquaCrop source directory.

## Examples

``` r
if (FALSE) { # \dontrun{
source_dir <- download_source()
source_dir <- download_source(dest_dir = "~/builds")
} # }
```
