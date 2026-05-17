# Build AquaCrop from Source

Compiles the AquaCrop executable from source code using make and a
Fortran compiler. Accepts a pre-resolved compiler to avoid redundant
detection calls when invoked from install_source.

## Usage

``` r
build_source(source_dir, compiler = NULL, target = "all", verbose = TRUE)
```

## Arguments

- source_dir:

  Character. Path to the AquaCrop source directory.

- compiler:

  Character or NULL. Fortran compiler binary name or full path,
  optionally including flags (e.g., "gfortran -arch x86_64"). If NULL
  (default), auto-detected via .detect_fc().

- target:

  Character. Build target passed to make. One of "all" (default,
  produces both executable and library), "bin" (executable only), or
  "lib" (library only).

- verbose:

  Logical. If TRUE (default), prints build information.

## Value

Character. Path to the compiled executable.

## Examples

``` r
if (FALSE) { # \dontrun{
exe_path <- build_source("AquaCrop")
exe_path <- build_source("AquaCrop", compiler = "ifort", target = "bin")
} # }
```
