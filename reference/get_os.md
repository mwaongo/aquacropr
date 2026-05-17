# Detect Operating System

Detects the current operating system and returns a standardized name
compatible with AquaCrop binary distributions.

## Usage

``` r
get_os()
```

## Value

Character string: one of "windows", "linux", or "macos".

## Details

The function uses a two-step detection process:

1.  Attempts to use `Sys.info()['sysname']` (most reliable)

2.  Falls back to `.Platform$OS.type` and `R.version$os` for edge cases
    where [`Sys.info()`](https://rdrr.io/r/base/Sys.info.html) is
    unavailable

Operating system names are normalized to match AquaCrop conventions:

- Darwin → macos

- Linux → linux

- Windows → windows
