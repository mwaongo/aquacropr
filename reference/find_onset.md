# Compute AquaCrop Growing Season Onset Day per Year

Reads a CAL file, determines the onset method, and computes the onset
day-of-year (DOY) for each year in the record. Supports:

- Fixed onset.

- AquaCrop rainfall criteria 1-4.

- Criterion 5: generalised Sivakumar (1988).

- Criterion 6: Marteau (2009).

- Criterion 7: fuzzy logic (Waongo et al., 2014).

- Thermal criteria 1-4.

## Usage

``` r
find_onset(
  site_name,
  cal_path = "CAL/",
  climate_path = "CLIMATE/",
  years = NULL,
  base_path = getwd()
)
```

## Arguments

- site_name:

  Character. Station name used to locate both the CAL file and the
  climate files (.PLU, .Tnx, .ETo).

- cal_path:

  Character. Path to the CAL directory. Default: "CAL/".

- climate_path:

  Character. Path to the climate directory. Default: "CLIMATE/".

- years:

  Integer vector or NULL. Years to compute onset for. If NULL (default),
  all years present in the climate file are used.

- base_path:

  Character. Base absolute path. Default: current working directory.

## Value

A data.frame with columns:

- year:

  Integer.

- onset_doy:

  Integer. Onset DOY, or last day of the search window if criterion not
  met.

- onset_date:

  Date.

## See also

[`read_cal`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
[`read_plu`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_eto`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_tnx`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# AquaCrop rainfall criterion 2
find_onset("site_01")

# Generalised Sivakumar (parameters in CAL file)
find_onset("site_siv")

# Marteau 2009 (parameters in CAL file)
find_onset("site_mrt")

# Fuzzy logic Waongo et al. 2014 (parameters in CAL file)
find_onset("site_fuz")
} # }
```
