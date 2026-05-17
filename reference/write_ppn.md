# Write an AquaCrop Program Parameter (.PPn) File

Creates an AquaCrop program parameter file with default values that can
be selectively overridden via the `args` list.

## Usage

``` r
write_ppn(site_name, args = list(), path = "SIMUL/", eol = NULL)
```

## Arguments

- site_name:

  Character. Station name (used to build the output filename).

- args:

  Named list of parameter values to override. Any parameter not supplied
  keeps its AquaCrop default value. Supported names (with defaults):
  `evap_decline` (4), `ke_x` (1.10), `cc_threshold_hi` (5),
  `root_expansion_start` (70), `max_root_expansion` (5.00),
  `shape_root_stress` (-6), `germination_swc` (20), `p_adjustment`
  (1.0), `aeration_days` (3), `senescence_exponent` (1.00),
  `p_sen_decrease` (12), `topsoil_thickness` (10), `evap_depth` (30),
  `cn_depth` (0.30), `cn_amc` (1), `salt_diffusion` (20),
  `salt_solubility` (100), `capillary_shape` (16), `tmin_default`
  (12.0), `tmax_default` (28.0), `gdd_method` (3), `rainfall_scs` (1),
  `eff_rainfall_pct` (70), `showers_per_decade` (2), `evap_reduction`
  (5).

- path:

  Character. Output directory. Default: `"SIMUL/"`.

- eol:

  Character. End-of-line style: `"windows"`, `"linux"`, or `"macos"`. If
  `NULL` (default), auto-detected.

## Value

Invisibly returns the output file path.

## See also

Other AquaCrop file writers:
[`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md),
[`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
[`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
[`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
[`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
[`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md),
[`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md),
[`write_man()`](https://mwaongo.github.io/aquacropr/reference/write_man.md),
[`write_obs()`](https://mwaongo.github.io/aquacropr/reference/write_obs.md),
[`write_off()`](https://mwaongo.github.io/aquacropr/reference/write_off.md),
[`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
[`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
[`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
[`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Default parameters
write_ppn("grid_001")

# Override temperature defaults
write_ppn("grid_001", args = list(tmin_default = 15.0, tmax_default = 32.0))
} # }
```
