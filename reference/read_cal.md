# Parse an AquaCrop Calendar (.CAL) File

Reads a .CAL file and returns a structured list of onset parameters.
Supports fixed onset, rainfall criteria 1-7, and thermal criteria 1-4.

## Usage

``` r
read_cal(file)
```

## Arguments

- file:

  Character. Full path to the .CAL file.

## Value

A named list with elements:

- onset:

  Character. "fixed", "rainfall", or "thermal".

- fixed_day:

  Integer or NA.

- window_start:

  Integer or NA.

- window_length:

  Integer or NA.

- criterion_internal:

  Integer or NA.

- criterion:

  Integer or NA. User-facing number (1-7 rainfall, 1-4 thermal).

- preset_value:

  Numeric or NA.

- successive_days:

  Integer or NA.

- occurrences:

  Integer or NA.

- rday:

  Integer or NA. Criterion 5 only.

- dspell:

  Integer or NA. Criterion 5 only.

- min_weekly_rain:

  Numeric or NA. Criterion 6 only.

- spell_days:

  Integer or NA. Criterion 6 only.

- lookahead_days:

  Integer or NA. Criteria 5 and 6 only.

- cum_rain_upper:

  Numeric or NA. Criterion 7 only.

- wet_days_lower:

  Integer or NA. Criterion 7 only.

- wet_days_upper:

  Integer or NA. Criterion 7 only.

- dry_spell_lower:

  Integer or NA. Criterion 7 only.

- dry_spell_upper:

  Integer or NA. Criterion 7 only.

- fuzzy_threshold:

  Numeric or NA. Criterion 7 only.

## Details

Criterion 5 (generalised Sivakumar) carries three extra fields: `rday`,
`dspell`, and `lookahead_days`.

Criterion 6 (Marteau 2009) carries three extra fields:
`min_weekly_rain`, `spell_days`, and `lookahead_days`.

Criterion 7 (fuzzy logic, Waongo et al. 2014) carries six extra fields:
`cum_rain_upper`, `wet_days_lower`, `wet_days_upper`, `dry_spell_lower`,
`dry_spell_upper`, and `fuzzy_threshold`.

All other criteria return `NA` for these fields.

## See also

Other AquaCrop readers:
[`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
[`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
[`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
[`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md),
[`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard rainfall criterion
cal <- read_cal("CAL/station_01.CAL")
cal$onset        # "rainfall"
cal$criterion    # 2
cal$preset_value # 30

# Sivakumar criterion
cal5 <- read_cal("CAL/station_siv.CAL")
cal5$criterion      # 5
cal5$rday           # 1
cal5$dspell         # 7
cal5$lookahead_days # 30

# Marteau criterion
cal6 <- read_cal("CAL/station_mrt.CAL")
cal6$criterion       # 6
cal6$min_weekly_rain # 5
cal6$spell_days      # 7
cal6$lookahead_days  # 20

# Fuzzy logic criterion
cal7 <- read_cal("CAL/station_fuz.CAL")
cal7$criterion      # 7
cal7$cum_rain_upper # 40
cal7$fuzzy_threshold # 0.5
} # }
```
