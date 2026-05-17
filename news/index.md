# Changelog

## aquacropr 0.2.0

### Breaking changes

- Package renamed from `aquacroptools` to `aquacropr`. Update all
  [`library()`](https://rdrr.io/r/base/library.html) calls and imports
  accordingly.
- [`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md)
  was re-implemented as a standalone improved function (previously part
  of `readers.R`).
- Dependency on `snakecase` removed; `withr` added.

### New features

#### Installation from source

- [`install_source()`](https://mwaongo.github.io/aquacropr/reference/install_source.md)
  — compile and install AquaCrop from Fortran source code
  (cross-platform).
- [`build_source()`](https://mwaongo.github.io/aquacropr/reference/build_source.md)
  — build the AquaCrop binary from source.
- [`download_source()`](https://mwaongo.github.io/aquacropr/reference/download_source.md)
  — download the AquaCrop source code.

#### Onset detection (fuzzy logic)

- [`find_onset()`](https://mwaongo.github.io/aquacropr/reference/find_onset.md)
  — detect the rainy season onset using a fuzzy logic algorithm. See
  [`vignette("sowingdate")`](https://mwaongo.github.io/aquacropr/articles/sowingdate.md)
  for a full worked example.

#### New writers

- [`write_cal()`](https://mwaongo.github.io/aquacropr/reference/write_cal.md)
  /
  [`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md)
  — write AquaCrop calendar (`.CAL`) files, single and batch.
- [`write_gwt()`](https://mwaongo.github.io/aquacropr/reference/write_gwt.md)
  /
  [`write_gwt_batch()`](https://mwaongo.github.io/aquacropr/reference/write_gwt_batch.md)
  — write groundwater table (`.GWT`) files.
- [`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md)
  /
  [`write_irrig_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irrig_batch.md)
  — write irrigation schedule (`.IRR`) files.
- [`write_obs()`](https://mwaongo.github.io/aquacropr/reference/write_obs.md)
  /
  [`write_obs_batch()`](https://mwaongo.github.io/aquacropr/reference/write_obs_batch.md)
  — write field observation (`.OBS`) files.
- [`write_off()`](https://mwaongo.github.io/aquacropr/reference/write_off.md)
  /
  [`write_off_batch()`](https://mwaongo.github.io/aquacropr/reference/write_off_batch.md)
  — write off-season condition files.
- [`write_ppn()`](https://mwaongo.github.io/aquacropr/reference/write_ppn.md)
  — write plot/project parameter files.
- [`write_sim()`](https://mwaongo.github.io/aquacropr/reference/write_sim.md)
  — write simulation settings files.
- [`create_irr_events()`](https://mwaongo.github.io/aquacropr/reference/create_irr_events.md)
  /
  [`create_irr_schedule()`](https://mwaongo.github.io/aquacropr/reference/create_irr_schedule.md)
  — helper functions to build irrigation event data frames.

#### New readers

- [`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md)
  — read AquaCrop calendar files.
- [`read_day_out()`](https://mwaongo.github.io/aquacropr/reference/read_day_out.md)
  — read AquaCrop daily output files as a tibble.
- [`read_season_out()`](https://mwaongo.github.io/aquacropr/reference/read_season_out.md)
  now returns a `tibble` instead of a plain data frame.

#### New validators

- [`is_cli()`](https://mwaongo.github.io/aquacropr/reference/is_cli.md),
  [`is_eto()`](https://mwaongo.github.io/aquacropr/reference/is_eto.md),
  [`is_tnx()`](https://mwaongo.github.io/aquacropr/reference/is_tnx.md)
  — validate climate input file formats.

### Improvements

- [`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md)
  /
  [`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md)
  — major overhaul: dynamic optional file passing (SW0, GWT, IRR),
  improved day-of-year handling, calendar file integration via
  [`find_onset()`](https://mwaongo.github.io/aquacropr/reference/find_onset.md) +
  [`read_cal()`](https://mwaongo.github.io/aquacropr/reference/read_cal.md),
  and better warnings for missing optional files.
- [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md)
  — default output path changed to `"CLIMATE/"`.
- [`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md)
  — additional parameters added for finer control.
- [`write_fwf()`](https://mwaongo.github.io/aquacropr/reference/write_fwf.md)
  — auto-detects EOF when `NULL` is passed.
- [`install_binaries()`](https://mwaongo.github.io/aquacropr/reference/install_binaries.md)
  — fixed cross-platform behavior; version 7.3 (typo-tagged release) is
  now excluded.
- [`init_aquacrop()`](https://mwaongo.github.io/aquacropr/reference/init_aquacrop.md)
  — improved startup messaging and initialization reliability.
- `read_fwf()` — output coerced to numeric for single-column results.
- Internal codebase refactored from monolithic `readers.R` into focused
  modules: `read_inputs.R`, `read_outputs.R`, `read_cal.R`,
  `utils-batch.R`, `utils-climate.R`, `utils-readers.R`,
  `utils-validation.R`, `utils-misc.R`.

### Bug fixes

- Fixed regex escaping in the internal clean-directory utility
  (#internal).
- Fixed crop duration handling and associated warnings in
  [`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md).
- Fixed section header indentation in
  [`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md).
- Fixed edge-case crash in
  [`find_onset()`](https://mwaongo.github.io/aquacropr/reference/find_onset.md).
- Fixed
  [`install_source()`](https://mwaongo.github.io/aquacropr/reference/install_source.md)
  for cross-platform compilation.

### Documentation

- Three new vignettes: `settingup`, `sowingdate`, `regionalsim`.
- pkgdown website updated and rebuilt.
- README substantially revised to reflect new package name and
  capabilities.
- Repository URLs updated to <https://github.com/mwaongo/aquacropr>.

------------------------------------------------------------------------

## aquacropr 0.1.0

- Initial release as `aquacroptools`.
- Core writers:
  [`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md),
  [`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  [`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md),
  [`write_sol()`](https://mwaongo.github.io/aquacropr/reference/write_sol.md),
  [`write_swo()`](https://mwaongo.github.io/aquacropr/reference/write_swo.md),
  [`write_man()`](https://mwaongo.github.io/aquacropr/reference/write_man.md),
  [`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
  [`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md),
  [`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md),
  [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md),
  [`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md),
  [`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md).
- Core readers:
  [`read_cli()`](https://mwaongo.github.io/aquacropr/reference/read_cli.md),
  [`read_eto()`](https://mwaongo.github.io/aquacropr/reference/read_eto.md),
  [`read_plu()`](https://mwaongo.github.io/aquacropr/reference/read_plu.md),
  [`read_tnx()`](https://mwaongo.github.io/aquacropr/reference/read_tnx.md).
- [`install_binaries()`](https://mwaongo.github.io/aquacropr/reference/install_binaries.md)
  — download and install pre-built AquaCrop binaries.
- [`init_aquacrop()`](https://mwaongo.github.io/aquacropr/reference/init_aquacrop.md)
  — initialize an AquaCrop project directory.
- [`run_aquacrop()`](https://mwaongo.github.io/aquacropr/reference/run_aquacrop.md)
  — run an AquaCrop simulation.
- Helper utilities:
  [`build_crop_parameters()`](https://mwaongo.github.io/aquacropr/reference/build_crop_parameters.md),
  [`calculate_crop_stages()`](https://mwaongo.github.io/aquacropr/reference/calculate_crop_stages.md),
  [`calculate_plant_density()`](https://mwaongo.github.io/aquacropr/reference/calculate_plant_density.md),
  [`day_number()`](https://mwaongo.github.io/aquacropr/reference/day_number.md),
  [`to_aquacrop_day()`](https://mwaongo.github.io/aquacropr/reference/to_aquacrop_day.md),
  [`weather()`](https://mwaongo.github.io/aquacropr/reference/weather.md),
  [`round_to()`](https://mwaongo.github.io/aquacropr/reference/round_to.md),
  [`ece_to_salinity()`](https://mwaongo.github.io/aquacropr/reference/ece_to_salinity.md),
  [`salinity_to_ece()`](https://mwaongo.github.io/aquacropr/reference/salinity_to_ece.md).
