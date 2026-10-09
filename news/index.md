# Changelog

## aquacropr (development version)

- [`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md)
  and
  [`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md)
  now refresh `LIST/ListProjects.txt` after writing. The file is rebuilt
  from the `.PRM` files actually present in `path`, one name per line,
  which is the list AquaCrop standalone reads to decide which projects
  to run. Set `update_list = FALSE` to skip it.
  [`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md)
  writes the list once, after all sites, rather than once per site.

------------------------------------------------------------------------

## aquacropr 0.3.0

### Breaking changes

- **`%>%` is no longer re-exported.** Package code uses the native pipe
  `|>` throughout, so `R/utils-pipe.R` and the `magrittr` dependency are
  gone. Code that relied on
  [`library(aquacropr)`](https://github.com/mwaongo/aquacropr) providing
  `%>%` must now load it from magrittr or dplyr directly, or switch to
  `|>`.
- **`Depends: R (>= 4.1.0)`**, raised from `R (>= 3.5)`. Package code
  now uses the native pipe `|>` throughout (51 call sites converted from
  `%>%`), which requires R 4.1. The shipped `regionalsim` vignette
  already used `|>`, so the previous `R (>= 3.5)` declaration was not
  accurate in any case.

### Climate writers

- [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md)
  is substantially faster. Writing the 427-cell regional grid (156,282
  rows) went from **25.2 s to 4.4 s** (~5.7x). Three changes account for
  it: each climate file is now written through a single connection
  instead of a
  [`readr::write_file()`](https://readr.tidyverse.org/reference/read_file.html)
  header pass followed by a
  [`readr::write_lines()`](https://readr.tidyverse.org/reference/read_lines.html)
  append pass; `readr`’s per-call overhead is replaced with base
  connections; and the repeated OS/path lookups
  ([`Sys.info()`](https://rdrr.io/r/base/Sys.info.html) via
  [`get_os()`](https://mwaongo.github.io/aquacropr/reference/get_os.md),
  [`fs::dir_exists()`](https://fs.r-lib.org/reference/file_access.html))
  are cached or moved to their base equivalents. Output is
  byte-identical.
- Fixed
  [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md)
  erroring on a **tibble containing `NA`**.
  [`write_fwf()`](https://mwaongo.github.io/aquacropr/reference/write_fwf.md)
  replaced missing values with `x[is.na(x)] <- replace_na`, which a
  tibble rejects as an incompatible assignment. This affected the
  bundled `weather` data and anything coming from
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).
- Fixed `eol` being ignored for data rows. The header honoured the
  requested `eol` but
  [`write_fwf()`](https://mwaongo.github.io/aquacropr/reference/write_fwf.md)
  re-detected it from the host OS, so `eol = "windows"` on Linux or
  macOS produced a file with CRLF header lines and LF data lines.
- [`write_fwf()`](https://mwaongo.github.io/aquacropr/reference/write_fwf.md)
  now warns when a formatted value is wider than its column. Previously
  such values silently ran into the neighbouring column, emitting
  e.g. `14.61234567890129.487654321098` with no separator between `tmin`
  and `tmax` – a corrupt file that AquaCrop reads without complaint.
- `.get_eol()` accepts `eol` case-insensitively, as documented.
- [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md)
  gains `quiet`. A single call reports six lines, which is ~2,500 lines
  of output over the 427-cell regional grid; pass `quiet = TRUE` to
  silence it. Default `FALSE`, so existing output is unchanged.
- [`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
  now defaults to `eol = NULL` (auto-detect), matching every other
  writer – it was the only one hard-coded to `"windows"`. Behaviour
  change for direct
  [`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
  calls on Linux and macOS, which now get LF rather than CRLF; a CRLF
  `.CLI` on a unix host can leave a trailing `\r` on the climate
  filenames it lists.
  [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md)
  is unaffected, as it already passed `eol` through explicitly.

### Consistency

- `eol` now defaults to `NULL` (auto-detect) in every writer.
  [`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md)
  and
  [`write_prm()`](https://mwaongo.github.io/aquacropr/reference/write_prm.md)
  were the last two hard-coded to `"windows"`;
  [`write_cro()`](https://mwaongo.github.io/aquacropr/reference/write_cro.md)’s
  documentation already claimed `NULL`, so its code and docs now agree.
  Files written with the default on Linux/macOS change from CRLF to LF;
  passing `eol = "windows"` explicitly is unchanged.
- [`write_cli()`](https://mwaongo.github.io/aquacropr/reference/write_cli.md)
  defaults to `path = "CLIMATE/"`, matching
  [`write_plu()`](https://mwaongo.github.io/aquacropr/reference/write_plu.md),
  [`write_eto()`](https://mwaongo.github.io/aquacropr/reference/write_eto.md),
  [`write_tnx()`](https://mwaongo.github.io/aquacropr/reference/write_tnx.md)
  and
  [`write_climate()`](https://mwaongo.github.io/aquacropr/reference/write_climate.md).
  It was the only climate writer defaulting to `"weather/"`.
- Package-level environments are now declared together in `globals.R`;
  `zzz.R` holds only the attach hook.
- `CONTRIBUTING.md` updated for the native pipe, the R 4.1 minimum, the
  `.data`/string column-reference rules, the repo’s mixed line endings,
  and the new test suite.

### Internals

- Removed
  [`utils::globalVariables()`](https://rdrr.io/r/utils/globalVariables.html).
  The 130-name list was roughly 85% stale (every `var_02`..`var_84`
  entry, plus `x`, `path`, `%>%`, `.`, `value`, `fmt`, `width`,
  `thickness_vec`), and it masked real lookup errors rather than fixing
  them. The 23 names actually needed are now explicit: data-masking
  references use the
  [`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html)
  pronoun, tidyselect references use strings, and the four bundled
  datasets are fetched through a new internal `.pkg_data()` helper
  instead of `utils::data(envir = environment())`. `R CMD check` reports
  no visible-binding notes.

### Testing

- Added `tests/testthat/` with regression tests covering the above.

### Other

- Renamed `write_irrig_batch()` to
  [`write_irr_batch()`](https://mwaongo.github.io/aquacropr/reference/write_irr_batch.md),
  matching the `write_<type>_batch()` pattern used by every other batch
  writer
  ([`write_sol_batch()`](https://mwaongo.github.io/aquacropr/reference/write_sol_batch.md),
  [`write_man_batch()`](https://mwaongo.github.io/aquacropr/reference/write_man_batch.md),
  [`write_cal_batch()`](https://mwaongo.github.io/aquacropr/reference/write_cal_batch.md),
  [`write_prm_batch()`](https://mwaongo.github.io/aquacropr/reference/write_prm_batch.md))
  and the single-site
  [`write_irr()`](https://mwaongo.github.io/aquacropr/reference/write_irr.md).
  Breaking change: update any code calling `write_irrig_batch()`.
- [`install_binaries()`](https://mwaongo.github.io/aquacropr/reference/install_binaries.md):
  `compiler` defaults to `NULL` (was `"gfortran"`), matching
  [`install_source()`](https://mwaongo.github.io/aquacropr/reference/install_source.md).
- [`install_binaries()`](https://mwaongo.github.io/aquacropr/reference/install_binaries.md):
  safer archive extraction, download validation, and post-install
  checks.
- [`install_binaries()`](https://mwaongo.github.io/aquacropr/reference/install_binaries.md):
  stricter `path`/`force`/version argument validation.
- [`run_aquacrop()`](https://mwaongo.github.io/aquacropr/reference/run_aquacrop.md):
  clearer error messages for a missing executable.
- Fixed `|>` and `\(...)` lambda usage inconsistent with
  `Depends: R (>= 3.5)`; switched to `%>%`/`function()`.
- Removed unused `gh` dependency and dead code in `utils-readers.R`.
- Deduplicated OS/executable-name detection via
  [`get_os()`](https://mwaongo.github.io/aquacropr/reference/get_os.md)
  and `.aquacrop_exe_name()`.
- Fixed non-ASCII characters in
  [`run_aquacrop()`](https://mwaongo.github.io/aquacropr/reference/run_aquacrop.md)
  and excluded `.claude/` from the build (`R CMD check` now passes
  clean).

------------------------------------------------------------------------

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
  / `write_irrig_batch()` — write irrigation schedule (`.IRR`) files.
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
