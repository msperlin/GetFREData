## Version 1.1.0 (2026-08-29)

### New features

- Added `companies_cvm_codes` argument to `get_fre_data2()`, allowing users to filter results by one or more CVM company codes.
- Cleaned table names in the output list by stripping the redundant `fre_cia_aberta_` prefix (e.g. `capital_social` instead of `cia_aberta_capital_social`).
- Added `verbose` logical parameter to `get_fre_data2()` to allow controlling or suppressing informative console output.
- Added parsed RDS caching in `cache_folder/parsed-rds/` so subsequent calls for previously parsed years load instantly from cached RDS files without re-unzipping or re-parsing CSVs.
- Added comprehensive input validation to `get_fre_data2()`: type and range checks for `first_year`/`last_year`, `verbose`, a warning when requesting future years, and type checking for `companies_cvm_codes`.
- Added `_pkgdown.yml` for website configuration and package-level documentation (`?GetFREData`).

### Bug fixes & usability

- Fixed a crash in the download helper where `file.size()` was called on a non-existent file after a failed download attempt.
- Download failures now raise a clear error (`cli::cli_abort()`) instead of silently returning `FALSE`, which previously led to confusing downstream errors.
- Suppressed `readr`'s CSV reading progress and speed messages (`progress = FALSE`, `suppressMessages()`) during dataset parsing.

### Breaking changes

- Removed the accidentally exported `my_fix_cols()` function from the public API. This was an internal helper and should not have been exported.

### Testing & Infrastructure

- Expanded unit test coverage with offline tests for CSV parsing helpers (`fix_single_name()`, `read_single_csv()`, `fix_df()`), input validation, and deprecation stubs.
- Updated GitHub Actions workflow to `@v4` (`actions/checkout@v4`).
- Resolved `R CMD check` NOTE regarding undefined global variable `.data` in `filter_by_company()`, reaching a clean `Status: OK` (0 ERRORS, 0 WARNINGS, 0 NOTES).

### Code cleanup

- Removed ~1,100 lines of dead code from the deprecated `get_fre_data()` XML pipeline: `fcts_xml.R`, `fct_read_fre_zip_file.R`, `fct_get_fre_links.R`, and `fct_utils.R`. The `get_fre_data()` function remains as a minimal deprecation stub pointing users to `get_fre_data2()`.
- Dropped `XML` and `magrittr` from package dependencies.
- Migrated all remaining `%>%` (magrittr pipe) usage to the native R pipe (`|>`).

## Version 1.0.1 (2026-05-20)

- Added `clean_fre_cache()` utility to allow users to clear down local raw ZIP and parsed RDS files.
- Suppressed expected coercion warnings during CSV file parsing and column format conversions in `get_fre_data2()`.
- Updated all user documentation, vignettes, and roxygen example references to use the active `get_fre_data2()` function instead of deprecated `get_fre_data()`.

## Version 0.9.0 (2025-04-10)

- added new function `get_fre_data2()`, which downloads data from CVM website (and not b3)

## Version 0.8.2 (2024-04-12)

- fixed issue with directory of ftp files with FRE links. Now, the folder is `dest_file <- file.path(cache_folder, paste0('ftp_zip_raw-', Sys.Date())`, indexed by day of query. This should facilitate updating the data.

## Version 0.8.1 (2022-06-10)

- fixed issue with ftp changing html (same problem as getdfpdata)

## Version 0.8.0 (2022-04-07)

- removed Date field in DESCRIPTION file
- Fixed bug in get_info_companies

## Version 0.7 (2021-08-26)

- Fixed issue in the aggregation of lists

## Version 0.6 (2021-04-19)

- Fixed issue with dob dates in board composition

## Version 0.5 (2021-04-03)

- First version
