## Version 0.9.0 (2025-04-10)

- added new function get_fre_data2, which downloads data from CVM website (and not b3)

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
