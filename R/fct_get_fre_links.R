get_fre_links <- function(companies_cvm_codes,
                          first_year = 2010,
                          last_year = lubridate::year(Sys.Date()),
                          cache_folder) {

  message('Fetching ftp contents')

  my_url <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/FRE/DADOS/'

  # avoid cran note
  year_files <- filter <- CD_CVM <- NULL

  # check dates
  df_ftp <- get_contents_ftp(my_url) %>%
    dplyr::filter(year_files >= first_year,
                  year_files <= last_year)


  if (nrow(df_ftp) == 0 ) {
    stop('Cant find any available FRE data for given dates')
  }

    # process each file
  df_fre_files <- dplyr::bind_rows(
    purrr::map(.x = df_ftp$full_links,
                             .f = download_unzip_read_ftp_fre_files,
                             cache_folder = cache_folder)
  )


  # filter by company
  if (!is.null(companies_cvm_codes)) {
    df_fre_files <- df_fre_files %>%
      filter(CD_CVM %in% companies_cvm_codes)
  }

  if (nrow(df_fre_files) == 0 ) {
    stop('Cant find any available FRE data for given company and dates')
  }

  return(df_fre_files)



}

download_unzip_read_ftp_fre_files <- function(url_in, cache_folder) {

  message('\t* Reading ', basename(url_in))

  dest_file <- file.path(cache_folder, 'ftp_zip_raw',
                         basename(url_in))

  my_download_file(dl_link = url_in,
                   dest_file = dest_file,
                   be_quiet = TRUE)

  # extract to temp
  temp_zip_dir <- file.path(tempdir(),
                            basename(tempfile()))
  dir.create(temp_zip_dir, recursive = TRUE)

  utils::unzip(zipfile = dest_file, junkpaths = TRUE, exdir = temp_zip_dir)

  # read csv files
  f_to_read <- file.path(temp_zip_dir,
                         paste0(basename(tools::file_path_sans_ext(dest_file)),
                                '.csv'))
  # check if file exists
  if (!file.exists(f_to_read)) {
    stop('While unzipping \n', url_in, '\ncannot find ', basename(f_to_read))
  }

  # fix for cran
  CD_CVM <- year_files <- filter <- CD_CVM <- NULL

  df_files <- readr::read_csv2(f_to_read,
                               col_types = readr::cols(),
                               locale = readr::locale(encoding = 'Latin1',
                                                      decimal_mark = ',')) %>%
    dplyr::mutate(CD_CVM = readr::parse_number(CD_CVM))


  return(df_files)

}
