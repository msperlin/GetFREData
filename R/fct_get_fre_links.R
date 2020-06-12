#' Fetches ftp contents and parse all available links
#'
#' @inheritParams get_fre_data
#'
#' @return A dataframe with links and files
#' @export
#'
#' @examples
#' \dontrun{ # keep cran fast
#' df_ftp <- get_contents_ftp('http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/FRE/DADOS/')
#' }
get_fre_links <- function(companies_cvm_codes,
                          first_year = 2010,
                          last_year = lubridate::year(Sys.Date()),
                          cache_folder) {

  message('Fetching ftp contents..', appendLF = FALSE)

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

  #message('Downloading fre file: ', basename(url_in), appendLF = FALSE)

  dest_file <- file.path(cache_folder, 'ftp_zip',
                         basename(url_in))
  my_download_file(dl_link = url_in,
                   dest_file = dest_file, be_quiet = TRUE)

  # fix for cran
  CD_CVM <- year_files <- filter <- CD_CVM <- NULL

  df_files <- readr::read_csv2(dest_file, col_types = readr::cols(),
                         locale = readr::locale(encoding = 'Latin1',
                                                decimal_mark = ',')) %>%
    dplyr::mutate(CD_CVM = readr::parse_number(CD_CVM))

  return(df_files)

}
