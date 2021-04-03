#' Downloads and reads FRE datasets
#'
#' The FRE (formularios de referencia) is the reporting system of companies
#' traded at B3. This function will access the CVM ftp and parse all available files according to user
#' choices such as dates and companies.
#'
#' @param companies_cvm_codes Numeric CVM code  of companies. IF set to NULL (default), will return data for all available companies.
#' @param first_year First year of selected data
#' @param last_year Last year of selected data
#' @param fre_to_read Whether to read 'first', 'last' or 'all' fre docs ('last' is default).
#'   Be aware that companies do release several FRE docs for a single year.
#' @param cache_folder Path of cache folder to keep cache and zip files
#'
#' @return A list of tibbles, where each element is a different table from the FRE system
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' l_fre <- get_fre_data(18627)
#' }
get_fre_data <- function(companies_cvm_codes = NULL,
                         first_year = lubridate::year(Sys.Date()) - 2,
                         last_year = lubridate::year(Sys.Date()),
                         fre_to_read = 'last',
                         cache_folder = 'gfred_cache') {

  if ((!is.null(companies_cvm_codes)) & (!is.numeric(companies_cvm_codes))) {
    stop('Input companies_cvm_codes should be numeric (e.g. ')
  }

  available_options <- c("first",
                         "last",
                         'all')

  idx <- fre_to_read %in% available_options
  if (any(!idx)) {
    stop(paste0('Cant find type type_format: ', paste0(fre_to_read[!idx], collapse = ', ')),
         '\n\n',
         'Available type_format are: ', paste0(available_options, collapse = ', '))
  }

  my_url <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/FRE/DADOS/'


  df_fre_full_links <- get_fre_links(companies_cvm_codes,
                                     first_year,
                                     last_year,
                                     cache_folder)

  # fix for CRAN check
  CNPJ_CIA <- DT_REFER <- DENOM_CIA <- CD_CVM <- DT_RECEB <- NULL

  # process which fre files to read
  if (fre_to_read == 'first') {

    df_fre_links_to_read <- df_fre_full_links %>%
      dplyr::group_by(CNPJ_CIA, DT_REFER, DENOM_CIA, CD_CVM) %>%
      dplyr::slice(which.min(DT_RECEB))

  } else if (fre_to_read == 'last') {
    df_fre_links_to_read <- df_fre_full_links %>%
      dplyr::group_by(CNPJ_CIA, DT_REFER, DENOM_CIA, CD_CVM) %>%
      dplyr::slice(which.max(DT_RECEB))
  } else if (fre_to_read == 'all') {
    df_fre_links_to_read <- df_fre_full_links
  }

  message('\nFound ', nrow(df_fre_links_to_read),
          ' FRE docs to read\n')

  # sort by company and date
  df_fre_links_to_read <- df_fre_links_to_read %>%
    dplyr::arrange(DENOM_CIA, DT_REFER)

  df_fre_links_to_read$idx = 1:nrow(df_fre_links_to_read)

  message('Starting Downloads:')
  l_fre <- purrr::map(.x = split(df_fre_links_to_read,
                                 df_fre_links_to_read$idx),
                      .f = download_read_fre_zip_files,
                      cache_folder = cache_folder)

  l_out <- l_fre[[1]]
  if (length(l_fre) != 1) {

    for (i_list in 2:length(l_fre)) {
      l_out <- my_merge_dfs_lists(l_out, l_fre[[i_list]])
    }

  }

  return(l_out)

}

# Downloads and reads FRE file from ftp
download_read_fre_zip_files <- function(df_file_in,
                                        cache_folder) {

  # fix for CRAN check
  CD_CVM <- NULL

  message('-> ',
          'Company ', df_file_in$CD_CVM,
          ' | fre file ', df_file_in$ID_DOC, ' (ver ', df_file_in$VERSAO, ')',
          ' | ', df_file_in$DT_REFER,
          appendLF = FALSE)

  dest_file <- file.path(cache_folder, 'fre_zip_files',
                         paste0('FRE_',
                         df_file_in$CD_CVM, '_',
                         (df_file_in$DT_REFER), '_',
                         'ver', df_file_in$VERSAO, '.zip'))

  my_download_file(dl_link = df_file_in$LINK_DOC,
                   dest_file = dest_file, be_quiet = TRUE)


  l_out <- read_single_fre_zip_file(my_zip_file = dest_file,
                                    df_file_in = df_file_in,
                                    cache_folder = cache_folder,
                                    folder_to_unzip = tempdir())

  return(l_out)

}
