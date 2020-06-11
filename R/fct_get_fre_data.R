#' Downloads and reads DFP datasets
#'
#' The DFP (demonstrativos financeiros padronizados) is the annual reporting system of companies
#' traded at B3. This function will access the CVM ftp and parse all available files according to user
#' choices
#'
#' @param companies_cvm_codes Numeric CVM code  of companies. IF set to NULL (default), will return data for all available companies.
#' @param first_year First year of selected data
#' @param last_year Last year of selected data
#' @param type_docs Type of financial documents. E.g. c('DRE', 'BPA'). Definitions: '*' = fetch all docs,  'BPA' = Assets (ativos),
#'                 'BPP' = Liabilities (passivo),
#'                 'DRE' = income statement (demonstrativo de resultados),
#'                 'DFC_MD' = cash flow by direct method (fluxo de caixa pelo metodo direto),
#'                 'DFC_MI' = cash flow by indirect method (fluxo de caixa pelo metodo indireto),
#'                 'DMPL' = statement of changes in equity (mutacoes do patrimonio liquido),
#'                 'DVA' = value added report (desmonstrativo de valor agregado)
#' @param type_format Type of format of document (con = consolidated, ind = individual). Default = c('con', 'ind')
#' @param clean_data Clean data or return raw data? See read_dfp|itr_csv() for details
#' @param use_memoise Use memoise caching? If no (default), the function will read all .csv files. If yes, will use package
#'                    memoise for caching results (execution speed increases significantly)
#' @param cache_folder Path of cache folder to keep memoise and zip files
#'
#' @return A list of tibbles, separated by column GRUPO_DFP
#' @export
#'
#' @examples
#' \dontrun{
#' df_dfp <- get_dfp_data()
#' }
get_fre_data <- function(companies_cvm_codes = NULL,
                         first_year = 2010,
                         last_year = lubridate::year(Sys.Date()),
                         fre_to_read = 'last',
                         use_memoise = FALSE,
                         cache_folder = 'gfred_cache') {

  if ((!is.null(companies_cvm_codes))&(!is.numeric(companies_cvm_codes))) {
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

  message('Found ', nrow(df_fre_links_to_read),
          ' FRE docs to read')

  #browser()

  l_fre <- purrr::map(.x = split(df_fre_full_links, df_fre_full_links$LINK_DOC),
                      .f = download_read_fre_zip_files,
                      cache_folder = cache_folder)





}

download_read_fre_zip_files <- function(df_file_in,
                                        cache_folder) {

  message('Downloading fre file: ',
          'id cvm = ', df_file_in$CD_CVM,
          ' | ref date = ', df_file_in$DT_REFER,
          ' | version ', df_file_in$VERSAO,
          appendLF = FALSE)

  dest_file <- file.path(cache_folder, 'fre_zip_files',
                         paste0('FRE_',
                         df_file_in$CD_CVM, '_',
                         (df_file_in$DT_REFER), '_',
                         'ver', df_file_in$VERSAO, '.zip'))

  my_download_file(dl_link = df_file_in$LINK_DOC,
                   dest_file = dest_file, be_quiet = TRUE)


  l_out <- read_single_fre_zip_file(dest_file, folder.to.unzip = tempdir())

  browser()

}
