#' Returns a up to date table with information about B3 companies
#'
#' Imports a dataset from cvm <http://sistemas.cvm.gov.br/cadastro/SPW_CIA_ABERTA.ZIP>, containing
#' up to date information about companies, active or not.
#'
#' @inheritParams get_fre_data
#'
#' @return A dataframe with several information about B3 companies
#' @export
#'
#' @examples
#'
#' \dontrun{ # keep cran check fast
#' df_info <- get_info_companies()
#' str(df_info)
#' }
get_info_companies <- function(cache_folder = 'gfred_cache') {

  # use function from GetDFPData2
  df_cvm <- GetDFPData2::get_info_companies(cache_folder = cache_folder)

  return(df_cvm)

}
