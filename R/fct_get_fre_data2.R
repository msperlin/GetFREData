#' Downloads and reads FRE datasets
#'
#' The FRE (formularios de referencia) is the reporting system of companies
#' traded at B3. This function will access the CVM ftp and parse all available files according to user
#' choices such as dates and companies.
#'
#' @param first_year First year of selected data
#' @param last_year Last year of selected data
#' @param cache_folder Path of cache folder to keep cache and zip files (default = 'gfred_cache')
#'
#' @return A list of tibbles, where each element is a different table from the FRE system
#' @export
#'
#' @examples
#' \dontrun{
#' l_fre <- get_fre_data2(cache_folder = fs::file_temp("fre-cache"))
#' }
get_fre_data2 <- function(first_year = lubridate::year(lubridate::today()) - 1,
                          last_year = lubridate::year(lubridate::today()),
                          cache_folder = get_default_cache_dir()) {

  if (first_year < 2010) {
    cli::cli_abort('CVM FRE data is only available after 2010...')
  }

  vec_year <- first_year:last_year

  l_raw <- purrr::map(vec_year, get_single_year,
                      cache_folder)

  all_names <- purrr::map(l_raw, get_names)

  all_names <-   do.call(c, all_names) |>
    unique()

  message("")
  cli::cli_alert_info('merging dataframes')

  l_out <- list()
  for (i_name in all_names) {
    df <- l_raw |>
      purrr::map(i_name) |>
      purrr::list_rbind()

    l_out[[i_name]] <- df
  }

  cli::cli_alert_info('fixing dataframes')

  l_out2 <- purrr::map(l_out, fix_df)

  message("")
  cli::cli_alert_success('got list of dataframes with {length(l_out2)} elements')

  return(l_out2)

}

get_names <- function(l_in) {
  all_names <- names(l_in)

  return(all_names)
}

get_single_year <- function(year, cache_folder) {

  cli::cli_h1('fetching FRE data for year {year}')

  dest_file <- fs::path(
    cache_folder,
    'raw-zip-files',
    glue::glue('cvm-ftp-zip_{year}.zip')
  )

  fs::dir_create(dirname(dest_file), recurse = TRUE)

  if (fs::file_exists(dest_file)) {
    cli::cli_alert_success('\tfile {basename(dest_file)} found at cache')

  } else {
    #cli::cli_h2('fetching FRE data for {year}')

    # fix check msg
    year_files <- NULL

    ftp_files <- get_contents_ftp(get_base_url()) |>
      dplyr::filter(year_files == year)

    if (nrow(ftp_files) == 0) {
      cli::cli_abort("cant find ftp data for year {year}")
    }

    this_link <- ftp_files$full_links[1]

    temp_zip <- fs::file_temp(ext = 'zip')

    cli::cli_alert_info("downloading {basename(this_link)}")
    my_download_file(this_link, temp_zip, be_quiet = TRUE)

    fs::file_copy(temp_zip, dest_file)
  }

  cli::cli_alert_info("\tparsing files")

  temp_unzip_dir <- fs::file_temp('unzip-fre')
  fs::dir_create(temp_unzip_dir)

  utils::unzip(dest_file, exdir = temp_unzip_dir)

  available_files <- fs::dir_ls(temp_unzip_dir)

  l_csv <- purrr::map(available_files, read_single_csv)

  all_names <- names(l_csv)

  fixed_names <- purrr::map_chr(all_names, fix_single_name)

  cli::cli_alert_success(
    glue::glue("\tdone, got list with {length(l_csv)} elements")
  )

  names(l_csv) <- fixed_names


  return(l_csv)

}

fix_single_name <- function(name_in) {
  f_basename <- basename(name_in)

  id_str <- f_basename |>
    stringr::str_remove("fre_")

  year_str <- readr::parse_number(id_str)

  id_str <- f_basename |>
    stringr::str_remove(glue::glue("_{year_str}.csv"))

  return(id_str)
}

read_single_csv <- function(f_csv) {

  f_basename <- basename(f_csv)

  #cli::cli_alert_info('parsing file {f_basename}')

  id_str <- f_basename |>
    stringr::str_remove("fre_")

  year_str <- readr::parse_number(id_str)

  id_str <- f_basename |>
    stringr::str_remove(glue::glue("_{year_str}.csv"))

  my_locale <- readr::locale(decimal_mark = ',', encoding = "Latin1")
  df <- readr::read_csv2(f_csv, locale = my_locale,
                         col_types = readr::cols(.default = readr::col_character()),
                         show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(source_file = basename(f_csv))

  #browser()

  troubled_names <- c("cpf_auditor", "cnpj_auditor", "valor",
                      "participacao_emissor")

  for (i_trouble in troubled_names) {
    if (any(names(df) == i_trouble)) {
      df[[i_trouble]] <- as.character(df[[i_trouble]])
    }
  }


  l_out <- list()
  l_out[[id_str]] <- df

  return(df)
}

get_base_url <- function() {
  my_url <- 'http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/FRE/DADOS/'
  return(my_url)
}

fix_df <- function(df_in) {

  str_num_cols <- c("quantidade_", "valor_", "id_", 'versao',
                    'preco_', 'numero_', 'percentual_')

  all_names <- names(df_in)

  all_numbers <- stringr::str_subset(all_names, paste0(str_num_cols, collapse = "|"))

  for (i_col in all_numbers) {
    df_in[[i_col]] <- as.numeric(df_in[[i_col]] )
  }

  str_dates_cols <- c("dt_", "data_")
  all_dates <- stringr::str_subset(all_names, paste0(str_dates_cols, collapse = "|"))

  for (i_col in all_dates) {
    df_in[[i_col]] <- as.Date(df_in[[i_col]] )
  }

  return(df_in)

}
