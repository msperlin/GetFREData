#' Downloads and reads FRE datasets
#'
#' The FRE (formularios de referencia) is the reporting system of companies
#' traded at B3. This function will access the CVM ftp and parse all available files according to user
#' choices such as dates and companies.
#'
#' @param first_year First year of selected data. CVM FRE data is available from 2010 onwards.
#' @param last_year Last year of selected data.
#' @param companies_cvm_codes Numeric vector of CVM company codes to filter.
#'   If \code{NULL} (default), returns data for all available companies.
#'   Use \code{\link{search_company}} to look up CVM codes.
#' @param cache_folder Path of cache folder to keep cache, raw zip files, and parsed RDS files (default = 'gfred_cache').
#' @param verbose Logical. Whether to print informative messages during execution (default = TRUE).
#'
#' @details
#' The resulting list contains tibbles corresponding to individual tables published in the FRE reports:
#' \itemize{
#'   \item \code{acionador}: Stockholders details and equity participation
#'   \item \code{capital_social}: Share capital structure and issued shares
#'   \item \code{posicao_acionaria}: Equity ownership structure
#'   \item \code{administrador_membro_conselho_fiscal}: Board and director composition
#'   \item \code{remuneracao_reconhecida_administradores}: Executive compensation details
#'   \item \code{remuneracao_orgaos}: Summary of remuneration by corporate body
#'   \item \code{auditor_formulario_referencia}: Independent audit firm details
#'   \item \code{distribuicao_dividendo}: Dividend distributions and payout metrics
#'   \item \code{dividas}: Debt composition by maturity and guarantee
#'   \item And other corporate disclosures.
#' }
#'
#' Parsed files are automatically cached as RDS objects in \code{cache_folder/parsed-rds/}
#' to accelerate subsequent queries for the same years.
#'
#' @return A list of tibbles, where each element is a different table from the FRE system.
#' @export
#'
#' @examples
#' \dontrun{
#' l_fre <- get_fre_data2(first_year = 2020, last_year = 2020, cache_folder = tempdir())
#'
#' # Filter by CVM company codes silently
#' l_fre <- get_fre_data2(companies_cvm_codes = c(9512, 18627), verbose = FALSE)
#' }
get_fre_data2 <- function(first_year = lubridate::year(lubridate::today()) - 1,
                          last_year = lubridate::year(lubridate::today()),
                          companies_cvm_codes = NULL,
                          cache_folder = get_default_cache_dir(),
                          verbose = TRUE) {

  # -- input validation --
  if (!is.numeric(first_year) || length(first_year) != 1) {
    cli::cli_abort("{.arg first_year} must be a single numeric value.")
  }
  if (!is.numeric(last_year) || length(last_year) != 1) {
    cli::cli_abort("{.arg last_year} must be a single numeric value.")
  }
  if (first_year > last_year) {
    cli::cli_abort(
      "{.arg first_year} ({first_year}) must be <= {.arg last_year} ({last_year})."
    )
  }
  if (first_year < 2010) {
    cli::cli_abort('CVM FRE data is only available from 2010 onwards.')
  }
  if (last_year > lubridate::year(lubridate::today())) {
    if (verbose) {
      cli::cli_warn("Requesting future year {last_year}; data may not be available yet.")
    }
  }
  if (!is.null(companies_cvm_codes) && !is.numeric(companies_cvm_codes)) {
    cli::cli_abort("{.arg companies_cvm_codes} must be a numeric vector of CVM codes.")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    cli::cli_abort("{.arg verbose} must be a single logical value (TRUE or FALSE).")
  }

  fs::dir_create(cache_folder, recurse = TRUE)

  vec_year <- first_year:last_year

  l_raw <- purrr::map(vec_year, get_single_year,
                      cache_folder = cache_folder,
                      verbose = verbose)

  all_names <- purrr::map(l_raw, get_names)

  all_names <- do.call(c, all_names) |>
    unique()

  if (verbose) {
    message("")
    cli::cli_alert_info('merging dataframes')
  }

  l_out <- list()
  for (i_name in all_names) {
    df <- l_raw |>
      purrr::map(i_name) |>
      purrr::list_rbind()

    l_out[[i_name]] <- df
  }

  if (verbose) {
    cli::cli_alert_info('fixing dataframes')
  }

  l_out2 <- purrr::map(l_out, fix_df)

  # -- filter by company if requested --
  if (!is.null(companies_cvm_codes)) {
    if (verbose) {
      cli::cli_alert_info('filtering by {length(companies_cvm_codes)} CVM code(s)')
    }

    filter_by_company <- function(df) {
      if ("cd_cvm" %in% names(df)) {
        idx <- as.numeric(df$cd_cvm) %in% companies_cvm_codes
        df <- df[idx, , drop = FALSE]
      }
      df
    }
    l_out2 <- purrr::map(l_out2, filter_by_company)
  }

  if (verbose) {
    message("")
    cli::cli_alert_success('got list of dataframes with {length(l_out2)} elements')
  }

  return(l_out2)

}

get_names <- function(l_in) {
  all_names <- names(l_in)

  return(all_names)
}

get_single_year <- function(year, cache_folder, verbose = TRUE) {

  # -- Check parsed RDS cache --
  parsed_cache_file <- fs::path(
    cache_folder,
    'parsed-rds',
    glue::glue('fre_parsed_{year}.rds')
  )

  if (fs::file_exists(parsed_cache_file)) {
    if (verbose) {
      cli::cli_alert_success('found parsed RDS file for year {year} in cache')
    }
    return(readr::read_rds(parsed_cache_file))
  }

  if (verbose) {
    cli::cli_h1('fetching FRE data for year {year}')
  }

  dest_file <- fs::path(
    cache_folder,
    'raw-zip-files',
    glue::glue('cvm-ftp-zip_{year}.zip')
  )

  fs::dir_create(dirname(dest_file), recurse = TRUE)

  if (fs::file_exists(dest_file)) {
    if (verbose) {
      cli::cli_alert_success('\tfile {basename(dest_file)} found at cache')
    }
  } else {
    year_files <- NULL

    ftp_files <- get_contents_ftp(get_base_url()) |>
      dplyr::filter(year_files == year)

    if (nrow(ftp_files) == 0) {
      cli::cli_abort("cant find ftp data for year {year}")
    }

    this_link <- ftp_files$full_links[1]

    temp_zip <- fs::file_temp(ext = 'zip')

    if (verbose) {
      cli::cli_alert_info("downloading {basename(this_link)}")
    }

    my_download_file(this_link, temp_zip, be_quiet = !verbose)

    fs::file_copy(temp_zip, dest_file)
  }

  if (verbose) {
    cli::cli_alert_info("\tparsing files")
  }

  temp_unzip_dir <- fs::file_temp('unzip-fre')
  fs::dir_create(temp_unzip_dir)

  utils::unzip(dest_file, exdir = temp_unzip_dir)

  available_files <- fs::dir_ls(temp_unzip_dir)

  l_csv <- purrr::map(available_files, read_single_csv)

  all_names <- names(l_csv)

  fixed_names <- purrr::map_chr(all_names, fix_single_name)

  names(l_csv) <- fixed_names

  # Save to parsed RDS cache
  fs::dir_create(dirname(parsed_cache_file), recurse = TRUE)
  readr::write_rds(l_csv, parsed_cache_file)

  if (verbose) {
    cli::cli_alert_success(
      glue::glue("\tdone, got list with {length(l_csv)} elements")
    )
  }

  return(l_csv)

}

fix_single_name <- function(name_in) {
  f_basename <- basename(name_in)

  id_str <- f_basename |>
    stringr::str_remove("^fre_cia_aberta_") |>
    stringr::str_remove("^fre_")

  year_str <- readr::parse_number(id_str)

  id_str <- id_str |>
    stringr::str_remove(glue::glue("_{year_str}\\.csv")) |>
    stringr::str_remove(glue::glue("{year_str}\\.csv"))

  if (id_str == "" || id_str == ".") {
    id_str <- "cia_aberta"
  }

  return(id_str)
}

read_single_csv <- function(f_csv) {

  f_basename <- basename(f_csv)

  id_str <- f_basename |>
    stringr::str_remove("fre_")

  year_str <- readr::parse_number(id_str)

  id_str <- f_basename |>
    stringr::str_remove(glue::glue("_{year_str}.csv"))

  my_locale <- readr::locale(decimal_mark = ',', encoding = "Latin1")
  df <- suppressWarnings(
    readr::read_csv2(f_csv, locale = my_locale,
                     col_types = readr::cols(.default = readr::col_character()),
                     show_col_types = FALSE)
  ) |>
    janitor::clean_names() |>
    dplyr::mutate(source_file = basename(f_csv))

  troubled_names <- c("cpf_auditor", "cnpj_auditor", "valor",
                      "participacao_emissor")

  for (i_trouble in troubled_names) {
    if (any(names(df) == i_trouble)) {
      df[[i_trouble]] <- as.character(df[[i_trouble]])
    }
  }

  return(df)
}

get_base_url <- function() {
  my_url <- 'https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/FRE/DADOS/'
  return(my_url)
}

fix_df <- function(df_in) {

  str_num_cols <- c("quantidade_", "valor_", "id_", 'versao',
                    'preco_', 'numero_', 'percentual_')

  all_names <- names(df_in)

  all_numbers <- stringr::str_subset(all_names, paste0(str_num_cols, collapse = "|"))

  for (i_col in all_numbers) {
    df_in[[i_col]] <- suppressWarnings(as.numeric(df_in[[i_col]]))
  }

  str_dates_cols <- c("dt_", "data_")
  all_dates <- stringr::str_subset(all_names, paste0(str_dates_cols, collapse = "|"))

  for (i_col in all_dates) {
    df_in[[i_col]] <- suppressWarnings(as.Date(df_in[[i_col]]))
  }

  return(df_in)

}
