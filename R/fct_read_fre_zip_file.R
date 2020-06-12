#' Reads a single FRE zip file downloaded from Bovespa
#'
#' @param my_zip_file Full path to zip file
#' @param df_file_in Dataframe with information about file
#' @param folder_to_unzip Folder to unzip files (default = tempdir())
#' @inheritParams get_fre_data
#'
#' @return A list with several dataframes containing financial statements
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # no examples fit here
#' }
#'
read_single_fre_zip_file <- function(my_zip_file,
                                     df_file_in,
                                     cache_folder = 'gfred_cache',
                                     folder_to_unzip = tempdir()) {

  # sanity check
  if (tools::file_ext(my_zip_file) != 'zip') {
    stop(paste('File', my_zip_file, ' is not a zip file.') )
  }

  if (!file.exists(my_zip_file)) {
    stop(paste('File', my_zip_file, ' does not exists.') )
  }

  if (file.size(my_zip_file) == 0){
    stop(paste('File', my_zip_file, ' has size 0!') )
  }

  if (length(my_zip_file) != 1){
    stop('This function only works for a single zip file... check your inputs')
  }

  if (!dir.exists(folder_to_unzip)) {
    cat(paste('Folder', folder_to_unzip, 'does not exist. Creating it.'))
    dir.create(folder_to_unzip)
  }

  my_dir <- file.path(cache_folder, 'fre_parsed_cache_files')
  if (!dir.exists(my_dir)) dir.create(my_dir, recursive = TRUE)

  f_cache <- file.path(my_dir,
                       paste0('fre_cache_',
                              'cvm', df_file_in$CD_CVM, '_',
                              stringr::str_sub(df_file_in$DENOM_CIA, 1, 10), '_',
                              lubridate::year(df_file_in$DT_REFER), '_',
                              'id', df_file_in$ID_DOC, '_',
                              'ver', df_file_in$VERSAO, '.rds')
  )

  if (file.exists(f_cache)&
      (file.size(f_cache)> 0)) {

    message(' | found parsed cache file')
    my_l <- readr::read_rds(f_cache)
    return(my_l)
  }

  my_basename <- tools::file_path_sans_ext(basename(my_zip_file))
  rnd_folder_name <- file.path(folder_to_unzip, paste0('DIR-',my_basename))

  if (!dir.exists(rnd_folder_name)) dir.create(rnd_folder_name)

  utils::unzip(my_zip_file,
               exdir = rnd_folder_name, junkpaths = TRUE)

  # list files and check it
  my_files <- list.files(rnd_folder_name)

  if (length(my_files) == 0) {

    file.remove(my_zip_file)
    stop(paste0('Zipped file contains 0 files. ',
                'This is likelly a problem with the downloaded file. ',
                'Try running the code again as the corrupted zip file was deleted and will be downloaded again.',
                '\n\nIf the problem persists, my suggestions is to remove the time period with problem.') )
  }


  my_l <- gdfpd.read.zip.file.type.fre(rnd_folder_name, folder_to_unzip)

  # fix for CRAN check
  CNPJ_CIA <- DT_REFER <- DENOM_CIA <- CD_CVM <- NULL
  DT_RECEB <- VERSAO <- ID_DOC <- NULL

  # save info on companies
  df_info_file <- df_file_in %>%
    dplyr::select(CNPJ_CIA, DENOM_CIA, DT_REFER, CD_CVM,
                  ID_DOC, VERSAO)


  my_l <- purrr::map(.x = my_l, .f = my_fix_cols,
                     df_info_file = df_info_file)


  # save cache
  message(' | reading and saving cache')
  readr::write_rds(my_l, f_cache)

  return(my_l)
}

#' Reads folder for zip file post 2011 (internal)
#'
#' @param folder_to_unzip Folder to unzip files
#' @param rnd_folder_name Folder name where unzipped files are available
#'
#' @return A list with financial statements
#'
#' @examples
#' # no example (this functions not used directly)
gdfpd.read.zip.file.type.fre <- function(rnd_folder_name, folder_to_unzip = tempdir()) {

  zipped_file <- file.path(rnd_folder_name, list.files(rnd_folder_name, pattern = '*.fre')[1])

  utils::unzip(zipped_file, exdir = rnd_folder_name)

  company_reg_file <- file.path(rnd_folder_name,'ControleAcionario.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))


  # get stock holders composition

  df_stockholders <- do.call(what = rbind, lapply(xml_data, xml.fct.stockholder))
  rownames(df_stockholders) <- NULL

  # stock composition
  company_reg_file <- file.path(rnd_folder_name,'CapitalSocial.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  if (length(xml_data) ==0 ) {
    df_capital <- data.frame(stock.type = NA,
                             stock.class = NA,
                             qtd.issued = NA)
  } else {
    # find integralized capital information
    idx <- sapply(xml_data, function(x) x$CodigoTipoCapital) == '3'

    # get data
    if ( !(any(idx)) ) { # fix for non existing integralized stocks (corner case)
      df_capital <- data.frame(stock.type = c('ON', 'PN'),
                               stock.class = c('0', '0'),
                               qtd.issued = c(0, 0), stringsAsFactors = FALSE)
    } else {

      effective_capital <- xml_data[[max(which(idx))]]

      if ( is.null(effective_capital$CapitaisSocialPorClasse)) {

        df_capital <- data.frame(stock.type = c('ON', 'PN'),
                                 stock.class = c('0', '0'),
                                 qtd.issued = c(as.numeric(effective_capital$QuantidadeAcoesOrdinarias),
                                                as.numeric(effective_capital$QuantidadeAcoesPreferenciais)), stringsAsFactors = FALSE )
      } else {

        my.fct <- function(x) {
          my.df <- data.frame(stock.type = 'PN',
                              stock.class = x$CodigoClasseAprf,
                              qtd.issued = as.numeric(x$QuantidadeAcoes))
          return(my.df)
        }

        temp.df <- do.call(what = rbind, lapply(effective_capital$CapitaisSocialPorClasse,FUN = my.fct))
        rownames(temp.df) <- NULL

        df_capital <- rbind(data.frame(stock.type = c('ON'),
                                       stock.class = c('0'),
                                       qtd.issued = as.numeric(effective_capital$QuantidadeAcoesOrdinarias),
                                       stringsAsFactors = FALSE ),
                            temp.df)

      }

    }

  }

  # market value of company
  company_reg_file <- file.path(rnd_folder_name,'CotacaoValoresMobiliarios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  if (is.null(xml_data)) {

    df_stock_values <- data.frame(stock.type = NA,
                                  stock.class = NA,
                                  max.price = NA,
                                  min.price = NA,
                                  avg.price = NA,
                                  qtd.issued = NA,
                                  flag.missing.avg.price = NA)

    df_mkt_value <- data.frame(mkt.avg.value = NA,
                               mkt.min.value = NA,
                               mkt.max.value = NA)

  } else {

    #   find data for current ref.date
    temp.dates <- as.Date(sapply(xml_data, function(x) stringr::str_sub(x$DataFimTrimestre,1,10) ))
    ref.date <- max(temp.dates)

    xml_data <- xml_data[ref.date == temp.dates]

    df_stock_values <- do.call(what = rbind, lapply(xml_data,FUN = xml.fct.stock.values))
    rownames(df_stock_values) <- NULL

    df_stock_values <- merge(df_stock_values, df_capital, by = c('stock.class', 'stock.type'), all = TRUE )

    # fix for 0 qtd.issued
    idx <- df_stock_values$qtd.issued ==0
    df_stock_values$avg.price[idx] <- 0
    df_stock_values$max.price[idx] <- 0
    df_stock_values$min.price[idx] <- 0

  }

  # company value
  df_mkt_value <- data.frame(mkt.avg.value = sum(df_stock_values$qtd.issued*df_stock_values$avg.price),
                             mkt.min.value = sum(df_stock_values$qtd.issued*df_stock_values$min.price),
                             mkt.max.value = sum(df_stock_values$qtd.issued*df_stock_values$max.price) )

  # get: increases of capital
  company_reg_file <- file.path(rnd_folder_name,'AumentoCapitalEmissor.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_capital_increases <- do.call(what = rbind, lapply(xml_data, xml.fct.capital))
  rownames(df_capital_increases) <- NULL

  # get: capital reduction
  company_reg_file <- file.path(rnd_folder_name,'ReducaoCapitalEmissor.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file))

  df_capital_reductions <- do.call(what = rbind,
                                  lapply(xml_data,
                                         xml.fct.capital.reduction))
  rownames(df_capital_reductions) <- NULL

  # get: compensation details
  company_reg_file <- file.path(rnd_folder_name,'RemuneracaoReconhecidaAdministradores.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  idx.periods <- as.numeric(sapply(xml_data,
                                   function(x) return(x$DatasExerciciosSociais$NumeroExercicioSocial)) )

  # index 2  seems to be the magic number for the remuneration data for related year
  # it seems this data gets updated over time
  xml_data <- xml_data[which(idx.periods == 2)]

  df_compensation <- do.call(what = rbind,
                             lapply(xml_data,
                                    xml.fct.compensation))
  rownames(df_compensation) <- NULL

  # get compensation summary

  company_reg_file <- file.path(rnd_folder_name,'RemuneracaoOrgaos.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  idx.periods <- as.numeric(sapply(xml_data,
                        function(x) return(x$DatasExerciciosSociais$NumeroExercicioSocial)) )

  # here I use the minimum index.. seems to be 2 again..
  xml_data <- xml_data[which(idx.periods == min(idx.periods))]
  df_compensation_summary <- do.call(what = rbind, lapply(xml_data, xml.fct.compensation.summary))
  rownames(df_compensation_summary) <- NULL

  # get: transactions related parts

  company_reg_file <- file.path(rnd_folder_name,'TransacaoComParteRelacionada.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  dF_transactions_related <- do.call(what = rbind, lapply(xml_data, xml.fct.transactions.related))
  rownames(dF_transactions_related) <- NULL

  # get: splits, inplits and other events

  company_reg_file <- file.path(rnd_folder_name,'DesdobramentoGrupamentoBonificacao.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_other_events <- do.call(what = rbind, lapply(xml_data, xml.fct.splits.inplits))
  rownames(df_other_events) <- NULL

  # get: repurchases
  company_reg_file <- file.path(rnd_folder_name,'PlanoRecompraAcoes.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_stock_repurchases <- do.call(what = rbind, lapply(xml_data, xml.fct.repurchases))
  rownames(df_stock_repurchases) <- NULL

  # get: debt
  company_reg_file <- file.path(rnd_folder_name,'Dividas.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  number.time.period <- as.numeric(sapply(xml_data, function(y) return(y$ExercicioSocial$NumeroExercicioSocial)))
  max.exercicio <- max(number.time.period)

  idx <- number.time.period == max.exercicio

  xml_data <- xml_data[idx]

  df_debt_composition <- do.call(what = rbind, lapply(xml_data, xml.fct.debt))
  rownames(df_debt_composition) <- NULL

  # get: composition management and fiscal council
  company_reg_file <- file.path(rnd_folder_name,'AdministradorMembroConselhoFiscalNegocios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_board_composition <- do.call(what = rbind, lapply(xml_data, xml.fct.board.composition))
  rownames(df_board_composition) <- NULL

  # get: composition commitees
  company_reg_file <- file.path(rnd_folder_name,'MembroComiteNegocios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_commitee_composition <- do.call(what = rbind, lapply(xml_data, xml.fct.committee.composition))
  rownames(df_commitee_composition) <- NULL

  # get: family relations

  company_reg_file <- file.path(rnd_folder_name,'RelacaoConjugalNegocios.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_family_relations <- do.call(what = rbind, lapply(xml_data, xml.fct.family.relations))
  rownames(df_family_relations) <- NULL

  # get: family relations in related companies

  company_reg_file <- file.path(rnd_folder_name,'HistoricoRelacaoSubordinacaoAdministradorEmissor.xml')
  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_family_related_companies <- do.call(what = rbind, lapply(xml_data, xml.fct.family.related.parts))
  rownames(df_family_related_companies) <- NULL


  # get: auditing information
  company.reg.file.1 <- file.path(rnd_folder_name, 'AuditorFormularioReferencia_v2.xml')
  company.reg.file.2 <- file.path(rnd_folder_name, 'AuditorFormularioReferencia.xml')
  my.files <-  c(company.reg.file.1, company.reg.file.2)
  company_reg_file <-my.files[file.exists(my.files)]

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_auditing <- do.call(what = dplyr::bind_rows, lapply(xml_data, xml.fct.auditing))
  rownames(df_auditing) <- NULL

  # get: responsible for documents

  company_reg_file <- file.path(rnd_folder_name, 'ResponsavelConteudoFormularioNegociosNovo.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_responsible_docs <- do.call(what = dplyr::bind_rows, lapply(xml_data, xml.fct.responsible ))
  rownames(df_responsible_docs) <- NULL

  # get table 18.1 Valor mobiliarios
  company_reg_file <- file.path(rnd_folder_name, 'DireitoClasseEspecieAcao.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))

  df_stocks_details <- do.call(what = dplyr::bind_rows, lapply(xml_data, xml.fct.stocks.details ))
  rownames(df_stocks_details) <- NULL

  # get table 3.5 - dividends and payout
  company_reg_file <- file.path(rnd_folder_name, 'InformacoesFinanceirasSelecionadas.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file, encoding = 'UTF-8'))
  xml_data <- xml_data[1]

  if (is.null(xml_data)) {
    df_dividends_details <- data.frame(net.profit = NA,
                                       distributed.dividend = NA,
                                       retained.profit = NA,
                                       payout = NA,
                                       div.yeild.on.equity = NA,
                                       stringsAsFactors = FALSE )
  } else {
    df_dividends_details <- do.call(what = dplyr::bind_rows, lapply(xml_data, xml.fct.div.details ))
  }

  rownames(df_dividends_details) <- NULL

  # get: intangible/patents  (table 9.1.b)
  company_reg_file <- file.path(rnd_folder_name,
                                'PatentesMarcasFranquias.xml')

  xml_data <- XML::xmlToList(XML::xmlParse(company_reg_file,
                                           encoding = 'UTF-8'))
  df_intangible_assets <- do.call(what = dplyr::bind_rows,
                                lapply(xml_data, xml.fct.intangible.details ))
  rownames(df_intangible_assets) <- NULL

  # save output

  my.l <- list(df_stockholders = df_stockholders,
               df_capital = df_capital,
               df_stock_values = df_stock_values,
               df_mkt_value = df_mkt_value,
               df_increase_capital = df_capital_increases,
               df_capital_reduction = df_capital_reductions,
               df_compensation = df_compensation,
               df_compensation_summary = df_compensation_summary,
               df_transactions_related = dF_transactions_related,
               df_other_events = df_other_events,
               df_stock_repurchases = df_stock_repurchases,
               df_debt_composition = df_debt_composition,
               df_board_composition = df_board_composition,
               df_committee_composition = df_commitee_composition,
               df_family_relations = df_family_relations,
               df_family_related_companies = df_family_related_companies,
               df_auditing = df_auditing,
               df_responsible_docs = df_responsible_docs,
               df_stocks_details = df_stocks_details,
               df_dividends_details  = df_dividends_details,
               df_intangible_details = df_intangible_assets)

  return(my.l)
}

#' Manipulates list from FRE zip file
#'
#' This function will add several columns to each table in fre list.
#'
#' @param df_in Entry dataframe
#' @param df_info_file Informations about file
#'
#' @return An organized dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' # no example here
#' }
my_fix_cols <- function(df_in, df_info_file) {

  if (is.null(df_in)) {
    #df.in <- data.frame(flag.NODATA = TRUE)
    return(data.frame())
  }

  if (!is.data.frame(df_in)) return(df_in)

  if (nrow(df_in) ==0) return(data.frame())

  df_in <- dplyr::bind_cols(df_info_file, df_in)

  # force Encoding
  my_encoding_fct <- function(col.in) {
    if (is.factor(col.in)) {
      col.in <- as.character(col.in)
    }

    if (is.numeric(col.in)) return(col.in)

    if (is.character(col.in)) {
      Encoding(col.in) <- 'UTF-8'
    }

    return(col.in)
  }


  df_in <- as.data.frame(lapply(X = df_in, my_encoding_fct),
                         stringsAsFactors = FALSE)

  return(df_in)
}
