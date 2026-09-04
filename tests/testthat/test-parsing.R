test_that('fix_single_name extracts table identifier correctly', {

  expect_equal(fix_single_name("fre_cia_aberta_capital_social_2020.csv"), "capital_social")
  expect_equal(fix_single_name("/path/to/fre_cia_aberta_2022.csv"), "cia_aberta")

})

test_that('read_single_csv reads and cleans columns correctly (offline)', {

  temp_dir <- fs::file_temp('test-csv')
  fs::dir_create(temp_dir)
  temp_csv <- fs::path(temp_dir, "fre_test_table_2020.csv")

  # Create a small mock CSV file matching CVM format (Latin1 encoding, semicolon sep)
  df_mock <- data.frame(
    CD_CVM = c("9512", "18627"),
    VALOR_TOTAL = c("1.000,50", "2.500,00"),
    DT_REFER = c("2020-12-31", "2020-12-31"),
    stringsAsFactors = FALSE
  )

  readr::write_csv2(df_mock, temp_csv)

  df_read <- read_single_csv(temp_csv)

  expect_s3_class(df_read, "tbl_df")
  expect_true("cd_cvm" %in% names(df_read))
  expect_true("valor_total" %in% names(df_read))
  expect_true("source_file" %in% names(df_read))
  expect_equal(df_read$source_file[1], "fre_test_table_2020.csv")

  # Test fix_df type conversion
  df_fixed <- fix_df(df_read)
  expect_type(df_fixed$valor_total, "double")
  expect_s3_class(df_fixed$dt_refer, "Date")

})

test_that('add_cd_cvm_to_tables maps cd_cvm correctly across tables (offline)', {

  # Mock cia_aberta table with id_doc, cnpj_cia, and cd_cvm
  df_cia <- data.frame(
    cnpj_cia = c("00.000.000/0001-91", "11.111.111/0001-11"),
    dt_refer = as.Date(c("2020-01-01", "2020-01-01")),
    versao = c(1, 1),
    denom_cia = c("COMPANY A", "COMPANY B"),
    cd_cvm = c("001023", "009512"),
    id_doc = c("1001", "1002"),
    stringsAsFactors = FALSE
  )

  # Mock table with id_documento
  df_sub_id <- data.frame(
    cnpj_companhia = c("00.000.000/0001-91", "11.111.111/0001-11", "00.000.000/0001-91"),
    data_referencia = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
    versao = c(1, 1, 1),
    id_documento = c("1001", "1002", "1001"),
    valor = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  # Mock table with only cnpj_companhia (fallback)
  df_sub_cnpj <- data.frame(
    cnpj_companhia = c("11.111.111/0001-11", "00.000.000/0001-91"),
    descricao = c("desc1", "desc2"),
    stringsAsFactors = FALSE
  )

  # Mock table already having cd_cvm
  df_sub_cvm <- data.frame(
    cd_cvm = c("999999"),
    info = c("existing"),
    stringsAsFactors = FALSE
  )

  # Mock empty table with id_documento
  df_empty <- data.frame(
    id_documento = character(0),
    info = character(0),
    stringsAsFactors = FALSE
  )

  l_mock <- list(
    cia_aberta = df_cia,
    table_id = df_sub_id,
    table_cnpj = df_sub_cnpj,
    table_cvm = df_sub_cvm,
    table_empty = df_empty
  )

  l_result <- add_cd_cvm_to_tables(l_mock)

  # All tables must have cd_cvm
  expect_true(all(sapply(l_result, function(df) "cd_cvm" %in% names(df))))

  # Check mapping via id_documento
  expect_equal(l_result$table_id$cd_cvm, c("001023", "009512", "001023"))

  # Check fallback mapping via cnpj_companhia
  expect_equal(l_result$table_cnpj$cd_cvm, c("009512", "001023"))

  # Check pre-existing cd_cvm preserved
  expect_equal(l_result$table_cvm$cd_cvm, "999999")

  # Check empty table has cd_cvm column of type character and 0 rows
  expect_equal(nrow(l_result$table_empty), 0)
  expect_type(l_result$table_empty$cd_cvm, "character")

  # Check column relocation: cd_cvm is immediately after id_documento
  cols_id <- names(l_result$table_id)
  expect_equal(which(cols_id == "cd_cvm"), which(cols_id == "id_documento") + 1)

  # Check graceful handling when cia_aberta is missing
  l_no_cia <- list(table_a = df_sub_id)
  expect_equal(add_cd_cvm_to_tables(l_no_cia), l_no_cia)
})

