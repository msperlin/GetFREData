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
