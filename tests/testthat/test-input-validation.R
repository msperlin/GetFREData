test_that('get_fre_data2 input validation works correctly', {

  # Invalid first_year / last_year types
  expect_error(get_fre_data2(first_year = "2020"), "first_year")
  expect_error(get_fre_data2(last_year = "2020"), "last_year")

  # first_year > last_year
  expect_error(get_fre_data2(first_year = 2021, last_year = 2020), "must be <= ")

  # first_year < 2010
  expect_error(get_fre_data2(first_year = 2005, last_year = 2010), "2010 onwards")

  # Invalid companies_cvm_codes type
  expect_error(get_fre_data2(first_year = 2020, last_year = 2020, companies_cvm_codes = "PETR4"), "numeric vector")

  # Invalid verbose type
  expect_error(get_fre_data2(first_year = 2020, last_year = 2020, verbose = "YES"), "single logical value")

})

test_that('get_fre_data deprecated function throws error', {

  expect_error(get_fre_data(), "deprecated")

})
