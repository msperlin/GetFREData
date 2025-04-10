cache_folder <- tempdir()

test_that('fetch fre data using get_fre_data2', {

  skip_on_cran()
  skip_if_offline()

  cache_folder <- fs::file_temp('fre-cache-temp')

  l_fre <- get_fre_data2(first_year = 2020,
                         last_year = 2020,
                         cache_folder = cache_folder)

  expect_true(
    (is.list(l_fre)) & (length(l_fre) > 0)
  )

})

test_that('fetch fre data using get_fre_data2 with cache', {

  skip_on_cran()
  skip_if_offline()

  l_fre <- get_fre_data2(first_year = 2020,
                         last_year = 2020,
                         cache_folder = cache_folder)

  expect_true(
    (is.list(l_fre)) & (length(l_fre) > 0)
  )

})
