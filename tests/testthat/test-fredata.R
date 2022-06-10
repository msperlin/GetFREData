cache_folder <- tempdir()

test_that('fetching info companies (no cache)', {

  skip_on_cran()
  skip_if_offline()

  my_info <- get_info_companies(cache_folder = cache_folder)

  expect_true(
    (is.data.frame(my_info)) & (nrow(my_info) > 0)
  )

})

test_that('fetching info companies (with cache)', {

  skip_on_cran()
  skip_if_offline()

  my_info <- get_info_companies(cache_folder = cache_folder)

  expect_true(
    (is.data.frame(my_info)) & (nrow(my_info) > 0)
  )

})

test_that('fetch fre data', {

  skip_on_cran()
  skip_if_offline()

  my_id <- 19615
  l_fre <- get_fre_data(companies_cvm_codes = my_id,
                        first_year = 2020, last_year = 2020,
                        cache_folder = cache_folder)

  expect_true(
    (is.list(l_fre)) & (length(l_fre) > 0)
  )

})
