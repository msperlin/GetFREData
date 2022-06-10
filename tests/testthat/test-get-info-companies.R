cache_folder <- tempdir()

test_info <- function(df_in) {
  expect_true(nrow(df_in) > 0)
  expect_true(ncol(df_in) > 0)
}

test_that('fetching info companies (no cache)', {

  skip_on_cran()
  skip_if_offline()

  my_info <- get_info_companies(cache_folder = cache_folder)

  test_info(my_info)

})

test_that('fetching info companies (with cache)', {

  skip_on_cran()
  skip_if_offline()

  my_info <- get_info_companies(cache_folder = cache_folder)

  test_info(my_info)

})
