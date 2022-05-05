test_that('info companies', {

  skip_on_cran()
  skip_if_offline()

  cache_folder <- tempdir()
  my_info <- get_info_companies(cache_folder = cache_folder)

  expect_true(
    (is.data.frame(my_info)) & (nrow(my_info) > 0)
  )

})


test_that('main fct', {

  skip_on_cran()
  skip_if_offline()

  cache_folder <- tempdir()

  my_id <- 19615
  l_fre <- get_fre_data(companies_cvm_codes = my_id,
                        first_year = 2020, last_year = 2020,
                        cache_folder = cache_folder)

  expect_true(
    (is.list(l_fre)) & (length(l_fre) > 0)
  )

})
