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

test_that('clean_fre_cache works correctly', {

  temp_cache <- fs::file_temp('fre-cache-test')
  fs::dir_create(temp_cache)

  fs::file_create(fs::path(temp_cache, "dummy.txt"))

  expect_true(fs::dir_exists(temp_cache))

  res <- clean_fre_cache(cache_folder = temp_cache)

  expect_true(res)
  expect_false(fs::dir_exists(temp_cache))

  res_again <- clean_fre_cache(cache_folder = temp_cache)
  expect_false(res_again)

})

