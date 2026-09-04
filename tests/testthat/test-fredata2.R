cache_folder <- tempdir()

test_that('fetch fre data using get_fre_data2', {

  skip_on_cran()
  skip_if_offline()

  cache_folder <- fs::file_temp('fre-cache-temp')

  l_fre <- get_fre_data2(first_year = 2020,
                         last_year = 2020,
                         cache_folder = cache_folder,
                         verbose = FALSE)

  expect_true(
    (is.list(l_fre)) & (length(l_fre) > 0)
  )

  # Check that ALL tables have cd_cvm
  expect_true(all(sapply(l_fre, function(df) "cd_cvm" %in% names(df))))

  # Test filtering by CVM code
  l_fre_filtered <- get_fre_data2(first_year = 2020,
                                  last_year = 2020,
                                  companies_cvm_codes = c(9512),
                                  cache_folder = cache_folder,
                                  verbose = FALSE)

  expect_true(is.list(l_fre_filtered))

  # Check that ALL tables have cd_cvm in filtered output
  expect_true(all(sapply(l_fre_filtered, function(df) "cd_cvm" %in% names(df))))

  # Check that all non-empty tables only contain data for CVM 9512
  all_9512 <- all(sapply(l_fre_filtered, function(df) {
    if (nrow(df) == 0) return(TRUE)
    all(as.numeric(df$cd_cvm) == 9512)
  }))
  expect_true(all_9512)

})

test_that('fetch fre data using get_fre_data2 with cache', {

  skip_on_cran()
  skip_if_offline()

  l_fre <- get_fre_data2(first_year = 2020,
                         last_year = 2020,
                         cache_folder = cache_folder,
                         verbose = FALSE)

  expect_true(
    (is.list(l_fre)) & (length(l_fre) > 0)
  )

  # Check that ALL tables from cached RDS have cd_cvm
  expect_true(all(sapply(l_fre, function(df) "cd_cvm" %in% names(df))))

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
