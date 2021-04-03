select_responsible_beverage <- function() {
  # be responsible! - only drink after 18:00
  hour_now <- as.numeric(format(Sys.time(), '%H'))

  day_beverages <- c('coffee', 'mate cuiudo', 'english tea', 'terere', 'fanta uva',
                     'tap water')
  night_beverages <- c('Cuba Libre', 'Caipirinha', 'Pisco Sour',
                       'Mojito', 'Beer', 'Wine', 'Coco Loco',
                       'Cachaca Joao Barreiro')

  if (hour_now < 18) {
    my_beverage <- sample(day_beverages, 1)
  } else {
    my_beverage <- sample(night_beverages, 1)
  }

  return(my_beverage)
}


my_merge_dfs_lists <- function(l_1, l_2) {
  names_1 <- names(l_1)
  names_2 <- names(l_2)

  if (is.null(names_1)) return(l_2)

  if (is.null(names_2)) return(l_1)

  if (!all(names_1 == names_2)) {
    stop('Cant bind dataframes. Names in lists dont match!')
  }

  n_elem <- length(l_1)

  l_out <- list()
  for (i_l in seq(n_elem)) {

    l_out[[i_l]] <- dplyr::bind_rows(l_1[[i_l]], l_2[[i_l]])

  }

  names(l_out) <- names(l_2)
  return(l_out)

}
