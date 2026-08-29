my_download_file <- function(dl_link,
                             dest_file,
                             max_dl_tries = 10,
                             be_quiet = TRUE) {

  if (file.exists(dest_file)) {

    if (!be_quiet) message(' no dl (file exists)', appendLF = FALSE)

    return(TRUE)
  }

  # create directory
  if (!dir.exists(dirname(dest_file))) dir.create(dirname(dest_file), recursive = TRUE)

  for (i_try in seq(max_dl_tries)) {

    try({
      my.OS <- tolower(Sys.info()["sysname"])
      if (my.OS %in% c('windows', 'darwin')) {

        utils::download.file(url = dl_link,
                             destfile = dest_file,
                             extra = '--no-check-certificate',
                             quiet = TRUE,
                             mode = 'wb')
      } else {
        utils::download.file(url = dl_link,
                             destfile = dest_file,
                             method = 'auto',
                             quiet = TRUE,
                             mode = 'wb')
      }

    })

    if (!file.exists(dest_file) || file.size(dest_file) < 10) {
      message(paste0('\t\tError in downloading. Attempt ', i_try, '/', max_dl_tries),
              appendLF = FALSE)
      Sys.sleep(1)
    } else {

      if (!be_quiet) message('\tSuccess', appendLF = TRUE)
      return(TRUE)
    }

  }

  cli::cli_abort(
    "Failed to download {.url {dl_link}} after {max_dl_tries} attempts."
  )

}
