my_download_file <- function(dl_link,
                             dest_file,
                             max_dl_tries = 10,
                             be_quiet = TRUE) {

  # no need for sleep in my tests
  #Sys.sleep(0.05)

  if (file.exists(dest_file)) {

    if (!be_quiet) message(' no dl (file exists)', appendLF = FALSE)

    return(TRUE)
  }

  # create directory
  if (!dir.exists(dirname(dest_file))) dir.create(dirname(dest_file), recursive = TRUE)

  for (i_try in seq(max_dl_tries)) {

    try({
      # old code. See issue 11: https://github.com/msperlin/GetDFPData/issues/11
      # utils::download.file(url = dl.link,
      #                      destfile = dest.file,
      #                      quiet = T,
      #                      mode = 'wb')

      # fix for issue 13: https://github.com/msperlin/GetDFPData/issues/13
      my.OS <- tolower(Sys.info()["sysname"])
      if (my.OS == 'windows') {
        utils::download.file(url = dl_link,
                             destfile = dest_file,
                             #method = 'wget',
                             #extra = '--no-check-certificate',
                             quiet = TRUE,
                             mode = 'wb')
      } else {
        # new code (only works in linux)
        #dl_link <- stringr::str_replace(dl_link, stringr::fixed('https'), 'http' )

        # utils::download.file(url = dl_link,
        #                      destfile = dest_file,
        #                      method = 'wget',
        #                      extra = '--no-check-certificate',
        #                      quiet = TRUE,
        #                      mode = 'wb')

        utils::download.file(url = dl_link,
                             destfile = dest_file,
                             method = 'wget',
                             extra = "--ciphers 'DEFAULT:!DH' --no-check-certificate", # use unsecure dl
                             quiet = T,
                             mode = 'wb')
      }



    })

    if (file.size(dest_file) < 10  ){
      message(paste0('\t\tError in downloading. Attempt ',i_try,'/', max_dl_tries),
              appendLF = FALSE)
      Sys.sleep(1)
    } else {

      if (!be_quiet) message('\tSuccess', appendLF = TRUE)
      return(TRUE)
    }

  }

  return(FALSE)


}
