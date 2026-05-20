#' Cleans the local cache folder
#'
#' This function deletes all downloaded ZIP files, temporary files, and cached
#' datasets stored in the cache folder.
#'
#' @param cache_folder Path of cache folder to delete (default = get_default_cache_dir())
#'
#' @return A boolean indicating whether the operation succeeded (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' clean_fre_cache()
#' }
clean_fre_cache <- function(cache_folder = get_default_cache_dir()) {
  if (!fs::dir_exists(cache_folder)) {
    cli::cli_alert_info("Cache folder {.path {cache_folder}} does not exist. Nothing to clean.")
    return(invisible(FALSE))
  }

  cli::cli_alert_info("Cleaning cache folder {.path {cache_folder}}...")

  tryCatch({
    fs::dir_delete(cache_folder)
    cli::cli_alert_success("Successfully deleted cache folder {.path {cache_folder}}.")
    return(invisible(TRUE))
  }, error = function(e) {
    cli::cli_abort("Failed to delete cache folder {.path {cache_folder}}: {e$message}")
  })
}
