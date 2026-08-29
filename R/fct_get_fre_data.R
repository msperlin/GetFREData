#' Downloads and reads FRE datasets (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_fre_data()` was deprecated in v1.0.0 in favour of [get_fre_data2()].
#'
#' @param ... Ignored.
#'
#' @return Nothing; always errors.
#' @export
#'
#' @examples
#' \dontrun{
#' # Use get_fre_data2() instead
#' l_fre <- get_fre_data2()
#' }
get_fre_data <- function(...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "get_fre_data()",
    with = "get_fre_data2()"
  )
}
