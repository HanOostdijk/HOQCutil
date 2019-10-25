#' attach library as silent as possible
#'
#' @name silent_library
#' @param package_name Character string with name of package or `name` (as in `as.name`) of package
#' @param mywarnings Boolean indicating if warnings should be displayed. Default: FALSE
#' @return Character vector (invisible) with the names of attached packages after attaching `package_name`: identical to (invisible) .packages()
#' @export
#' @examples
#' \dontrun{
#' silent_library('knitr')
#' silent_library(knitr)
#' }
silent_library <- function (package_name,mywarnings = FALSE) {
  suppressWarnings({
    suppressPackageStartupMessages({
      library(
        as.character(as.name(substitute(package_name))) ,
        character.only = TRUE,
        warn.conflicts = mywarnings,
        quietly = !mywarnings,
        verbose = mywarnings
      )
    })
  })
}
