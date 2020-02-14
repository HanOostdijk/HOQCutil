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
#' packages_to_load <- c('glue','FactoMineR','curl')
#' purrr::walk(packages_to_load[1:2],silent_library)
#' silent_library(packages_to_load[[3]])
#' }
silent_library <- function (package_name,mywarnings = FALSE) {
  package_name2 <- rlang::enexpr(package_name)
  if (rlang::is_symbol(package_name2)) {
    package_name <- rlang::as_string(package_name2)
  }
  suppressWarnings({
    suppressPackageStartupMessages({
      library(
        package_name,
        character.only = TRUE,
        warn.conflicts = mywarnings,
        quietly = !mywarnings,
        verbose = mywarnings
      )
    })
  })
}
