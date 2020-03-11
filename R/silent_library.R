#' attach library as silent as possible
#'
#' @name silent_library
#' @param package_name Character string with name of package or `name` (as in `as.name`) of package or a hardcode character vector (**not** the **name** of a character vector)
#' @param mywarnings Boolean indicating if warnings should be displayed. This argument is left in for compatibility reason but will be ignored.
#' @return NULL (invisible)
#' @export
#' @examples
#' \dontrun{
#' silent_library('knitr')
#' silent_library(knitr)
#' packages_to_load <- c('glue','FactoMineR','curl')
#' purrr::walk(packages_to_load[1:2],silent_library)
#' silent_library(packages_to_load[[3]])
#' silent_library(c('glue','FactoMineR','curl'))
#' # silent_library(packages_to_load) # won't work: "there is no package called 'packages_to_load'"
#' }

silent_library <- function (package_name, mywarnings = FALSE) {
  package_name2 <- rlang::enexpr(package_name)
  if (rlang::is_symbol(package_name2)) {
    package_name <- rlang::as_string(package_name2)
  }
  suppressWarnings({
    suppressPackageStartupMessages({
      lapply(package_name,
             function(p){
               library(
                 p,
                 character.only = TRUE,
                 warn.conflicts = FALSE,
                 quietly = TRUE,
                 verbose = FALSE
               )
             })
    })
  })
  invisible(NULL)
}
