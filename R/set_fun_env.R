#' set environment for function
#'
#' This function can set/reset the environment of a function. This is e.g. used in function [replace_package_fun](replace_package_fun) where a function in a package is replaced by a global one. In such a case the environment of the function should be changed to that of the package otherwise other package functions can not be used.
#' @name set_fun_env
#' @param fn Character with name of function
#' @param ns Character with name of package
#' @param envir Environment in which the environment statement is executed
#' @return the environment that is set (invisible)
#' @section Details:
#' See [blog entry](https://www.hanoostdijk.nl/hugo/post/2020/01/25/use-internal-functions-of-a-package/) for an example of use.
#' @export
#' @examples
#' \dontrun{
#' set_fun_env('fun','HOQCutil')
#' }

set_fun_env <- function(fn, ns = NULL, envir = globalenv()) {
  if (is.null(ns))
    ns = 'globalenv()'
  else if (class(ns) == "character")
    ns = paste0('asNamespace(\"', ns, '\")')
  if ((class(ns) == "character")) {
    stmt = paste0('environment(', fn, ')<-', ns)
    invisible(eval(parse(text = stmt), envir = envir))
  } else if (class(ns) == "environment") {
    x = eval(parse(text="rlang::set_env(get(fn,envir=envir),new_env=ns)"))
    assign(fn,x,envir=envir)
    stmt = paste0('environment(', fn, ')')
    invisible(eval(parse(text=stmt),envir=envir))
  }
}
