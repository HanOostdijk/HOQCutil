#' set environment for function
#'
#' This function can set/reset the environment of a function. This is e.g. used in function [replace_package_fun](replace_package_fun) where a function in a package is replaced by a global one. In such a case the environment of the function should be changed to that of the package otherwise other package functions can not be used.
#' @name set_fun_env
#' @param fn Character with name of function
#' @param ns Character with name of package
#' @param e Environment in which the environment statement is executed
#' @export
#' @examples
#' \dontrun{
#' set_fun_env('fun','HOQCutil')
#' }

set_fun_env <- function(fn,ns=NULL,e=globalenv()) {
  if (is.null(ns))
    ns = 'globalenv()'
  else
    ns = paste0('asNamespace(\"',ns,'\")')
  stmt = paste0('environment(',fn,')<-',ns)
  eval(parse(text=stmt),envir=e)
}
