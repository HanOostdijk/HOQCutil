#' replace function in package
#'
#' This function uses trace to (temporarily) replace a function in a package. The same function can be used to redo this action by using `start=F
#' @name replace_package_fun
#' @param fn1 Character string with name of function in package
#' @param fn2 Character string with name of function in the **global** environment. See details
#' @param ns Character with name of package
#' @param start Boolean indicating if the rename should be activated (when TRUE) or redone (when FALSE)
#' @param envir Environment in which the new environment wil be set
#' @return NULL
#' @export
#' @section Acknowledgement:
#' The inner function `my_edit` is an adaptation of the code gotten by using `getOption("editor")` in an RStudio window. I assume this is part of the RStudio product. Because the function did not work in the `blogdown` environment, I replaced the call to (RStudio function ?) `.rs.deparseFunction` with a call to the base function `deparse`.
#' @section Details:
#' The function works by using `trace` to insert a `browser()` line in a function and immediately replacing this line, by using a customized editor function, to a call of the replacement function. See [blog entry](https://www.hanoostdijk.nl/hugo/post/2020/01/25/use-internal-functions-of-a-package/) for an example of use. A non-global function `fn2` can temporarily copied to the global environment. See examples.
#' @examples
#' \dontrun{
#' # example function
#' format_WENS2 <- function (x,WENS='NS',dig=3) {
#' cat('print of  the body of internal function HOQCutil:::internal_demo_function :',sep='\n')
#' print(internal_demo_function)
#' NULL
#' }
#'
#' replace_package_fun('format_WENS','format_WENS2','HOQCutil',start=T)
#' HOQCutil:::format_WENS(3.1415,WENS='NS',dig=3)
#' replace_package_fun('format_WENS','format_WENS2','HOQCutil',start=F)
#'
#' # when format_WENS2 is not a global function :
#'
#' assign('HOQC1181_format_WENS2',format_WENS2,envir = globalenv())
#' replace_package_fun('format_WENS','HOQC1181_format_WENS2','HOQCutil',start=T)
#' HOQCutil:::format_WENS(3.1415,WENS='NS',dig=3)
#' replace_package_fun('format_WENS','HOQC1181_format_WENS2','HOQCutil',start=F)
#' rm(list='HOQC1181_format_WENS2',envir = globalenv() )
#'
#' }

replace_package_fun <- function (fn1, fn2, ns, envir=globalenv(), start = T) {

  fun_call <- paste('return(',fn2,'(',
                    paste(names(formals(fn1,envir=asNamespace(ns))),collapse=','),
                    '))',sep='')

  my_edit <- function (name, file = "", title = file, ...)
  {
    # default editor function changed at indicated places
    if (missing(name))
      return(.Call("rs_editFile", file, PACKAGE = "(embedding)"))
    if (is.null(file) || !nzchar(file)) {
      file <- tempfile("rstudio-scratch-", fileext = ".R")
      on.exit(unlink(file), add = TRUE)
    }
    # next block replace because of missing .rs.deparseFunction in rmarkdown env.
    # deparsed <- if (is.function(name))
    #   .rs.deparseFunction(name, useSource = TRUE, asString = FALSE)
    # else deparse(name)
    deparsed <- if (is.function(name))
      deparse(name)
    else deparse(name)
    # next line inserted: replace trace line by call to replacement function
    deparsed[grepl('doTrace',deparsed)]<-fun_call
    writeLines(deparsed, con = file)
    # next line commented: no need for manual editing
    # .Call("rs_editFile", file, PACKAGE = "(embedding)")
    #### eval(parse(file), envir = globalenv())
    eval(parse(file), envir = envir)
  }

  old_edit = options("editor"= my_edit)
  on.exit(options(old_edit),add=T)
  utils::capture.output(
    {
      if (start) {
        trace(
          fn1,
          browser,
          print = FALSE, edit = TRUE,
          where = asNamespace(ns)
        )
        HOQCutil::set_fun_env(fn2,ns,envir)
      }
      else {
        untrace(fn1, where = asNamespace(ns))
        HOQCutil::set_fun_env(fn2,envir,envir)
      }
    },type = "message")
  invisible(NULL)
}

#' function used for test
internal_demo_function <- function() {
  cat('this is internal demo function','\n')
}

