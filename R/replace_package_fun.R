#' replace function in package
#'
#' This function uses trace to (temporarily) replace a function in a package. The same function can be used to redo this action by using `start=F
#' @name replace_package_fun
#' @param fn1 Character string with name of function in package
#' @param fn2 Character string with name of function in the global environment
#' @param ns Character with name of package
#' @param start Boolean indicating if the rename should be activated (when TRUE) or redone (when FALSE)
#' @return NULL
#' @export
#' @section Acknowledgement:
#' The inner function `my_edit` is an adaptation of the code gotten by using `getOption("editor")` in an RStudio window. I assume this is part of the RStudio product. Because the function did not work in the `blogdown` environment, I replaced the call to (RStudio function ?) `.rs.deparseFunction` with a call to the base function `deparse`.
#' @section Details:
#' The function works by using `trace` to insert a `browser()` line in a function and immediately replacing this line, by using a customized editor function, to a call of the replacement function.
#' @examples
#' \dontrun{
#' format_WENS2 <- function (x,WENS='NS',dig=3) {
#' cat('print of  the body of internal function HOQCutil:::internal_demo_function :',sep='\n')
#' print(internal_demo_function)
#' }
#' replace_package_fun('format_WENS','format_WENS2','HOQCutil',start=T)
#' HOQCutil:::format_WENS(3.1415,WENS='NS',dig=3)
#' replace_package_fun('format_WENS','format_WENS2','HOQCutil',start=F)
#' }

replace_package_fun <- function (fn1, fn2, ns, start = T) {

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
    eval(parse(file), envir = globalenv())
  }

  old_edit = options("editor"= my_edit)
  on.exit(options(old_edit),add=T)
  if (start) {
    trace(
      fn1,
      browser,
      print = FALSE, edit = TRUE,
      where = asNamespace(ns)
    )
    set_fun_env(fn2,ns)
  }
  else {
    untrace(fn1, where = asNamespace(ns))
    set_fun_env(fn2)
  }
  invisible(NULL)
}

#' function used for test
internal_demo_function <- function() {
  cat('this is internal demo function','\n')
}

