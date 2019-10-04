#' assign values to multiple variables
#'
#' @name massign
#' @param ... One or more names that can be used as variable names.
#' @param values Values that are replicated where needed to assign to the variable names
#' @param envir The environment in which the values are assigned (bound) to the variable names
#' @return NULL
#' @export
#' @section acknowledgement:
#' I encountered this function in a contribution by Tommy (\url{https://stackoverflow.com/users/662787/tommy}) in \url{https://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line}. I adapted the function slightly to allow a character vector for the first argument and derived the binary operator \code{\%ma\%} from it. I also changed the name of the function from \code{vassign} to \code{massign} because the relevant feature is assignment to multiple variables.
#' @examples
#' \dontrun{
#' massign(aa,bb, values= c(16,30) )
#' x= c('aa','bb') %ma% c(16,30)
#' }

massign <- function(..., values=NULL, envir = parent.frame()) {
	# adapted from
	# https://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line
	# to allow for character string with variable names in first argument
	x <- substitute(...())
	if (length(x) > 1) {
		vars <- as.character(substitute(...()))
	} else {
		vars = as.list(...)
	}
	values <- rep(values, length.out = length(vars))
	for (i in seq_along(vars)) {
		assign(vars[[i]], values[[i]], envir)
	}
	invisible(NULL)
}

#' @export
#'
#' @name %ma%
#' @rdname massign
#' @param n Character vector with variable names.
#' @param v Values that are replicated where needed to assign to the variable names
#'
`%ma%` <- function (n,v,e=parent.frame()) {
	massign(n,values=v,envir = e)
	invisible(NULL)
}
