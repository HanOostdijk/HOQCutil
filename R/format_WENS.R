#' format_WE
#'
#' format_WE can be used to format longitude data.
#'
#' @param x numeric vector with longitude data
#' @param W character to use for negative longitudes
#' @param E character to use for positive longitudes
#' @param dig numeric scalar indicating number of digits after decimal point
#' @export

format_WE <- function(x,W='W',E='E',dig=2) {
	WE = paste0(W,E)
	format_WENS(x,WE,dig)
}
#' format_NS
#'
#' format_NS can be used to format lattitude data.
#'
#' @param x numeric vector with lattitude data
#' @param N character to use for positive lattitudes
#' @param S character to use for negative lattitudes
#' @param dig numeric scalar indicating number of digits after decimal point
#' @export

format_NS <- function(x,N='N',S='S',dig=2) {
	NS = paste0(S,N)
	format_WENS(x,NS,dig)
}

format_WENS <- function(x,WENS,dig=3) {
	Z1 = substr(WENS,1,1)
	Z2 = substr(WENS,2,2)
	f  = sprintf('%%.%.0ff',dig)
	xf = sprintf(f,abs(x))
	Z  = ifelse(x < 0, Z1, ifelse(x > 0, Z2,''))
	ticks <- mapply(function(x,z) bquote(.(x)*degree*~.(z)),xf,Z )
	do.call(expression, ticks)
}
