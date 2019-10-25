#' debugging the httr::GET function
#'
#' By temporarily setting a trace on \code{httr::GET} the \code{debug_httr_get} function is able to retrieve the url that is used as input and the resulting response object. This is especially useful when debugging a high-level function that somewhere in a subfunction makes one of more calls to \code{httr::GET}.
#'
#' @name debug_httr_get
#' @param fn An expression that calls (indirectly) the \code{httr::GET} function
#' @param ret A character string or a numeric vector indicating the information that should be returned from the call(s) to \code{httr::GET}. Default: finalurl'. See details section.
#' @return List or character value with the requested information. See details section
#' @export
#' @section details:
#'
#' The function works by temporarily setting a trace on the \code{httr::GET} function. The result is a list with the requested information for each call to \code{httr::GET} that is caused by the expression in \code{fn}. If there is only one call to \code{httr::GET} the list has length 1 and will be unpacked. The possible values for \code{fn} and the returned information:
#' \describe{
#' \item{none or 'finalurl':}{the full url for which the \code{httr::GET} request is done}
#' \item{'inputurl':}{the url structure that was the input for \code{httr::GET} request}
#' \item{'resp':}{the resulting \code{httr::GET} response object}
#' \item{'json':}{the formatted `json` representation of the content of the response object}
#' \item{'fjson':}{the content of the response object after applying \code{jsonlite::fromJSON}}
#' \item{'org_result':}{the original results of the expression (as if no tracing had been used)}
#' \item{a numeric vector:}{a portion of the response object according to the vector used in \code{purrr::pluck} to extract from the response object}
#' }
#' @examples
#' \dontrun{
#' x=debug_httr_get(  rtweet::get_trends("23424977")              )
#' print(x)
#' debug_httr_get(  rtweet::get_trends("23424977"), ret=c(9,2)  )
#' print(x)
#' x=debug_httr_get(  rtweet::get_trends("23424977"), ret='resp'  )
#' str(x)
#' }

debug_httr_get <- function (fn, ret = 'finalurl') {
	# setup for initialisation and exit
	on.exit(remove('debug_httr_get_data', envir = .GlobalEnv))
	base::assign('debug_httr_get_data', list(), envir = .GlobalEnv)
	what_to_do <-  quote({
		odata =
			get('debug_httr_get_data', envir = .GlobalEnv)
		base::assign('debug_httr_get_data',
								 append(odata, list(
								 	list(
								 		hu = hu,
								 		url = url,
								 		ret = returnValue()
								 	)
								 )),
								 envir = .GlobalEnv)
	})
	# set trace, execute statement, do untrace, and retrieve collected data
	d = utils::capture.output({
		trace(
			httr::GET,
			exit = what_to_do,
			print = FALSE,
			where = asNamespace("rtweet")
		)
	  debug_httr_get_data_org_result = fn
		untrace(httr::GET)
		res = base::get('debug_httr_get_data',
										envir = .GlobalEnv)
	},
	type = c('message'))
	# select the requested information from the collected data
	if (is.numeric(ret)) {
		ret1 = as.list(ret)
		ret = 'numeric'
	}
	resp = purrr::map(res,  ~ purrr::pluck(.x, 'ret'))
	`%>%` = magrittr::`%>%`
	res = switch(
		ret,
		purrr::map_chr(res,  ~ purrr::pluck(.x, 'hu', 'url')),
		'inputurl' = purrr::map(res,  ~ purrr::pluck(.x, 'url')),
		'resp' = resp,
		'numeric' = purrr::map(resp,  ~ do.call(purrr::pluck, c(list(
			.x
		), ret1))),
		'json' = purrr::map(resp,  ~ httr::content(.x, as = 'text')) %>%
			purrr::map( ~ jsonlite::prettify(.x, indent = 2)),
		'fjson' = purrr::map(resp,  ~ httr::content(.x, as = 'text')) %>%
		  purrr::map( ~ jsonlite::fromJSON(.x)),
		'org_result' = debug_httr_get_data_org_result
	)
	if (is.list(res) &&
			all(purrr::map_lgl(res, is.character))  &&
			ret != 'json' && ret != 'org_result')
		res = purrr::flatten_chr(res)
	if (length(res) == 1 && is.list(res))
		res = res[[1]]
	res
}
