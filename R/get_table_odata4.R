#' get_table_cbs_odata4 : retrieves CBS Odata4 table data
#'
#' This function can be used to retrieve data or information from the new \href{https://www.cbs.nl}{CBS} ('Centraal Bureau voor de Statistiek' or 'Statistics Netherlands') Odata4 data infrastructure. The general OData4 protocol is described in \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} and the part that is implemented by CBS in  \url{https://acc-ccb.cbs.nl/implement.html}
#'
#' @name get_table_cbs_odata4
#' @param table_id Character string identifying table for which information will be returned. When table_id == NULL catalog information will be returned. Default: NULL
#' @param subtable Character string indicating subtable for which information will be returned. When subtable == NULL information about the available subtables will be returned. Default: NULL
#' @param query Character string with an OData4 query. See \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} for the general OData4 query possibilities and \url{https://acc-ccb.cbs.nl/implement.html} for the subset of the CBS implementation. Default: ""
#' @param verbose Boolean indicating if the generated url should be shown as a message. Default: FALSE
#' @param encode Boolean indicating if the query has to be encode by URLencode. Default: TRUE
#' @param odata_root Character string with the root for the url. Default: "https://acc-ccb.cbs.nl"
#' @param odata_cat Character string with the catalog identifier. Default: "CBS"
#' @param response Boolean indicating if the query has to return the httr response object instead of a table. Useful for debugging. Default: FALSE
#' @param error_msg Boolean indicating if the query has to return a more extensive error message. Useful for debugging. Default: FALSE
#' @return if not succesful a character string with an error message. If succesful a data.frame when the contents of a subtable was requested and a list with the properties of the table when Properties was requested. If the extra parameter response=TRUE is set, the result is a httr response object.

#' @export

#' @examples
#' \dontrun{
#' t1=get_table_cbs_odata4()
#' t1=get_table_cbs_odata4(subtable="Datasets")
#' t1=get_table_cbs_odata4(subtable="Catalogs")
#' t1=get_table_cbs_odata4(table_id="82931NED")
#' t1=get_table_cbs_odata4(table_id="82931NED",subtable="Dimensions")
#' t1=get_table_cbs_odata4(table_id="82931NED",subtable="Properties")
#' t1=get_table_cbs_odata4(table_id="82931NED",subtable="Observations",query="$skip=1&$top=2")
#' t1=get_table_cbs_odata4(table_id="82931NED",subtable="Observations",query="$count")
#' t1=get_table_cbs_odata4(table_id="82931NED",subtable="Observations",query="(7)")
#' }

get_table_cbs_odata4 <-
	function (root = NULL,
		table_id = NULL,
		subtable = NULL,
		query = NULL,
		verbose = F,
		encode = T,
		odata_root = "https://acc-ccb.cbs.nl",
		odata_cat  = "CBS",
		response = F,
		error_msg = F) {
		if (is.null(subtable)) {
			subtable = ""
		} else
			subtable = glue::glue("/{subtable}")
		if (is.null(table_id)) {
			table = ""
		} else
			table = glue::glue("/{table_id}")
		root = glue::glue("{odata_root}/{odata_cat}")
		if (is.null(query)) {
			query1 = ""
		}	else {
			if (stringr::str_to_lower(query) == '$count') {
				query1 = '$count'
			} else if (stringr::str_detect(stringr::str_trim(query), "^\\(\\d+\\)$")) {
				query1 = stringr::str_trim(query)
			}	else {
				if (encode == TRUE) {
					query1 = URLencode(query)
				} else {
					query1 = query
				}
				query1 = glue::glue("&{query1}")
			}
		}
		post_code = 0
		if (stringr::str_to_lower(query1) == '$count') {
			url1 = glue::glue("{root}{table}{subtable}/{query1}")
		} else if (stringr::str_detect(query1, "^\\(\\d+\\)$")) {
			post_code = 1
			url1 = glue::glue("{root}{table}{subtable}{query1}")
		} else {
			url1 = glue::glue("{root}{table}{subtable}?$format=json{query1}")
		}
		if (verbose == T) {
			suppressWarnings({
				if (require('HOQCutil', quietly = TRUE)) {
					cat(HOQCutil::hard_split(
						glue::glue("generated url: {url1}"),
						getOption('width')
					), sep = "\n")
					cat(HOQCutil::hard_split(
						glue::glue("unencoded query:  {query}"),
						getOption('width')
					), sep = "\n")
				} else {
					cat(glue::glue("generated url: {url1}"), sep = "\n")
					cat(glue::glue("unencoded query:  {query}"), sep = "\n")
				}
			})
		}
		res = get_table_cbs_odata4_GET(url1, response, error_msg)
		if (post_code > 0 && !(class(res) == 'response')) {
			res = as.data.frame((res)[-1])
		}
		res
	}

#' get_table_cbs_odata4_GET : retrieves url
#'
#' This function handles the network IO for the \code{\link{get_table_cbs_odata4}} function but can also be used stand-alone.
#'
#' @name get_table_cbs_odata4_GET
#' @param url Character string with url that will be passed to GET function without further encoding. It is assumed that a json result can be returned. Default: none
#' @param response Boolean indicating if the query has to return the httr response object instead of a table. Useful for debugging. Default: FALSE
#' @param error_msg Boolean indicating if the query has to return a more extensive error message. Useful for debugging. Default: FALSE
#' @return if not succesful a character string with an error message. If succesful the contents is regarded as a json object and translated to a data.frame or list when possible. If the parameter response=TRUE is set, the result is a httr response object.

#' @export

#' @examples
#' \dontrun{
#' myurl = "https://acc-ccb.cbs.nl/CBS/82931NED/Observations?$format=json&$skip=1&$top=2"
#' t1=get_table_cbs_odata4_GET(myurl)
#' t1=get_table_cbs_odata4_GET(myurl,response=T)
#' t1=get_table_cbs_odata4_GET(myurl,error_msg=T)
#' }

get_table_cbs_odata4_GET <- function (url,
	response = F,
	error_msg = F) {
	res1 = httr::GET(url)
	if (error_msg == TRUE) {
		e = jsonlite::fromJSON(httr::content(res1, as = "text"))
		m = purrr::pluck(e, 'error', 'message')
		if (length(m) > 0) {
			cat('\nerror message (get_table_cbs_odata4_GET) :\n')
			cat(stringr::str_wrap(m))
			d = purrr::pluck(e, 'error', 'details')
			if (length(d) > 0) {
				cat('\nerror details:\n')
				print(d)
			}
		}
	}
	if (response == TRUE) {
		return(res1)
	}
	else if (httr::http_error(res1))
		return(httr::http_status(res1)$message)
	else if (stringr::str_detect(httr::headers(res1)$`content-type`, 'json')) {
		res2 = jsonlite::fromJSON(httr::content(res1, as = "text"))
		if (!is.null(res2$value)) {
			return(res2$value)
		} else {
			return(res2)
		}
	} else if (stringr::str_detect(httr::headers(res1)$`content-type`, 'xml')) {
		res2 = xml2::read_xml(httr::content(res1, as = "text"))
	} else {
		res2 = httr::content(res1, as = "text")
	}
}

