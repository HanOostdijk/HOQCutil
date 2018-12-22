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
#' @param error_msg Boolean indicating if the query has to return a more extensive error message. Useful for debugging. Default: TRUE
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
		verbose = FALSE,
		encode = TRUE,
		odata_root = "https://acc-ccb.cbs.nl",
		odata_cat  = "CBS",
		response = FALSE,
		error_msg = TRUE) {
		if (is.null(subtable)) {
			subtable = ""
		} else
			subtable = glue::glue("/{subtable}")
		if (is.null(table_id)) {
			table = ""
		} else
			table = glue::glue("/{table_id}")
		if (is.null(root)) {
			odata_root = stringr::str_trim(odata_root)
			odata_cat = stringr::str_trim(odata_cat)
			if (stringr::str_length(odata_cat) > 0)
				root = glue::glue("{odata_root}/{odata_cat}")
			else
				root = odata_root
		}
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
				#query1 = glue::glue("&{query1}")
			}
		}
		post_code = 0
		if (stringr::str_to_lower(query1) == '$count') {
			url1 = glue::glue("{root}{table}{subtable}/{query1}")
		} else if (stringr::str_detect(query1, "^\\(\\d+\\)$")) {
			post_code = 1
			url1 = glue::glue("{root}{table}{subtable}{query1}")
		} else {
			# url1 = glue::glue("{root}{table}{subtable}?$format=json{query1}")
			if (stringr::str_length(query1) == 0)
				url1 = glue::glue("{root}{table}{subtable}")
			else
				url1 = glue::glue("{root}{table}{subtable}?{query1}")
		}
		if (verbose == T) {
			w = getOption('width', 110)
			display_wrapped(glue::glue("generated url  : {url1}"), w)
			display_wrapped(glue::glue("unencoded query: {query}"), w)
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
#' @param error_msg Boolean indicating if the query has to return a more extensive error message. Useful for debugging. Default: TRUE
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
	response = FALSE,
	error_msg = TRUE) {
	res1 = httr::GET(url)
	err1 = httr::http_error(res1)
	txt1 = httr::content(res1, as = "text",encoding='UTF-8')
	msg1 = httr::http_status(res1)$message
	cnt1 = httr::headers(res1)$`content-type`
	jsn1 = stringr::str_detect(httr::headers(res1)$`content-type`, 'json')
	xml1 = stringr::str_detect(httr::headers(res1)$`content-type`, 'xml')
	if (length(jsn1) == 0 && err1)
		return(msg1)
	if (jsn1) {
		e = get_json(txt1)
	}
	if (jsn1 && error_msg) {
		w = getOption('width', 110)
		if (is.null(e)) {
			cat('\nerror in json ? (get_table_cbs_odata4_GET) :\n')
			display_wrapped(txt1, w, T)
		} else {
			m = purrr::pluck(e, 'error', 'message')
			if (length(m) > 0) {
				cat('\nerror message (get_table_cbs_odata4_GET) :\n')
				display_wrapped(m, w, T)
				d = purrr::pluck(e, 'error', 'details')
				if (length(d) > 0) {
					cat('\nerror details:\n')
					print(d)
				}
			}
		}
	}
	if (response == TRUE) {
		return(res1)
	}
	else if (err1)
		return(msg1)
	else if (jsn1) {
		if (!is.null(e$value)) {
			return(e$value)
		} else {
			return(e)
		}
	} else if (xml1) {
		res2 = xml2::read_xml(txt1)
	} else {
		res2 = txt1
	}
}

get_json <- function (tekst) {
	hoqc_chr <- 'hoqc error'
	tryCatch(
		hoqc_chr <- jsonlite::fromJSON(tekst),
		error = function(e)
			e,
		finally = {
		}
	)
	if (length(hoqc_chr) == 1 && hoqc_chr == 'hoqc error') {
		hoqc_chr = NULL
	}
	hoqc_chr
}
