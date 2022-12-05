#' Retrieving CBS Odata4 table data
#'
#' The function `get_table_cbs_odata4` can be used to retrieve data or information from the new \href{https://www.cbs.nl}{CBS} ('Centraal Bureau voor de Statistiek' or 'Statistics Netherlands') Odata4 data infrastructure. The general OData4 protocol is described in \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} and the part that is implemented by CBS in \url{https://beta-dataportal.cbs.nl/}
#'
#' @name get_table_cbs_odata4
#' @param root Character string with the root for the url. When specified `odata_root` and `odata_cat` are not used
#' @param table_id Character string identifying table for which information will be returned. When table_id == NULL catalog information will be returned.
#' @param subtable Character string indicating subtable for which information will be returned. When subtable == NULL information about the available subtables will be returned.
#' @param query Character string with an OData4 query. See \url{http://docs.oasis-open.org/odata/odata/v4.0/odata-v4.0-part1-protocol.html} for the general OData4 query possibilities and \url{https://acc-ccb.cbs.nl/implement.html} for the subset of the CBS implementation.
#' @param verbose Boolean indicating if the generated url should be shown as a message.
#' @param encode Boolean indicating if the query has to be encode by URLencode.
#' @param odata_root Character string with the root for the url.
#' @param odata_cat Character string with the catalog identifier.
#' @param restype Character string indicating if the query has to return a `data.frame` or `list` (when `''`), a httr response object (when `resp`) or a JSON string (when `json`).
#' @param error_msg Boolean indicating if the query has to return a more extensive error message. Useful for debugging.
#' @return if not succesful a character string with an error message. If succesful the contents is regarded as a json object and translated to a data.frame or list when possible. However if  `restype='resp'`  the result is a httr response object and when `restype='json'` the JSON string is not converted

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
#'
#' myurl = "https://beta-odata4.cbs.nl/CBS/82931NED/Observations?$format=json&$skip=1&$top=2"
#' t1=get_table_cbs_odata4_GET(myurl)
#' t1=get_table_cbs_odata4_GET(myurl,restype='resp')
#' t1=get_table_cbs_odata4_GET(myurl,restype='json')
#' t1=get_table_cbs_odata4_GET(myurl,error_msg=T)
#' }
#' @section Additional example:
#' See \url{https://gist.github.com/HanOostdijk/787e9724dcd63735a431bcd16cbd18a0} for an analysis done with `get_table_cbs_odata4`. The resulting PDF file can be found in \url{https://www.hanoostdijk.nl/hugo/opendata_beta_versie4_dec2018_20181225.pdf} .

get_table_cbs_odata4 <-
	function (root = NULL,
		table_id = NULL,
		subtable = NULL,
		query = NULL,
		verbose = FALSE,
		encode = TRUE,
		odata_root = "https://odata4.cbs.nl",
		odata_cat  = "CBS",
		restype = '',
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
					query1 = utils::URLencode(query)
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
		res = get_table_cbs_odata4_GET(url1, restype, error_msg)
		if (post_code > 0 && is.list(res) && class(res)!="response") {
			res = as.data.frame((res)[-1])
		}
		res
	}

#' get_table_cbs_odata4_GET : retrieves url
#'
#' The function `get_table_cbs_odata4_GET` handles the network IO for the `get_table_cbs_odata4` function but can also be used stand-alone.
#'
#' @name get_table_cbs_odata4_GET
#' @param url Character string with url that will be passed to GET function without further encoding. It is assumed that a json result can be returned.
#' @param restype Character string indicating if the query has to return a `data.frame` or `list` (when `''`), a httr response object (when `resp`) or a JSON string (when `json`).
#' @param error_msg Boolean indicating if the query has to return a more extensive error message. Useful for debugging.

#' @rdname get_table_cbs_odata4
#' @export

get_table_cbs_odata4_GET <- function (url,
	restype = '',
	error_msg = TRUE) {
	res1 = httr::GET(url)
	err1 = httr::http_error(res1)
	txt1 = httr::content(res1, as = "text",encoding='UTF-8')
	msg1 = httr::http_status(res1)$message
	cnt1 = httr::headers(res1)$`content-type`
	jsn1 = stringr::str_detect(httr::headers(res1)$`content-type`, 'json')
	xml1 = stringr::str_detect(httr::headers(res1)$`content-type`, 'xml')
	restype = tolower(restype)
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
	if (restype == 'resp') {
		return(res1)
	}
	else if (err1)
		return(msg1)
	else if (restype == 'json')
	  return(txt1)
	else if (jsn1) {
		if (!is.null(e$value)) {
		  ev = e$value
		  while (!is.null(e$`@odata.nextLink`)) {
		    res1 = httr::GET(e$`@odata.nextLink`)
  	    txt1 = httr::content(res1, as = "text",encoding='UTF-8')
	   		e = get_jsonx(txt1)
		    ev <- rbind(ev,e$value)
		  }
			return(ev)
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
