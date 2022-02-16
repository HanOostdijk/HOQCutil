#' read text of pdf-file by line or by cell
#'
#' @name read_pdf
#' @param filename Character string with path of the pdf-file
#' @param vtolerance Numeric scalar with vertical tolerance
#' @param by Character string with value "line" or "cell" indicating if text is gathered by text line of text cell
#' @param pageno Integer indicating the number of the page to read
#' @return data.frame with the fields 'text', 'y'  and (when by=="cell")  'x'
#' @section Details:
#' Uses \code{\link[pdftools:pdf_data]{pdftools::pdf_data}} as workhorse
#' @export
#' @examples
#' \dontrun{
#' df1 <- read_pdf (r"(D:\data\R\TTVA\inputs\TTV Amstelveen teamindeling Senioren VJ22.pdf)", by= "line")
#' names(df1) # [1] "y" "text"
#' df1 <- read_pdf (r"(D:\data\R\TTVA\inputs\TTV Amstelveen teamindeling Senioren VJ22.pdf)", by= "cell")
#' names(df1) # [1] "x"    "text" "y"
#' }
#'

read_pdf <-  function (filename,
                       vtolerance = 2,
                       by = "line",
                       pageno = 1) {
  df1 <-
    pdftools::pdf_data(filename,
                       font_info = FALSE,
                       opw = "",
                       upw = "")[[pageno]]
  if (nrow(df1) == 0)
    return(data.frame())
  df1 <- df1 %>%
    dplyr::select(x, y, text) %>%
    dplyr::nest_by(y) %>%
    dplyr::ungroup() %>%
    # if difference with previous is small (< vtolerance), then keep them the same
    dplyr::mutate(y1 = ifelse(is.na(lag(y)), 0, lag(y)) ,
           y1 = ifelse(y < y1+!!vtolerance, y1, y)) %>%
    dplyr::select(-y) %>%
    dplyr::rename(y = y1) %>%
    tidyr::unnest(data) %>%
    dplyr::arrange(y, x)
  if (by == "line") {
    df1 <- df1 %>%
    dplyr::nest_by(y) %>%
    dplyr::mutate(x = data$x[1],
           text = paste(data$text, collapse = " ", sep = " "))  %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(x, data))
  }
  df1
}

#' read text of a table from a page of a pdf-file
#'
#' @name read_pdf_fields
#' @param filename Character string with path of the pdf-file
#' @param vtolerance Numeric scalar with vertical tolerance to fix vertical mismatches
#' @param htolerance Numeric scalar with horizontal tolerance to fix mismatches field contents (field starts before header)
#' @param header_line Integer indicating which lines contain the headers of the table
#' @param pageno Integer indicating the number of the page to read
#' @return data.frame with the table . All fields have character values
#' @section Details:
#' It is assumed that the table occupies a whole page and that the columns are defined by the words in the header.\cr
#' In the following example
#' \preformatted{field1         field2           field3}
#' \preformatted{v1a v1b        v2a  v2b     v2c  v3a   v3b}
#' field1 will be filled with "v1a v1b", field2 with "v2a v2b" and field3 with "v3a v3b".\cr
#' Multiple words in a field are separated by only one blank (even when the original data contains more than one blank)\cr\cr
#' The actual reading of the file is done with  \code{\link{read_pdf}} and  \code{\link[pdftools:pdf_data]{pdftools::pdf_data}} .
#' @export
#' @examples
#' \dontrun{
#' df1 <- read_pdf_fields (r"(D:\data\R\TTVA\inputs\TTV Amstelveen teamindeling Senioren VJ22.pdf)" )
#' names(df1) # [1] "Teamnr."    "Klasse"     "Teamrating" "Captain"    "Speler"     "Rating"     "Speeldag"
#' }
#'
read_pdf_fields <- function (filename,
                             vtolerance = 2,
                             htolerance = 2,
                             header_line = 1,
                             pageno = 1) {
  lines <- read_pdf(filename, vtolerance = vtolerance, by = "cell",
                    pageno = pageno) %>%
    dplyr::nest_by(y, .key = 'data')   %>%
    dplyr::ungroup()

  fields <- lines$data[[header_line]] %>%
    dplyr::mutate(x = x - !!htolerance) %>%
    dplyr::rename(x1 = x) %>%
    dplyr::mutate(x2 = dplyr::lead(x1), x2 = ifelse(is.na(x2), 1000, x2))

  lines <- lines %>%
    dplyr::slice(-header_line)

  mufun2 <- function (data, x1, x2) {
    data %>%
      dplyr::filter((x >= !!x1) & (x < !!x2)) %>%
      dplyr::select(text) %>%
      unlist() %>%
      paste(collapse = " ")
  }

  mufun <- function (fields, lines) {
    res <- data.frame(nr = 1:nrow(lines))
    for (ix in (1:nrow(fields))) {
      x1 <- purrr::pluck(fields, 'x1')[ix]
      x2 <- purrr::pluck(fields, 'x2')[ix]
      n  <- purrr::pluck(fields, 'text')[ix]
      df1 <- lines %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sel = mufun2(data,!!x1,!!x2)) %>%
        dplyr::select(sel) %>%
        dplyr::rename(!!n := sel)
      res <- cbind(res, df1)
    }
    res %>% dplyr::select(-nr)
  }

  mufun(fields, lines)
}
