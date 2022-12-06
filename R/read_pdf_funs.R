#' read text of pdf-file by line, cell or table
#'
#' The function `read_pdf` reads the text of pdf-file on cell or line level. \cr
#' The output of `read_pdf` on cell level can be combined by function `read_pdf_line` to get the output on line level.\cr
#' The function `read_pdf_fields` reads text of a table from a page of a pdf-file. \cr
#'
#' @name read_pdf
#' @param filename Character string with path of the pdf-file
#' @param vtolerance Numeric scalar with vertical tolerance
#' @param frame_table data.frame indicating data frames on pages. See Details
#' @param by Character string with value "line" or "cell" indicating if text is gathered by text line or cell
#' @return read_pdf returns a data.frame with the fields \cr
#' "page", "seqnr", "framenr", "width", "height", "space", "x", "y" and "text"  when `by` == 'cell' and \cr
#' "page", "framenr", "seqnr", "x", "y" and "text" when `by` == 'line' \cr \cr
#' read_pdf_line always returns a data.frame with the fields
#' "page", "framenr", "seqnr", "x", "y" and "text" \cr \cr
#' read_pdf_fields returns a data.frame with the table . All fields have character values
#' @section Details:
#' Uses \code{\link[pdftools:pdf_data]{pdftools::pdf_data}} as workhorse . \cr
#' The `frame_table` is a data.frame that indicates the location of the frames in the pages.
#' The function [cut3d()] is used to assign a frame number to each cell. See this function for a description
#'
#' @export
#' @rdname readpdffuns
#' @examples
#' \dontrun{
#' df1 <- read_pdf (r"(D:\data\R\TTVA\inputs\TTV Amstelveen teamindeling Senioren VJ22.pdf)", by= "line")
#' names(df1) # [1] "page"    "framenr" "seqnr"   "x"       "y"       "text"
#' df1 <- read_pdf (r"(D:\data\R\TTVA\inputs\TTV Amstelveen teamindeling Senioren VJ22.pdf)", by= "cell")
#' names(df1) # [1] "page"    "framenr" "seqnr"   "width"   "height"  "space"   "x"       "y"       "text"
#' }
#'

read_pdf <- function (filename,
                      vtolerance = 6,
                      frame_table = NULL,
                      by = "line") {
  df1 <- pdftools::pdf_data(filename)
  if (length(df1) == 0)
    return(data.frame())
  df1 <-
    purrr::imap_dfr(df1, function(x, i)
      cbind(data.frame(page = i), x)) |>
    dplyr::mutate(seqnr = dplyr::row_number())
  if (is.null(frame_table)) {
    df1 <- df1 |>
      dplyr::mutate (framenr = 1)
  } else {
    framenr <- cut3d(df1[,c('page','x','y')],frame_table)
    df1 <- dplyr::mutate (df1,framenr = !!framenr)
  }
  df1 <- df1 |>
    dplyr::arrange(page, framenr, y, x) |>
    dplyr::nest_by(page, framenr, y) |>
    dplyr::ungroup() |>
    # if difference with previous is small (< vtolerance), then keep them the same
    dplyr::mutate(y1 = dplyr::lag(y, default = -vtolerance - 1) ,
                  y2 = ifelse(abs(y - y1) > vtolerance, y, NaN)) |>
    tidyr::fill(y2, .direction = "down") |>
    # corrected y now in y2, remove temp. fields and rename
    dplyr::select(-c(y, y1)) |>
    dplyr::rename(y = y2) |>
    tidyr::unnest(data) |>
    dplyr::select (page, framenr, seqnr, width, height, space, x, y, text) |>
    dplyr::arrange(page, framenr, y, x)
  if (by == "line") {
    df1 <- read_pdf_line(df1)
  }
  df1
}

#' read text of pdf-file by line
#'
#' @name read_pdf_line
#' @param read_pdf_df data.frame created by `read_pdf` function
#' @section Details:
#' Uses \code{\link[pdftools:pdf_data]{pdftools::pdf_data}} as workhorse . \cr
#' The fields 'seqnr' and 'x' in the output of read_pdf_line are the attributes of the first cell that contributed to 'text'. \cr \cr
#' The `frame_table` is a data.frame that indicates the location of
#'
#' @export
#' @rdname readpdffuns
#' @examples
#' \dontrun{
#' df1 <- read_pdf (r"(D:\data\R\TTVA\inputs\TTV Amstelveen teamindeling Senioren VJ22.pdf)", by= "cell")
#' df2 <- read_pdf_line(df1)
#' names(df1) # [1] "page"    "framenr" "seqnr"   "x"       "y"       "text"
#' }

read_pdf_line <- function (read_pdf_df) {
  # read_pdf_df is a data.frame created by read_pdf
  read_pdf_df |>
    dplyr::mutate(space = ifelse(space == T, " ", "")) |>
    dplyr::nest_by(page, framenr, y)  |>
    dplyr::mutate(
      x = data$x[1],
      seqnr = data$seqnr[1],
      text = paste(data$text, data$space, collapse = "", sep = "")
    ) |>
    dplyr::ungroup() |>
    dplyr::select(page, framenr, seqnr, x, y, text)
}


#' read text of a table from a page of a pdf-file
#'
#' @name read_pdf_fields
#' @param filename Character string with path of the pdf-file
#' @param vtolerance Numeric scalar with vertical tolerance to fix vertical mismatches
#' @param htolerance Numeric scalar with horizontal tolerance to fix mismatches field contents (field starts before header)
#' @param header_line Integer indicating which lines contain the headers of the table
#' @param pageno Integer indicating the number of the page to read
#' @section Details:
#' Using read_pdf_fields,it is assumed that the table occupies a whole page and that the columns are defined by the words in the header.\cr
#' In the following example
#' \preformatted{field1         field2           field3}
#' \preformatted{v1a v1b        v2a  v2b     v2c  v3a   v3b}
#' field1 will be filled with "v1a v1b", field2 with "v2a v2b" and field3 with "v3a v3b".\cr
#' Multiple words in a field are separated by only one blank (even when the original data contains more than one blank)\cr\cr
#' The actual reading of the file is done with  \code{\link{read_pdf}} and  \code{\link[pdftools:pdf_data]{pdftools::pdf_data}} .
#' @export
#' @rdname readpdffuns
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
  lines <- read_pdf(filename, vtolerance = vtolerance, by = "cell") %>%
    dplyr::filter(page == !!pageno)   %>%
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


#' classify 3D data according to rules
#'
#' @name cut3d
#' @param df1 data.frame with three columns of numerical data that is to be classified (i.e. assigned a number)
#' @param dfclass  data.frame with the rules used for classifying. See Details
#' @param def  numeric value that is assigned to row of `df1` when no rule is satisfied
#' @section Details:
#' `dfclass` (containing the rules) is a data.frame with seven columns: tree times the lower and upper boundary of each of three dimensions
#' and the numeric value that is assigned when a row from `df1` satisfies the rule.
#' A row satifies a rule if each of its fields has a value in the range `[lower,upper]` or when the lower bound is NaN.
#' If the rule is not satisfied, the next rule is checked and so on.
#' @export
#' @examples
#' \dontrun{
#' d123tab <- tibble::tribble(
#'   ~d1L, ~d1U, ~d2L, ~d2U, ~d3L, ~d3U, ~r ,
#'    1,    1,    1,    1,    NaN,  NaN, 1 ,
#'    1,    1,    2,    3,    NaN,  NaN, 2 ,
#'    1,    1,    4,    4,    1,    1,   3 ,
#'    1,    1,    4,    5,    2,    3,   4 ,
#'    1,    1,    NaN,  NaN,  4,    10,   5
#' )
#'
#' mymat <- tibble::tribble(
#'   ~d1, ~d2, ~d3,
#'   1, 1, 6,
#'   1, 4, 1,
#'   1, 4, 6 ,
#'   1, 4, 12
#' )
#'
#' cut3d(mymat,d123tab)
#' # [1] 1 3 5 1
#'
#' }
#'

cut3d <- function (df1, dfclass,def=1) {

  dfL <- purrr::pmap(as.list(df1), list) # https://rpubs.com/wch/200398
  purrr::map_dbl(dfL,~cut3d1(.,dfclass,def1=def))
}

cut3d1 <- function (dfL, dfclass, def1) {
  res = def1
  for (i in seq(nrow(dfclass))) {
    d1 <- ifelse( is.na(dfclass[i,1]) |
                  ((dfclass[i,1] <= dfL[[1]]) &
                  (dfclass[i,2] >= dfL[[1]])), T,F)
    if (d1 == F) next ;
    d2 <- ifelse( is.na(dfclass[i,3]) |
                  ((dfclass[i,3] <= dfL[[2]]) &
                  (dfclass[i,4] >= dfL[[2]])), T,F)
    if (d2 == F) next ;
    d3 <- ifelse( is.na(dfclass[i,5]) |
                  ((dfclass[i,5] <= dfL[[3]]) &
                  (dfclass[i,6] >= dfL[[3]])), T,F)
    if (d3 == T) {
      res <- dfclass[[i,7]]
      break
    }
  }
  res
}
