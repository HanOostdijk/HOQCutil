#' Scanner functions
#'
#'  [cleanup_bw()], [scan_with_hocr()] and [extract_table()] can be used to cleanup and scan (OCR) an image and extract a table into a data.frame format. The workflow would then be:
#'  - read an image from file with [magick::image_read()] \cr e.g.
#'    `img1 = magick::image_read('example1.png')`
#'  - define the list with cleanup options \cr e.g.
#'    `cln_options1 = list(resize="4000x",trim=10,enhance=TRUE,sharpen=1)`
#'  - use the [cleanup_bw()] function with this list \cr e.g.
#'    `img2 = img2 = clean_up (img1,cln_options1)`
#'  - scan (OCR) the cleansed image with [scan_with_hocr()] \cr e.g.
#'    `df1  = scan_with_hocr(img2,add_header_cols=F)`
#'  - indicate in the columns of `df1` which fields belong to the table headers (or alternatively define a `headers` list)
#'  - extract the table with the [extract_table()] function \cr e.g.
#'    `df2= extract_table(df1, headers=NULL,lastline = Inf, desc_above=T)` or alternatively \cr
#'    `df2= extract_table(df1, headers=headers,lastline = Inf, desc_above=T)`
#' @name scanner_functions
NULL
#'
#' cleaning an image prior to Tesseract scan
#'
#' A Tesseract scan often gives better results when the image is transformed. The `cleanup_bw` function transforms the image to black-and-white and allows some other transformations (see **Details**)
#'
#' @name cleanup_bw
#' @param img	 An image object returned by `image_read()` or `image_graph()`
#' @param cln_options A list with options for `magick` transformation function. Default: `list()`. See
#' **Details** for the transformations that can be applied.
#' @return An image object with the applied transformations
#' @export
#' @section Details:
#' The following list describe the transformations and the order in which they are done. Also is indicated if they are always done or only when specified in the `cln_options` list:
#' - [magick::image_trim()] removes edges that are the background color from the image. Only when `trim=` is in the list
#' - [magick::image_resize()] magick documentation : *resizes using custom filterType* (??). Only when `resize=` is in the list. (I use it as `resize= "4000x"`)
#' - [magick::image_modulate()] adjusts brightness, saturation and hue of image relative to current. The transformation is always done with the value 100 for each of the `brightness`, `saturation` and `hue` arguments unless overwritten by a equally named entry in the `cln_options` list (e.g. `brightness=120`)
#' - [magick::image_contrast()] enhances intensity differences in image. Only when `sharpen=` is in the list (e.g. `sharpen=1`)
#' - [magick::image_quantize()] reduces number of unique colors in the image. Is always done with argument `colorspace='gray'`
#' - [magick::image_transparent()] and [magick::image_background()] are always done for the 'colors' white and black to make the image black-and-white
#'- [magick::image_enhance()] tries to minimize noise. Done only when `enhance=TRUE` in the list.
#' @seealso [scanner_functions],[scan_with_hocr()] and [extract_table()]
#' @examples
#' \dontrun{
#' cln_options1 = list(resize="4000x",
#'                   trim=10,enhance=TRUE,sharpen=1)
#' img2 = clean_up (img1,cln_options1)
#' }

cleanup_bw <- function (img,cln_options=list()) {
  # clean image
  force(cln_options)
  if (!is.null(cln_options$trim)) {
    img = magick::image_trim(img,fuzz = cln_options$trim)
  }
  if (!is.null(cln_options$resize)) {
    img = magick::image_resize(img,cln_options$resize)
  }
  if (!is.null(cln_options$brightness)) {
    brightness = cln_options$brightness
  } else {
    brightness = 100
  }
  if (!is.null(cln_options$saturation)) {
    saturation = cln_options$saturation
  } else {
    saturation = 100
  }
  if (!is.null(cln_options$hue)) {
    hue = cln_options$hue
  } else {
    hue = 100
  }
  img = magick::image_modulate(img,
                               brightness=brightness, saturation=saturation, hue=hue)
  if (!is.null(cln_options$sharpen))  {
    img = magick::image_contrast(img,sharpen=cln_options$sharpen)
  }
  img = magick::image_quantize(img,colorspace ="gray")
  img = magick::image_background(
    magick::image_transparent(img, 'white', fuzz = 25), 'white')
  img = magick::image_background(
    magick::image_transparent(img, 'black', fuzz =75), 'black')
  if ( (!is.null(cln_options$enhance)) && cln_options$enhance == TRUE) {
    img = magick::image_enhance(img)
  }
  img
}
#' doing a Tesseract scan with HOCR output
#'
#' A Tesseract scan with HOCR output returns an XHTML document with not only the scanned word, but also information about the line where the word is found and the bounding box. The function `scan_with_hocr` does the scan and converts the document to a data frame. See **Details** and **Acknowledgment**.
#'
#' @name scan_with_hocr
#' @param img An image object or a character string with the name of an image file
#' @param confsel A Boolean indicating if the confidence rate should also be selected
#' @param extract_bbox A Boolean indicating if the bounding box should be unpacked (into x and y coordinates)
#' @param add_header_cols A Boolean indicating if two header columns (`header_col` and `header_col_seq`) should be added to the result and initialized to resp. `0` and `1`. Useful when [extract_table()] is used later on.
#' @param engine The OCR engine to use. See [tesseract::tesseract()]
#' @return A data.frame with the scanned words. See **Details**
#' @export
#' @section Details:
#' The result is a data.frame with one row for each word found and the following columns
#' - line : the line on which the word was found
#' - fldnr: the sequence number of the word on this line
#' - word : the word that is recognized by the engine
#' - bbox : the bounding box where the word was found (character string with e.g. '19 227 1087 251' indicating x-coordinates x1=19 and x2=1087 and y-coordinates y1=227 and y2=251). Not present when `extract_bbox=T` is set: in that case x1, x2, y1 and y2 are present.
#' - conf : the confidence rate of the word (only when `confsel=T` is set)
#' - header_col : column with `0`-s (only when `add_header_cols=T` is set)
#' - header_col_seq : column with `1`-s (only when `add_header_cols=T` is set)
#' @seealso [scanner_functions] , [cleanup_bw()] and [extract_table()]
#' @section Acknowledgment:
#' This function is an extension of the snippet by [Jeroen Ooms](https://ropensci.org/technotes/2018/02/14/tesseract-18/). I only added the extraction of the line info. Afterwards I made it into a function and usable for connecting it with [extract_table()].
#' @examples
#' \dontrun{
#' df1  = scan_with_hocr(img2,add_header_cols=F)
#' }
scan_with_hocr <- function (
  img,
  confsel=F,
  extract_bbox=T,
  add_header_cols=F,
  engine = tesseract::tesseract("eng"))
{
  # from # https://ropensci.org/technotes/2018/02/14/tesseract-18/
  `%>%` <- magrittr::`%>%`
  xml <- tesseract::ocr(img, HOCR = TRUE,engine = engine)
  doc <- xml2::read_xml(xml)
  nodes <- xml2::xml_find_all(doc, ".//span[@class='ocrx_word']")
  words <- xml2::xml_text(nodes)
  meta <- xml2::xml_attr(nodes, 'title')
  bbox <- stringr::str_replace(
    stringr::str_extract(meta, "bbox [\\d ]+"),
    "bbox ", "")
  conf <- as.numeric(stringr::str_replace(
    stringr::str_extract(meta, "x_wconf.*"), "x_wconf ", ""))
  nodesp <- purrr::map(nodes,~xml2::xml_parent(.))
  line <- unlist(purrr::map(nodesp,~xml2::xml_attr(., 'id'))  ) # e.g. id='line_1_8'
  line <- as.numeric(stringr::str_extract(line, "[\\d]+$"))
  header_col <- rep(0,length(line))
  header_col_seq <- rep(1,length(line))
  tb = data.frame(line=line, word = words, bbox = bbox, conf=conf,
                  header_col=header_col, header_col_seq=header_col_seq,
                  stringsAsFactors = F)
  tb = tb %>%
    dplyr::group_by(line) %>%
    dplyr::mutate(fld_nr=1:dplyr::n()) %>%
    dplyr::ungroup()
  if (extract_bbox)
    tb = tb %>%
    tidyr::separate(bbox,c('x1','y1','x2','y2'),' ') %>%
    dplyr::mutate_at(.vars=c('x1','y1','x2','y2'),as.numeric) %>%
    dplyr::select(line,fld_nr,word,x1,y1,y2,x2,conf,header_col,header_col_seq)
  if (confsel==F)
    tb = tb %>%
    dplyr::select(-conf)
  if (add_header_cols==F)
    tb = tb %>%
    dplyr::select(-header_col,-header_col_seq)
  tb
}

find_header_data1 <- function (df, entries) {
  row = df$line
  fld_nr = df$fld_nr
  find_entry <- function (entry, row, fld_nr) {
    which(entry[1] == row & entry[2] == fld_nr)
  }
  ixs = purrr::map_dbl(entries,  ~ find_entry(., row, fld_nr))
  h   = paste(df$word[ixs], collapse = ' ')
  mx1  = min(df$x1[ixs])
  mx2  = max(df$x2[ixs])
  #ml1  = min(df$line[ixs])
  ml2  = max(df$line[ixs])
  data.frame(t = h, x1 = mx1, x2 = mx2,
             ml2 = ml2, stringsAsFactors = F)
}

find_header_data2 <- function (df,header_cols) {
  if (! all(header_cols %in% names(df)))
    stop(paste(paste(header_cols,collapse = ' and '),' not in data.frame'))
  h1 = rlang::sym(header_cols[1])
  h2 = rlang::sym(header_cols[2])
  df_header = df %>%
    dplyr::filter(!!h1 > 0) %>%
    dplyr::arrange(!!h1,!!h2) %>%
    dplyr::group_by(!!h1) %>%
    dplyr::summarise(t = paste(trimws(word), collapse = " "),
                     x1  = min(x1),
                     x2  = max(x2),
                     ml2  = max(line)) %>%
    dplyr::select(-!!h1)
}

# df should contain the fields line, fld_nr, word, x1, x2
# and possibly header_col andheader_col_seq or aliases

#' extract a table from Tesseract HOCR scan
#'
#' If we use Tesseract with HOCR output (e.g. with [scan_with_hocr()]) to scan a table, we will have all data in a data.frame . This function convert this data.frame to a proper table when we indicate which data.frame elements to use as headers. See **Details**.
#'
#' @name extract_table
#' @param df A data.frame like the result of [scan_with_hocr()]. See **Details**.
#' @param headers A character vector or NULL indicating how the headers will be determined. See **Details**.
#' @param lastline An integer indicating the last line (number) in `df` that will be used.
#' @param desc_above A Boolean indicating if the description is above or on the same line  as its value (TRUE) or below or on the same line (FALSE)
#' @return A data.frame with table contents.
#' @export
#' @section Details:
#' `df` should contain the columns `line`, `word`, `x1`, `x2` and `fld_nr` (in case `headers` is specified) or `headers_col` and `headers_col_spec` (or aliases) when this is not. In this way it can be derived which data values belong to which headers.
#' The specification of headers can be done in two ways:
#' - specify a list as in the example where we indicate a.o. that the first header is composed from the first words on line 2 and 3 and the fourth header from the fourth and fifth word on line 2 and the fourth word on line 3. We use this list as the header argument in the first example.
#' - we use the column `headers_col` to indicate to which header a `df` element belongs. With the `headers_col_seq` column we can indicate which element will be used first. Normally no need to specify this. In this case we can specify `headers=NULL`. When the name of these columns is not `headers_col` and `headers_col_seq` but e.g. `h1` and `h2` we can specify them by using `headers=c('h1','h2')`
#'
#' From the header information we can assign the data to the headers. Everything before the first data that is assigned, will be considered as description. We assume that data values are always on one line, but descriptions can take more than one line. In that case the argument `desc_above` is used to determine if a description line is coupled with a data value below or above it.
#' @seealso [scanner_functions] , [cleanup_bw()] and [scan_with_hocr()]

#' @examples
#' \dontrun{
#' # example1: header definition in list
#' hdr_desc = list(
#'   list(c(2,1),c(3,1)),
#'   list(c(2,2),c(3,2)),
#'   list(c(2,3),c(3,3)),
#'   list(c(2,4),c(2,5),c(3,4))
#' )
#' df2= extract_table(df1,
#'    headers=dhr_desc, lastline = Inf, desc_above=T)
#'
#' # example2: header definition in column header_col
#' df1= edit(df1) # change header_col
#' df2= extract_table(df1,
#'    headers=NULL,lastline = Inf, desc_above=T)
#'
#' }

extract_table <- function (df, headers=NULL, lastline = Inf, desc_above=T) {
  `%>%` <- magrittr::`%>%`
  if (is.null(headers))
    headers = c("header_col","header_col_seq")
  if (is.character(headers) && length(headers) !=2)
    stop('extract_table: "headers" is character but not of length 2')
  else if (is.character(headers) && length(headers) ==2)
    df_header = find_header_data2(df,headers)
  else if (is.list(headers))
    df_header = purrr::map_dfr(headers,  ~ find_header_data1(df, .))
  else
    stop('extract_table: wrong format for "headers"')

  df_header %>%
    dplyr::summarize(ml = max(ml2)) %>%
    as.numeric() -> lhl
  df = df %>%
    dplyr::filter(line > lhl & line <= lastline)

  header_intervals = as.vector(unlist(t(df_header[, c('x1', 'x2')])),
                               mode = 'integer')
  # determine in which interval a field is located
  a1 = findInterval((df$x1 + df$x2) / 2, header_intervals)
  # field is in a value column only (col>0) when a1 is uneven
  # col = 0 when before or between data columns col= -1 when after data columns
  df = df %>%
    dplyr::mutate(col = (a1 - 1) / 2 + 1,
                  col = dplyr::case_when((col %% 1) == 0.5 ~ 0 ,
                                         col > ncol(df_header) ~ -1,
                                         T ~ col)) %>%
    dplyr::select(line, fld_nr, word, col)
  # combine (paste) words with the same line/col combination
  df = df %>%
    dplyr::arrange(line, fld_nr, col, word) %>%
    dplyr::group_by(line, col) %>%
    dplyr::summarise(word = paste(trimws(word), collapse = " ")) %>%
    dplyr::ungroup()
  # select only the value fields and pivot over the 'col' column
  cnames = c('line', paste0('c', seq(1, ncol(df_header))))
  df2 = df %>%
    dplyr::filter(col > 0) %>%
    tidyr::pivot_wider(
      line,
      names_from = 'col',
      values_from = 'word',
      names_prefix = 'c' ) %>%
    dplyr::select(cnames)
  # lines with values
  lwv = df2$line
  # descriptions
  df3 = df %>%
    dplyr::filter(col == 0)
  # lines with descriptions
  lwd = df3$line
  # determine to which value row descriptions will go to
  if (desc_above == T)
    tovaluerow = lwv[1 + rowSums(outer(lwd, lwv, `>`))]
  else
    tovaluerow = lwv[rowSums(outer(lwd, lwv, `>=`))]
  df3 = df3 %>%
    dplyr::mutate(tovaluerow = tovaluerow) %>%
    dplyr::arrange(line, tovaluerow) %>%
    dplyr::group_by(tovaluerow) %>%
    dplyr::summarise(desc = paste(trimws(word), collapse = " "))
  df4 = df3 %>%
    dplyr::right_join(df2, by = c(tovaluerow = 'line')) %>%
    dplyr::select(-tovaluerow)
  names(df4) <- c('desc', df_header$t)
  df4
}

