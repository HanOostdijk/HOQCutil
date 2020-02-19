#' match first entry in lookup table
#'
#' Returns for the entries in a vector the first entry in the lookup table `lookup_df`
#' that matches the entry or in case of no-match a default. See **Details**
#'
#' @name mfe_lookup
#' @param datavec Character vector with elements that are to be matched in lookup table
#' @param lookup_df Data.frame containing a search and a result column
#' @param lookup_search_col Integer indicating the number of the search column (in `lookup_df`)
#' @param lookup_result_col Integer indicating the number of the result column (in `lookup_df`)
#' @param default Boolean or character string indicating the value to use when no match is found (See **Details**)
#' @param case_sensitive Boolean indicating if the match is case sensitive
#' @return Character vector with the same length as `datavec` with the result of the match
#' @section Details:
#' Each element in `datavec` is compared with the elements in the `lookup_search_col` column of the `lookup_df` data.frame. This is done with a expression of the form  `grepl(.,.,fixed=T)` where the first dot represents an element of the `lookup_search_col` column and the second dot the `datavec` element (both after capitalizing the character strings when `case_sensitive==F`). The element of the `lookup_search_col` column can contain more than one element and in that case for each element a match must be found. In the example below a match is found for the fourth element if the data element matches both `x3` and `x4`. If a match is found the corresponding element in the `lookup_result_col` column is returned and when more matches are found only the **first** is returned. So for the first element in `datavec` we return 'one' and not 'two': 'x1' matches **first** even when you could say that 'x1;x2' matches more. Therefore the more specified case should be specified first: see e.g. the 'x3;x4' case in the example. \cr
#' For an entry without a match will be returned:
#'
#' - the entry itself when `default == TRUE` (the default)
#' - the empty string when `default == FALSE`
#' - the contents of `default` when `default` is not a boolean
#' @export
#' @examples
#' \dontrun{
#' lookup_df = tibble::tribble(
#'   ~col1, ~col2,
#'   'x1' , 'one' ,
#'   'x1;x2' , 'two' ,
#'   'X2' , 'two' ,
#'   'x3;x4' , 'three four' ,
#'   'x3' , 'three'
#' )  %>%
#'   transform(col1 = strsplit(col1, ";"))
#'
#' datavec = c('x1 and x2', 'only x2', 'x5', 'x3 and x4','x3 and x5')
#'
#' mfe_lookup(datavec,lookup_df,default=TRUE)
#' # c("one", "two", "x5", "three four", "three" )
#'
#' mfe_lookup(datavec,lookup_df,default=FALSE)
#' # c("one", "two", "", "three four", "three" )
#'
#' mfe_lookup(datavec,lookup_df,default='not found!')
#' # c("one", "two", "not found!", "three four", "three" )
#'
#' mfe_lookup(datavec,lookup_df,case_sensitive=TRUE)
#' # c("one", "only x2", "x5", "three four", "three" )
#'
#' }
#'

mfe_lookup <-
  function (datavec,
            lookup_df,
            lookup_search_col=1,
            lookup_result_col=2,
            default = TRUE,
            case_sensitive = FALSE) {
    res = datavec 											# default is entry itself
    toupper2 <- function(x) {
      if (case_sensitive == FALSE) x = toupper(x)
      x
    }
    d   = purrr::map(toupper2(res),  # loop over vector elements
                     function(x) {
                       # find the first lookup_df entry that matches x
                       z = purrr::map_lgl(lookup_df[[lookup_search_col]], # loop over lookup table
                                          function(y) { # and loop over multiple conditions
                                            purrr::reduce(purrr::map(y,~grepl(toupper2(.),x,fixed = TRUE)),`&`)
                                          }
                       )
                       c(which(z == TRUE), 0)[1]
                     })
    d   = as.numeric(d)
    if (is.logical(default) && default == FALSE)
      res = rep("", length(res)) 				# default is empty string
    else if (!is.logical(default))
      res = rep(default, length(res))  	# default is function argument
    res[d > 0] = lookup_df[[lookup_result_col]][d[d > 0]] # replace by found match
    res
  }


