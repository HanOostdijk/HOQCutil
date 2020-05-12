#' def_tab
#'
#' def_tab produces a text with a label for a table or figure
#'
#' @name def_tab
#' @param label_name Name of label. Default ''
#' @param label_text Text to which label is attached. Default ''
# @importFrom knitr is_latex_output
#' @export
#' @section details:
#' An example of use: \cr
#' Create a table with label \code{mylabel} \cr
#' \code{xtable(uf,caption=def_tab('mylabel','mycaption'))} \cr
#' and refer to it in a \code{knitr} text chunk with \cr
#' \code{`r ref_tab('mylabel',table=T,prefix="")`}
#' @seealso \code{\link{ref_tab}}
#' @examples
#' \dontrun{
#' # generate a reference to the table with label lbltabl1
#' def_tab('lbltab1',cap_tab1)
#' }

def_tab <- function (label_name='',label_text='')
{ if ( knitr::is_latex_output() ) {
	paste0(label_text,"\\label{table:",label_name,"}")
} else {""}
}

#' ref_tab
#'
#' ref_tab produces a reference to a label of a table or figure
#'
#' @name ref_tab
#' @param label_name Name of label
#' @param table Boolean to indicate if the label is for table (T) or figure(F)
#' @param add_page Boolean to indicate if the reference should include the page
#' @param prefix Text to include before the reference
#' @param def_text Text to include when the output is not Latex
# @importFrom knitr is_latex_output
#' @export
#' @section details:
#' An example of use: \cr
#' Create a table with label \code{mylabel} \cr
#' \code{xtable(uf,caption=def_tab('mylabel','mycaption'))} \cr
#' and refer to it in a \code{knitr} text chunk with \cr
#' \code{`r ref_tab('mylabel',table=T,prefix="")`}
#' @seealso \code{\link{def_tab}}
#' @examples
#' \dontrun{
#' # generate a reference to the table with label lbltabl1
#' ref_tab("lbltab1",table=T,prefix="")
#' }

ref_tab <- function (label_name,table=T,add_page=T,prefix='in',def_text='') {
	if (!knitr::is_latex_output()) return(def_text)
	if (table==T) {
		obj1 = 'Table' ;  obj2 = 'table' ;
	}  else {
		obj1 = 'Figure' ; obj2 = 'fig' ;
	}
	if (nchar(trimws(prefix)) == 0) {
		t = sprintf('%s&nbsp;\\ref{%s:%s}', obj1, obj2, label_name)
	} else {
		t = sprintf('%s&nbsp;%s&nbsp;\\ref{%s:%s}',prefix, obj1, obj2, label_name)
	}
	if (add_page == T) {
		t =paste0(t,' on page&nbsp;',sprintf('\\pageref{%s:%s}',obj2, label_name))
	}
	return(t)
}
