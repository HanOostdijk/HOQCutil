#' print data.frame in LaTeX
#'
#' This function adds to the print function of the `xtable` package by providing predefined `add.to.row` code for the `longtable` LaTeX package: 'continued on/from ...' and 'end of table' lines that can switched on or off. This functionality can only be used with the `longtable` environment. When `scalebox` is specified the `tabular` environment is used and `add.to.row` code is not available. If the `longtable` package is not available then set `scalebox=1` or another appropriate value
#'
#' @name pxtable
#' @param df data.frame that will be printed.
#' @param tablabel Character string that can be used to reference the table as `table:tablabel`. Default: ''
#' @param tabcap Character string that is as caption for the table. Default: ''
#' @param adef Character string with the default alignment. Default: "l"
#' @param ap Integer vector with the positions of the columns for which an alignment specification is given in `av`. Default: c()
#' @param av Character vector with aligment specifications for the positions in `ap`. E.g. with values as 'c', 'r', 'p{3cm}' . Default: c()
#' @param ddef Integer with the default number of digits to display. Default: 0
#' @param dp Integer vector with the positions of the columns for which an digit specification is given in `dv`. Default: c()
#' @param dv Integer vector with digit specifications for the positions in `dp`. Default: c()
#' @param tco Boolean indicating if 'continued on' should be displayed at the bottom of a page(only) when using the `longtable` package. Default: T
#' @param tcf Boolean indicating if 'continued from' should be displayed at the top of a page(only) when using the `longtable` package. Default: `tco`
#' @param te Boolean indicating if 'end of table' should be displayed at the end of the table (only) when using the `longtable` package. Default: F
#' @param newpage Boolean indicating if the table should start on a new page. Default: F
#' @param scalebox Positive number for scaling the table. Forces the `tabular` environment. See `xtable::print.xtable`. Default: NULL
#' @param include.colnames Boolean indicating if column names will be printed. See `xtable::print.xtable`. Default: T
#' @param colnames Character vector with column headers to use instead of field names. Default: NA
#' @param rotate.colnames Boolean indicating if column names will be printed vertically. See `xtable::print.xtable`. Default: T
#' @param include.rownames Boolean indicating if row names will be printed. See `xtable::print.xtable`. Default: F
#' @param sanitize.colnames.function Function used for nicer printing of column names. See `xtable::print.xtable`. Default: xtable::sanitize
#' @param booktabs Boolean indicating if LaTeX package `booktabs` will be used for formatting horizontal lines. See `xtable::print.xtable`. Default: F
#' @param ... Additional arguments that are passed to `xtable::print.xtable`.
#' @return  See `xtable::print.xtable` as this function prepares only its function arguments. Using this function in a `knitr` chunk with `results  ='markup'` will show the generated LaTeX text. With `results  ='asis'` the text is transfered to the intermediate output file and interpreted as LaTeX in the final pdf document.
#' @export
#' @examples
#' \dontrun{
#' df1 = data.frame(
#'  v1 = rep(c(1,NA),50),
#'  v2 = rep(c('R version 3.5.0', 'R is free software'),50),
#'  v3 = rep(c('Copyright (C) 2018', 'You are welcome'),50),
#'  v4 = rep(c('a','b'),50),
#'  v5 = rep(c(pi,NA),50),
#'  stringsAsFactors = F
#' )
#'
#' HOQCutil::pxtable  (df1,tablabel='tab1',tabcap='mycaption',
#' 	tablabel = "tab1", tabcap = "mycaption",
#' 	ap = 1:4, av = c("r", "p{6cm}", "p{6cm}", "c"), adef = "l",
#' 	dp = c(1, 5), dv = c(2, 3), ddef = 0, te = F,
#' 	include.rownames = T, rotate.rownames = T
#' }

pxtable <- function(df,
	tablabel = '',
	tabcap = '',
	adef = 'l',
	ap = c(),
	av = c(),
	ddef = 0,
	dp = c(),
	dv = c(),
	tco = T,
	tcf = tco,
	te = F,
	newpage = F,
	scalebox = NULL,
	include.colnames = T,
	colnames = NA,
	rotate.colnames = T,
	include.rownames = F,
	sanitize.colnames.function = xtable::sanitize,
	booktabs = F,
	...) {
	my_align = rep(adef, dim(df)[2])
	my_align[ap] = av
	my_digits = rep(ddef, dim(df)[2])
	my_digits[dp] = dv
	my_caption = HOQCutil::def_tab(tablabel, tabcap)

	if (!knitr::is_latex_output()) {
		ao =purrr::map_lgl(my_align,function(x) x %in% c('r','l','c'))
		if (purrr::some(ao, ~ isFALSE(.))){
		  warning('one or more alignment specifications changed!')
		  my_align = unlist(purrr::map_if(my_align,!ao,function(x) 'l') )
		}
		print(
			knitr::kable(
				df,
				digits = my_digits,
				col.names = colnames,
				row.names = include.rownames,
				align = my_align,
				caption = tabcap
			)
		)
		invisible(NULL)
	} else{
		if (!is.null(scalebox)) {
			floating = T
			tenv = "tabular"
			my_hline.after = c(0, nrow(df))
			add.to.row <- NULL

		} else {
			floating = F
			tenv = "longtable"
			my_hline.after = 0 # hline at nrow(df) handled by add.to.row
			nms = names(df)
			sanitize.colnames.function0 = sanitize.colnames.function
			sanitize.colnames.function1 <- function( x ) {
				colnames
			}
			if (!is.na(colnames) && length(colnames) == length(nms))
				sanitize.colnames.function0 = purrr::compose(
					sanitize.colnames.function,sanitize.colnames.function1
				)
			add.to.row <-
				format_addtorow(sanitize.colnames.function0(nms),
					dim(df)[1],
					include.colnames,
					include.rownames,
					rotate.colnames,
					booktabs,
					tcf,
					tco,
					te)
		}
		if (newpage) {
			cat('\\newpage')
		}
		print(
			x = xtable::xtable (
				df,
				caption = my_caption,
				align = c('l', my_align),
				digits = c(0, my_digits)
			),
			add.to.row = add.to.row,
			hline.after = my_hline.after,
			include.colnames = include.colnames,
			include.rownames = include.rownames,
			rotate.colnames = rotate.colnames,
			floating = floating,
			sanitize.colnames.function = sanitize.colnames.function0,
			tabular.environment = tenv,
			scalebox = scalebox,
			booktabs = booktabs,
			...
		)
		invisible(NULL)
	}
}

format_header <- function(names,
	include.colnames = T,
	include.rownames = T,
	sideways = F) {
	g = ''
	if (include.colnames) {
		if (sideways == T) {
			b = '\\begin{sideways}'
			e = '\\end{sideways}'
		} else {
			b = ''
			e = ''
		}
		g = glue::glue_collapse(glue::glue("{b} {names} {e}"), sep = ' & ')
		if (include.rownames) {
			g = paste0('  & ', g)
		}
	}
	g
}

format_addtorow <-
	function(nms,
		nr,
		include.colnames = T,
		include.rownames = F,
		rotate.colnames = include.colnames,
		booktabs = F,
		tcf = T,
		tco = tcf,
		te = tcf) {
		nrc <- length(nms)
		if (include.rownames) {
			nrc = nrc + 1
		}
		if (booktabs) {
			top = '\\toprule'
			mid = '\\midrule'
			bot = '\\bottomrule'
		} else {
			top = '\\hline'
			mid = '\\hline'
			bot = '\\hline'
		}
		if (tcf == T) {
			# format 'continued from previous page'
			tcf1 = glue::glue(
				"\\multicolumn{<nrc>}{l}",
				"{\\footnotesize \\tablename\\ \\thetable{} -- continued from previous page} \\\\",
				.open = '<',
				.close = '>'
			)
		} else {
			tcf1 = ''
		}
		if (tco == T) {
			# format 'continued on next page'
			tco1 = glue::glue(
				"\\multicolumn{<nrc>}{l}",
				"{\\footnotesize \\tablename\\ \\thetable{} -- continued on next page} \\\\",
				.open = '<',
				.close = '>'
			)
		} else {
			tco1 = ''
		}
		if (te == T) {
			# format 'end of table'
			te1 = glue::glue(
				"\\multicolumn{<nrc>}{l}",
				"{\\footnotesize end of \\tablename\\ \\thetable{} } \\\\",
				.open = '<',
				.close = '>'
			)
		} else {
			te1 = ''
		}

		command1 <- paste0(
			"\\endfirsthead\n",

			tcf1,
			" \n",
			format_header(
				nms,
				include.colnames = include.colnames,
				include.rownames = include.rownames,
				sideways = rotate.colnames
			),
			" \\\\ \n",
			mid,
			" \n",
			"\\endhead\n",

			bot,
			" \n",
			tco1,
			" \n",
			"\\endfoot\n",

			"\\endlastfoot\n"

		)

		command2 <- paste0(te1, "\n",bot,"\\\\","\n")
		if (!booktabs) {
			# remove the extra \hline when booktabs is not used
			command2 <- paste0(command2, "%")
		}
		add.to.row <- list()
		add.to.row$pos <- list(0, nr)
		add.to.row$command <- c(command1, command2)
		add.to.row
	}
