#' plot_PCA: replacement for [plot.PCA][FactoMineR::plot.PCA]
#'
#' plot_PCA is a replacement for [plot.PCA][FactoMineR::plot.PCA]. Out of this function I took the functionality that I need and added the functionality to emphasize points (variables or individuals).
#' The function plots the variables or individuals information from the structure returned by the function [PCA][FactoMineR::PCA].
#'
#' @name plot_PCA
#' @param res Structure that is the output of the function [PCA][FactoMineR::PCA]
#' @param var_ind Character string that indicates if variables (\code{var}) or individuals (\code{ind}) will be plotted.  Default: \code{"ind"}
#' @param axes Numeric vector with two integers indicating the dimensions that will be plotted.  Default: \code{1:2}
#' @param cex Positive number indicating the size of symbols and labels. Default: \code{1}
#' @param font.std Positive integer indicating the font that is used for the standard labels (i.e. not for supplemental, selected or emphasized elements). See description of \code{par()} for details: 1 plain text, 2 bold face, 3 italic and 4 bold italic. Default: \code{1}
#' @param pch.std Positive integer indicating the plotting character that is used for the standard points (i.e. not for supplemental, selected or emphasized elements). See description of \code{points} for details: 0 open square, 1 open circle, 2 open triangle etc. Default: \code{20} (i.e. very small closed circle)
#' @param col.std Color specification for the standard elements (i.e. not for supplemental, selected or emphasized elements). See description of \code{par()} for details of specification. Default: \code{"black"}
#' @param font.emp Positive integer indicating the font that is used for the labels of emphasized elements. See description of \code{font.std}. Default: \code{2}
#' @param pch.emp Positive integer indicating the plotting character that is used for the emphasized points. See description of \code{pch.std}. Default: \code{pch.std}
#' @param col.emp Color specification for emphasized elements. See description of \code{col.std}. Default: \code{"red"}
#' @param font.sel Positive integer indicating the font that is used for the labels of selected elements. See description of \code{font.std}. Default: \code{font.std}
#' @param pch.sel Positive integer indicating the plotting character that is used for the selected points. See description of \code{pch.std}. Default: \code{pch.std}
#' @param col.sel Color specification for selected elements. See description of \code{col.std}. Default: \code{col.std}
#' @param font.sup Positive integer indicating the font that is used for the labels of supplemental elements. See description of \code{font.std}. Default: \code{3}
#' @param pch.sup Positive integer indicating the plotting character that is used for the supplemental points. See description of \code{pch.std}. Default: \code{1}
#' @param col.sup Color specification for supplemental elements. See description of \code{col.std}. Default: \code{"blue"}
#' @param show.sup Boolean indicating if supplemental elements should be shown. Default: \code{TRUE}
#' @param select Character string  with a selection criterion (see \code{details}) or an integer vector with the sequence numbers of the active elements that are selected (i.e. be given a different layout than the other elements). Default: NULL (i.e. all are selected)
#' @param emphasize Character vector with the rownames or an integer vector with the sequence numbers of the active elements that should be emphasized (i.e. be given a different layout than the other elements). Default: NULL (i.e. none are emphasized)
#' @param invisible Character vector with the rownames of the elements (active and supplemental) that are not shown. Default: NULL
#' @export
#' @section Details:
#' The select mechanism is taken over asis from the \code{FactoMineR::PCA} function. The labels of selected elements will be plotted.\cr The authors Francois Husson \url{husson@agrocampus-ouest.fr} and Jeremy Mazet describe it as follows:\cr\cr
#' The select argument can be used in order to select a part of the elements (individuals if you draw the graph of individuals, or variables if you draw the graph of variables) that are drawn. For example, you can use:\cr
#' \code{select = 1:5} and then the elements \code{1:5} are drawn.\cr
#' \code{select = c("name1","name5")} and then the elements that have the names \code{name1} and \code{name5} are drawn.\cr
#' \code{select = "coord 10"} and then the 10 elements that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.\cr
#' \code{select = "contrib 10"} and then the 10 elements that have the highest contribution on the 2 dimensions of your plot are drawn.\cr
#' \code{select = "cos2 5"} and then the 5 elements that have the highest cos2 on the 2 dimensions of your plot are drawn.\cr
#' \code{select = "dist 8"} and then the 8 elements that have the highest distance to the center of gravity are drawn.\cr\cr
#' NB. the following functionality is contained in \link[FactoMineR]{plot.PCA} but not described there:\cr
#' \code{select = "cos2 0.8"} and then the elements for which the sum of the cos2 on the 2 dimensions of your plot is greater than 0.8 are drawn.
#' @section Acknowledgements:
#' All ideas come from the creators of the [FactoMineR][FactoMineR::FactoMineR-package] package. This implementation is mine and if you find any errors, these will be mine. In case of errors try using the original [plot.PCA][FactoMineR::plot.PCA].
#' @examples
#' \dontrun{
#' plot_PCA(res,cex=0.8,show.sup = F,emphasize = 'BU03620304',sel='contrib 6')
#' }

plot_PCA <-
	function( 	res, 	var_ind = 'ind', axes = 1:2, 	cex = 1,
		font.std= 1, 	pch.std = 20,  	col.std = 'black',
		font.emp = 2, pch.emp = pch.std, col.emp = 'red',
		font.sel = font.std , 	pch.sel = pch.std , 	col.sel = col.std ,
		font.sup = 3, 	pch.sup = 1, 	col.sup = 'blue',  	show.sup = TRUE,
		select = NULL, emphasize = NULL, invisible = NULL )
	{
		# replacement for FactoMineR::plot.PCA
		is_inst <- function(pkg) {
			# https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages
			# answer by Artem Klevtsov
			nzchar(system.file(package = pkg))
		}
		#if (requireNamespace(FactoMineR, quietly = TRUE) == FALSE) {
		if (!is_inst('FactoMineR')) {
			cat('\nYou probably need to install FactoMiner:\n')
			cat('  the first argument is the result of FactoMineR::PCA or similar\n')
			cat('  and FactoMineR::autoLab is used in this function\n')
		}
		is_inst <- function(pkg) {
			nzchar(system.file(package = pkg))
		}

		act = purrr::pluck(res, var_ind)
		sup = purrr::pluck(res, glue::glue("{var_ind}.sup"))
		all_nrs = 1:nrow(act$coord)
		all_nms = rownames(act$coord)

		if (!is.null(emphasize)) {
			if (is.character(emphasize))
				emp_nrs  = match(emphasize, all_nms)
			else
				emp_nrs  = emphasize
		} else
			emp_nrs = c()

		sel_nrs = plot_PCA_select(res, var_ind = var_ind, select = select, axes = axes)

		x2 = act$coord[, axes[1]]
		y2 = act$coord[, axes[2]]
		t2 = all_nms
		f2 = rep(font.std, length(x2))
		p2 = rep(pch.std, length(x2))
		c2 = rep(col.std, length(x2))
		f2[emp_nrs] = font.emp
		p2[emp_nrs] = pch.emp
		c2[emp_nrs] = col.emp
		t2[ (!all_nrs  %in% sel_nrs) & (!all_nrs  %in% emp_nrs)] = ''
		f2[ (all_nrs  %in% sel_nrs) & (!all_nrs  %in% emp_nrs)] = font.sel
		p2[ (all_nrs  %in% sel_nrs) & (!all_nrs  %in% emp_nrs)] = pch.sel
		c2[ (all_nrs  %in% sel_nrs) & (!all_nrs  %in% emp_nrs)] = col.sel

		if (!is.null(sup) && show.sup ==T) {
			x3 = sup$coord[, axes[1]]
			y3 = sup$coord[, axes[2]]
			t3 = rownames(sup$coord)
			f3 = rep(font.sup, length(x3))
			p3 = rep(pch.sup, length(x3))
			c3 = rep(col.sup, length(x3))

			x2 = c(x2, x3)
			y2 = c(y2, y3)
			t2 = c(t2, t3)
			f2 = c(f2, f3)
			p2 = c(p2, p3)
			c2 = c(c2, c3)
		}

		x2 = x2[ !t2  %in% invisible ]
		y2 = y2[ !t2  %in% invisible ]
		t2 = t2[ !t2  %in% invisible ]
		f2 = f2[ !t2  %in% invisible ]
		p2 = p2[ !t2  %in% invisible ]
		c2 = c2[ !t2  %in% invisible ]

		xlab = glue::glue("Dim {a1} ({a2}%)",
			a1 = axes[1],
			a2 = sprintf("%.2f", res$eig[axes[1], 2]))
		ylab = glue::glue("Dim {a1} ({a2}%)",
			a1 = axes[2],
			a2 = sprintf("%.2f", res$eig[axes[2], 2]))
		main = "Individuals factor map (PCA)"

		graphics::plot(	x2, 	y2, 	pch = p2, 	cex = cex, 	col = c2,
			main = main, 	xlab = xlab, 	ylab = ylab )
		FactoMineR::autoLab( x2, 	y2, labels = t2,
			cex = cex, 	col = c2, font = f2, 	shadotext = TRUE )
		graphics::abline(v = 0, lty = 2)
		graphics::abline(h = 0, lty = 2)
	}

#' plot_PCA_select: auxiliary function for \link{plot_PCA}
#'
#' \code{plot_PCA} is a replacement for [plot.PCA][FactoMineR::plot.PCA]. \code{plot_PCA_select} handles the selection part for \code{plot_PCA}: determines for which of the active elements the labels (names) will be printed. Restricting the number of these elements can be useful if the plot contains a lot of elements.
#'
#' @name plot_PCA_select
#' @param res Structure that is the output of the function [PCA][FactoMineR::PCA]
#' @param var_ind Character string that indicates if variables (\code{var}) or individuals (\code{ind}) will be plotted.  Default: \code{"ind"}
#' @param axes Numeric vector with two integers indicating the dimensions that will be plotted.  Default: \code{1:2}
#' @param select Character string  with a selection criterion (see \code{details}) or an integer vector with the sequence numbers of the active elements that are selected (i.e. be given a different layout than the other elements). Default: NULL (i.e. all are selected)
#' @export
#' @section Details:
#' The select mechanism is taken over asis from the [PCA][FactoMineR::PCA] function. The labels of selected elements will be plotted.\cr The authors Francois Husson \url{husson@agrocampus-ouest.fr} and Jeremy Mazet describe it as follows:\cr\cr
#' The select argument can be used in order to select a part of the elements (individuals if you draw the graph of individuals, or variables if you draw the graph of variables) that are drawn. For example, you can use:\cr
#' \code{select = 1:5} and then the elements \code{1:5} are drawn.\cr
#' \code{select = c("name1","name5")} and then the elements that have the names \code{name1} and \code{name5} are drawn.\cr
#' \code{select = "coord 10"} and then the 10 elements that have the highest (squared) coordinates on the 2 chosen dimensions are drawn.\cr
#' \code{select = "contrib 10"} and then the 10 elements that have the highest contribution on the 2 dimensions of your plot are drawn.\cr
#' \code{select = "cos2 5"} and then the 5 elements that have the highest cos2 on the 2 dimensions of your plot are drawn.\cr
#' \code{select = "dist 8"} and then the 8 elements that have the highest distance to the center of gravity are drawn.\cr\cr
#' NB. the following functionality is contained in [plot.PCA][FactoMineR::plot.PCA] but not described there:\cr
#' \code{select = "cos2 0.8"} and then the elements for which the sum of the cos2 on the 2 dimensions of your plot is greater than 0.8 are drawn.
#' @section Acknowledgements:
#' All ideas come from the creators of the [FactoMineR][FactoMineR::FactoMineR-package] package. This implementation is mine and if you find any errors, these will be mine.
#' @examples
#' \dontrun{
#' plot_PCA_select(res,var_ind = 'ind',sel='contrib 6', axes = 1:2)
#' }

plot_PCA_select <- function (res, var_ind = 'ind', select = NULL, axes = 1:2) {
	# replacement for select part in plot.PCA of FactoMineR
	act = purrr::pluck(res, var_ind)
	sup = purrr::pluck(res, glue::glue("{var_ind}.sup"))
	selection <- NULL
	if (!is.null(select)) {
		if (mode(select) == "numeric")
			selection <- select
		else {
			if (sum(rownames(act$coord) %in% select) > 0)
				selection <- which(rownames(act$coord) %in% select)
			else {
				selects = stringr::str_match(select,
					"(cos2|coord|contrib|cor|dist) ([0123456789.]+)")
				selectg = selects[2]
				selectn1 = as.numeric(selects[3])
				selectn2 = min(as.integer(selects[3]), nrow(act$coord))
				if (selectg == "contrib")
					selection <- (rev(order(
						act$contrib[, axes[1], drop = FALSE] * res$eig[axes[1], 1] +
							act$contrib[, axes[2], drop = FALSE] * res$eig[axes[2], 1]
					)))	[1:selectn2]
				if (selectg == "cor"  && !is.null(act$cor))
					selection <- (rev(order(act$cor[, axes[1]] ^ 2 +
							act$cor[, axes[2]] ^ 2)))	[1:selectn2]
				if (selectg == "cor"  && is.null(act$cor))# no cor (for 'ind')
					selection <- 1:nrow(act$coord)
				if (selectg == "dist" && !is.null(act$dist))
					selection <- (rev(order(act$dist)))  [1:selectn2]
				if (selectg == "dist" && is.null(act$dist)) # no dist (for 'var')
					selection <- 1:nrow(act$coord)
				if (selectg == "coord")
					selection <- (rev(order(act$coord[, axes[1]] ^ 2 +
							act$coord[, axes[2]] ^ 2)))	[1:selectn2]
				if (selectg == "cos2")
					if (selectn1 >= 1)
						selection <-
					(rev(order(apply(act$cos2[, axes], 1, sum))))	[1:selectn2]
				else
					selection <- which(apply(act$cos2[, axes], 1, sum) > selectn1)
				if (is.integer(select))
					selection <- select
			}
		}
	} else {
		selection = 1:nrow(act$coord)
	}
	unname(selection)
}

# See https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html for info about links etc.
#' print_PCA: prints a part of \link[FactoMineR]{PCA} output (sorted)
#'
#' The output of the [PCA][FactoMineR::PCA] function of the [FactoMineR][FactoMineR::FactoMineR-package] package is an object that contains various matrices. The function [print.PCA][FactoMineR::print.PCA] of that package gives an overview of the available matrices. The function `print_PCA` displays a sorted version of one of these matrices. With this it is easier to check which variables or individuals play a large role for a certain dimension or pair of dimensions.
#' @name print_PCA
#' @param res Structure that is the output of the function \link[FactoMineR]{PCA}
#' @param var_ind Character string that indicates if output for variables (\code{var}) or individuals (\code{ind}) will be printed.  Default: \code{'ind'}
#' @param measure Character string with the name of the matrix. Possible values are  \code{'coord'}, \code{'cor'}, \code{'cos2'}, \code{'contrib'} and \code{'dist'}. Default: \code{'contrib'}
#' @param decreasing Boolean indicating that greatest values come first (TRUE) or smallest values come first (FALSE). Default: \code{TRUE}
#' @param absolute Boolean indicating that that absolute values will be sorted: TRUE means take absolute value and FALSE means leave value asis. (Only relevant for \code{measurer='coord'}). Default: \code{TRUE}
#' @param dim Integer (or vector of two integers) indicating the dimension(s) on which the matrix will be sorted. Default: \code{1}
#' @export
#' @examples
#' \dontrun{
#' print_PCA(res,var_ind = 'var',decreasing = T,
#'       dim=1,measure = 'coord',abs=T)
#' }

print_PCA <- function(res, 	var_ind = 'ind', 	measure = 'contrib',
	decreasing = T, absolute = T,	dim = 1)
{
	if (!inherits(res, "PCA"))
		stop("argument res is not a PCA object")
	cat("**Results for the Principal Component Analysis (PCA)**\n")
	cat(
		"The analysis was performed on",
		nrow(res$call$X), "individuals, described by",
		ncol(res$call$X),	"variables\n"
	)
	data = purrr::pluck(res, var_ind, measure)
	data2 = data[, dim]
	if (length(dim) == 2) {
		if (measure == "contrib")
			data2 = data2[, dim[1], drop = FALSE] * res$eig[dim[1], 1] +
				data2[, dim[2], drop = FALSE] * res$eig[dim[2], 1]
		if (measure %in% c("cor", "coord"))
			data2 = data2[, dim[1]] ^ 2 + data2[, dim[2]] ^ 2
		if (measure == "dist") {
			#data2 = data2
		}
		if (measure == "cos2")
			data2 = data2[, dim[1]] + data2[, dim[2]]
	}
	if (absolute == T)
		data2 = abs(data2)
	o1 = order(data2, decreasing = decreasing)
	data = data[o1, ]
	ao = glue::glue("{var_ind}_{measure}_dec/{decreasing}_abs/{absolute}_{dim}")
	attr(data, "origin") = as.character(ao)
	data
}






