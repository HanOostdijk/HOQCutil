#' cap.out captures and wraps the output of an expression
#'
#' @name cap.out
#' @param cmd An expression or character vector
#' @param lines Line numbers to select from output. Default: all lines
#' @param numlines_only Boolean indicating no wrapping takes place and only the number of lines and the captured output are returned (in a list). Default: FALSE
#' @param se Start and End of each line (see details). Default: NULL (whole line)
#' @param width Position in each line where wrapping takes place. Default: \code{getOption('width',110)-3}
#' @param keep_empty Boolean indicating if empty should be kept. Default: FALSE
#' @param fixed_wrap Boolean indicating if wrapping takes place at a fixed position or takes into account word boundaries. Default: TRUE
#' @param abbr_ind Boolean indicating if abbreviations will be indicated with ...  . Default: TRUE
#' @param file A connection, or a character string naming the file to print to. If "" (the default), cat prints to the standard output connection, the console unless redirected by sink.
#' @param append Boolean. Only used if the argument file is the name of file (and not a connection or "|cmd"). If TRUE output will be appended to file; otherwise, it will overwrite the contents of file.
#' @export
#' @section details:
#'
#' Prints of XML documents can be very lengthy. Therefore I defined the function \code{cap.out} as a cover function of \code{capture.output}. With the argument \code{lines} one can specify the numbers of the lines that will be kept. For each line of output it can specified which part will be displayed. When the argument \code{se} is a 2-column matrix it specifies the start and end of the part. When \code{se} is an integer vector it gives the start position of the part when a number is negative and the end position when a number is postive. In those cases the part extends to the end or from the start of the line. When a number is 0, the line is not displayed. When \code{se} is not specified, all characters for all selected lines will be displayed.
#' To ensure that the specification of the lines and the positions match I will recycle the position  specification when necessary.
#' @section acknowledgements:
#' I was glad to be able to use the following (idea for) code :
#'
#' - the StackOverflow code to recycle argument by \href{https://stackoverflow.com/questions/9335099/implementation-of-standard-recycling-rules}{baptiste}.
#' - the idea to split long strings in parts I saw in a StackOverflow article by \href{https://stackoverflow.com/questions/32398301/fastest-way-to-split-strings-into-fixed-length-elements-in-r}{akrun}.
#' @section example description:
#' In the example I format an xml document.
#' Here I specify that I want to retrieve at most 1700 characters of these lines and in fact only will output the first 175 characters of each line with exception of the 6th one where I show 1650. I do not specify \code{lines=} but I know that the print of this document has 7 lines, so I could have added the argument \code{lines=1:7}. I also do not specify \code{width} and therefore use the default system option \code{getOption('width')}.
#' Both the \code{print} and the \code{cap.out} outputs indicate with \code{...} that a line is truncated; I ensure that the \code{width} of the \code{print} statement is a little greater than the greatest \code{se} to avoid outputs such as \code{'.. ...'} .
#' @examples
#' \dontrun{
#' # see description in details
#' cap.out( print(xml_doc,width=1700),se=c(175,175,175,175,175,1650,175) )
#' }


cap.out <- function (cmd,
	lines = NULL,
	numlines_only = F,
	se = NULL,
	width = getOption('width',110)-3,
	keep_empty = F,
	fixed_wrap = T,
	abbr_ind = T,
	file ="",
	append = FALSE) {
	# if cmd is not a character vector then first determine result of call
	type_cmd <- class(substitute(cmd))
	if (type_cmd == 'character') {
		results <- cmd
	} else {
		results <- capture.output(cmd, type = "output")
	}
	# determine number of lines of result
	numlines <- length(results)
	if (numlines_only == T) {
		res <- list('numl' = numlines, 'res' = results)
		return(res)
	}
	# determine which lines to extract
	if (is.null(lines))
		lines = seq_len(numlines)
	lines    <- unique(pmin(lines, numlines))
	results  <- results[lines]
	results  <- trimws(results, which = "right")
	numlines <- length(results) # number of lines to keep
	lresults <-
		stringr::str_length(results) # length of each of these lines
	# determine positions to extract
	if (is.null(se))
		se = lresults
	#ensure se has correct length !
	se2 <- seq_len(dim(as.matrix(se))[1]) # index vector for se
	# ensure se2 is not longer than results
	se2  <- se2[seq_len(pmin(length(se2), numlines))]
	# recycle se2 (in case it would be shorter)
	se2 <- expand_args(seq_len(numlines), se2)[[2]]
	if (!length(dim(se)) == 2) {
		se  <- se[se2] # recycle
		# se points to end position if >0 and else to start position
		cse <- sign(se) * pmin(lresults, abs(se))
		s = rep(1, length(cse))
		e = rep(-1, length(cse))
		s[cse  < 0] <- cse[cse < 0]
		e[cse >= 0] <- cse[cse >= 0]
	} else {
		se  <- se[se2,] # recycle
		s <- sign(se[, 1]) * pmin(lresults, abs(se[, 1]))
		e <- sign(se[, 2]) * pmin(lresults, abs(se[, 2]))
	}
	results <- stringr::str_sub(results, s, e)
	nresults <- stringr::str_length(results)
	# indicate abbreviations
	if (abbr_ind == TRUE) {
		abbr   <- (lresults != nresults) & (nresults != 0)
		results[abbr] <- stringr::str_c(results[abbr], " ...")
	}
	# remove empty lines
	if (keep_empty == FALSE){
		results <- purrr::keep(results, nresults > 0)
	} else {
		results[nresults == 0] <- " "
	}
	if (fixed_wrap == T) {
		cat(hard_split(results, width), sep = "\n",file=file,append=append)
	} else {
		cat(stringr::str_wrap(results, width), sep = "\n",file=file,append=append)
	}
}

expand_args <- function(...) {
	# recycles arguments
	# https://stackoverflow.com/questions/9335099/implem
	#    entation-of-standard-recycling-rules baptiste
	dots <- list(...)
	max_length <- max(sapply(dots, length))
	lapply(dots, rep, length.out = max_length)
}

#' hard_split splits the output of strings at a specific width
#'
#' Each string is split in pieces that have that length (or less for the last part of the string)
#' @name hard_split
#' @param strings A vector of strings
#' @param width The width (length) that each line will have (at most). Default \code{getOption('width',110)-3}
#' @export
#' @section acknowledgements:
#' I was glad to be able to use the following (idea for) code from a StackOverflow article by \href{https://stackoverflow.com/questions/32398301/fastest-way-to-split-strings-into-fixed-length-elements-in-r}{akrun}.
#' @examples
#'
#' hard_split(paste(letters,collapse =''),width=10)
#'

hard_split <- function(strings, width=getOption('width',110)-3) {
	regarg <- sprintf(".{1,%d}", width)
	strings1 = stringi::stri_extract_all_regex(strings, regarg)
	purrr::flatten_chr(strings1)
}

#' display_wrapped shows strings in the console
#'
#' Each string is split in pieces not exceeding a certain length by using `HOQCutil::hard_split` or `stringr::str_wrap`
#' @name display_wrapped
#' @param strings A vector of strings
#' @param width The width (length) that each line will have (at most).  Default \code{getOption('width', 110)-3}
#' @param force_wrap Boolean When TRUE forces the use of `stringr::str_wrap`. When FALSE `HOQCutil::hard_split` will be used when `HOQCutil` is available (which is of course the case unless this function is copied outside the package). Default: FALSE
#' @export
#' @examples
#' \dontrun{
#' x=glue::glue_collapse(rep(c(letters,' '),5))
#' display_wrapped(c(x,x),60)
#' display_wrapped(c(x,x),60,T)
#' }

display_wrapped <- function (strings,
	width = getOption('width', 110)-3,
	force_wrap = FALSE) {
	suppressWarnings({
		if (require('HOQCutil', quietly = TRUE) && force_wrap == FALSE) {
			cat(paste0(HOQCutil::hard_split(strings, width)),sep= "\n")
		} else {
			cat(stringr::str_wrap(strings, width), sep="\n")
		}
	})
}
