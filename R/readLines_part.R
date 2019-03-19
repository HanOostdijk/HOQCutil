#' readLines_part
#'
#' \code{readLines_part} reads the part of a text file that is delimited by two lines. A use case can be a file that contains a number of functions. While making documentation it is of course possible to include the whole file in an \code{.Rmd} chunk. By using this function one can use a more granular approach. In the \code{Detail} section one sees an example where code sections are delimited by R comment lines that start with \code{###HOQC_start} (the first line of the section) and with \code{###HOQC_end} (the last line of the section. The last part of these lines can contain an arbitrary name but also e.g. the name of the function that is defined. It is also possible in this way to retrieve consecutive sections (see the second example) or the whole file (see fourth example).
#'
#' @name readLines_part
#' @param filename Character string with the name of the file
#' @param suffixes Character vector of length 1 or 2 with the suffixes of the delimitation lines or NULL, Default NULL
#' @param start_str Character string with the prefix of the start delimitation line
#' @param end_str Character string with the prefix of the end delimitation line
#' @param symbols Character string that starts and ends a delimitation line
#' @param warn Boolean value passed to [readLines][base::readLines]. See argument \code{warn} of [readLines][base::readLines] function. Default = FALSE
#' @return Character string with the selected part of the file.
#' @export
#' @section Details:
#' As  an example of use assume that we have a file \code{rlp.txt} that looks like this (without the added numbers)  \cr \cr
#'\code{01 # example file for readLines_part} \cr
#'\code{02 ###HOQC_start fun1###} \cr
#'\code{03 fun1 <- function () \{}\cr
#'\code{04 return(pi)} \cr
#'\code{05 \} } \cr
#'\code{06 ###HOQC_end fun1###} \cr
#'\code{07 ###HOQC_start fun2###} \cr
#'\code{08 fun2 <- function () \{}\cr
#'\code{09 return(exp(1))} \cr
#'\code{10 \} } \cr
#'\code{11 ###HOQC_end fun2###} \cr \cr
#'\strong{Examples:} \cr \cr
#'\code{readLines_part('rlp.txt','fun1')}  \cr The first delimitation line is determined by the regex created by concatenating \code{'^'}, \code{symbols}, \code{start_str} and the first of the \code{suffixes}:  \code{###HOQC_start fun1###} \cr The second delimitation line is determined by the regex created by concatenating \code{'^'}, \code{symbols}, \code{end_str} and the second of the \code{suffixes}. Because only one suffix is specified the second suffix is assumed to have the same value as the first. So the second delimitation line is  \code{###HOQC_end fun1###} . \cr
#'Therefore the lines 03 - 05 will be read (lines starting with the concatenation of \code{symbols} and \code{start_str} or \code{end_str} are always removed). \cr \cr
#'\code{readLines_part('rlp.txt',c('fun1','fun2'))}  \cr The first delimitation line is the same as in the first example. \cr The second delimitation line is determined in the same way as in the first example and becomes:  \code{###HOQC_end fun2###} . \cr
#'Therefore the lines 02 - 11 will be read and as in the first example the delimitation lines are removed and therefore the lines 03 - 05 and 08 - 10 remain. \cr \cr
#'\code{readLines_part('rlp.txt','part3')}  \cr Because only one suffix is specified the second suffix is supposed to have the same value as the first. The first delimitation line is then \code{###HOQC_start part3###} and the second delimitation line  becomes  \code{###HOQC_end part3###} . However the first delimitation line is not in the file and therefore we start from the first line. In this case also the second delimitation line is not in the file and therefore we end with the last line.\cr
#'Therefore the lines 01 - 11 will be read and after removing the delimitation lines only the lines  01 (this is not a delimitation line), 03 - 05 and 08 - 10 remain. \cr \cr
#'\code{readLines_part('rlp.txt')}  \cr In this case \code{suffixes} get the default value \code{NULL} and the contents of the whole file will be returned.\cr
#'Therefore the lines 01 - 11 will all be read and returned.

readLines_part <- function (
	filename,
	suffixes = NULL,
	start_str = 'HOQC_start',
	end_str = 'HOQC_end',
	symbols = '###',
	warn=FALSE)
{
	tekst <- readLines(filename, warn=warn)
	if (!is.null(suffixes)) {
		suffix1 <- suffixes[1]
		if (length(suffixes) == 2) {
			suffix2 = suffixes[2]
		} else {
			suffix2 = suffixes[1]
		}
		if (stringr::str_length(suffix1)>0)
			suffix1 = glue::glue(' {suffix1}')
		if (stringr::str_length(suffix2)>0)
				suffix2 = glue::glue(' {suffix2}')
		start_str  <- glue::glue('^{symbols}{start_str}')
		end_str    <- glue::glue('^{symbols}{end_str}')
		start_str1 <- glue::glue('{start_str}{suffix1}{symbols}')
		end_str2   <- glue::glue('{end_str}{suffix2}{symbols}')
		start_line <- stringr::str_which(tekst, start_str1)
		if (length(start_line) == 0) start_line <- 1
		end_line   <- stringr::str_which(tekst, end_str2)
		if (length(end_line) == 0) end_line <- length(tekst)
		linenrs    <- seq(from = start_line, to = end_line, by = 1)
		tekst      <- tekst[linenrs]
		hf         <- stringr::str_detect(tekst,
			glue::glue('{start_str}|{end_str}'))
		tekst      <- tekst[!hf]
	}
	tekst
}
