#' create_child_chunk
#'
#' \code{create_child_chunk} creates a temporary file that can be read into an \code{.Rmd} file as a child chunk.
#'
#' @name create_child_chunk
#' @param contents Character vector where each element is a line that will be included in the temporary file that will be created
#' @param tmp_dir Character string with the name of the folder that will contain the temporary file. Default: name of current folder
#' @return Character string with the full name of the temporary file
#' @export
#' @examples
#' \dontrun{
#' # snippet of .Rmd file
#'
#' ```{r echo=F}
#' contents = readLines_part('codefile.R')
#' tfile = create_child(contents)
#' ```
#'
#' ```{r child=tfile}
#' ```
#'
#' ```{r echo=F}
#' unlink(tfile)
#' ```
#' }

create_child_chunk <- function (
	 contents,
	 tmp_dir = '.')
{
	tfile <- fs::file_temp(pattern = 'tmpfile',
			tmp_dir = tmp_dir,
			ext = '.Rmd')
  tfile <- as.character(tfile)
  child_data =c("```{r eval=F,echo=T}",contents,"```")
  cat(child_data,file=tfile,sep='\n')
  tfile
}
