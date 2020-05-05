#' igs Insert graphical statement
#'
#' generates html statement to insert graphics in a Hugo generated web document
#'
#' @name igs
#' @param filename Character string with the name of the image file
#' @param opts List with options to add to `img` statement. See Details
#' @param level Integer indicating the level where the image file is located. E.g. `level=2` indicates that the location of the image file is `../../filename` relative to the document
#' @return Character string with the generated html statement
#' @export
#' @section Details:
#' Especially useful when an externally created image has to be included . It can be used in a text chunk with e.g.
#' &#96;`​r igs(...)`&#96; . In `opts` can be specified all the options for the html `img` statement (with exception of `src`) :
#' `link`, `rel`, `alt`, `caption`, `width`, `height`, `attr` and `attrlink`
#' @section Acknowledgement:
#' Idea is from the article [Miscellaneous Wisdom about R Markdown & Hugo Gained from Work on our Website](https://ropensci.org/technotes/2020/04/23/rmd-learnings/)
#' by Maëlle Salmon
#' @examples
#' \dontrun{
#' igs("post/2020-05-04-dir_files/my.png",opts=list(width='75%',alt="my.png"))
#' # <p><img src="../../../../../post/2020-05-04-dir_files/my.png" width="75%" alt="my.png" /></p>
#' }

igs <- function (filename,opts=list(),level=5) {
  fn = paste0(c(rep("../",level),filename),collapse='')
  paste0(
    "<p><img src=",
    glue::glue('"{fn}" ') ,
    if (length(opts)>0) {
      glue::glue_collapse(
        glue::glue('{names(opts)}="{opts}"'),
        sep = " "
      )
    },
    " /></p>"
  )
}
