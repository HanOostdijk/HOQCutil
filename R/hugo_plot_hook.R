#' Plot hook for Hugo environment
#'
#' This [knitr()] plot hook uses the options in a knitr code chunk to generate some
#' [Hugo](https://gohugo.io/) lines that after handling by Hugo processor will produce
#' HTML code to display an image produced by an knitr code chunk. The last section
#' of **Details** shows the HTML that can be generated.
#' With the default settings the image is wrapped in a `div` with class `hoqc-center`.
#' In the css of my Hugo template `hoqc-center` has the attributes `text-align: center` .
#'
#' After setting the hook with `knitr::knit_hooks$set` each time that an image is
#' generated the function is called with the filename and the list with chunk options.
#'
#' The function uses the chunk options:
#'
#'  - `hugoopts`. This is a list with parameters for he Hugo shortcode `figure` .
#'  Possible parameters (no defaults) are:
#'
#'    -    `link` : 'href' to navigate to when image is clicked
#'    -    `target` : 'target' to use when image is clicked. E.g. '_blank'
#'    -    `rel` : 'rel' to use when image is clicked. E.g. 'nofollow'
#'    -    `src` : 'url' of generated image
#'    -    `alt` : 'alt-text' for image
#'    -    `caption` : 'caption' to place under image
#'    -    `width` : 'width' of the image
#'    -    `height` :  'height' of the image
#'    -    `attr` : 'text' of the 'caption' link
#'    -    `attrlink` : 'url' of the 'caption' link
#'  - `hugowrapin` : start of wrapping the {{figure}} shortcode. Default
#'  is `<div class="hoqc-center" >`
#'  - `hugowrapout`: start of wrapping the {{figure}} shortcode. Default
#'  is `</div >`
#'
#'  Example of generated HTML:
#' ```
#' ```   hugoopts= list(link="https://www.hanoostdijk.nl",target="_blank",rel="nofollow",
#' ```               alt="alt-text",caption="caption-text",width="500",
#' ```               attrlink="https://www.hanoostdijk.nl",attr="click here")
#'results after Hugo processing in the following HTML:
#' ```  <div class="hoqc-center" >
#' ```    <figure>
#' ```      <a href="https://www.hanoostdijk.nl" target="_blank" rel="nofollow">
#' ```        <img src="index_files/figure-html/index-8-1.png"
#' ```             alt="alt-text" width="500"/>
#' ```      </a>
#' ```      <figcaption>
#' ```        <p>caption-text <a href="https://www.hanoostdijk.nl">click here</a></p>
#' ```      </figcaption>
#' ```    </figure>
#' ```  </div >
#'
#' @name hugo_plot_hook
#' @param x Character string with the filename of a generated image
#' relative with respect to the code folder
#' @param options List with the chunk options
#' @return Character string to be processed by Hugo to create picture
#' @section Acknowledgement:
#' started with the idea from https://ropensci.org/technotes/2020/04/23/rmd-learnings/
#' @export
#' @examples
#' \dontrun{
#' to define the hook:
#'```{r setup}
#'knitr::knit_hooks$set(plot = hugo_plot_hook)
#'```
#'to make use of it:
#'```{r hugoopts= list(alt="alt-text",caption="caption-text",width="500") }
#'plot(1:3,(3:1)^2)
#'```
#'}

hugo_plot_hook <- function (x, options) {
  hugoopts <- options$hugoopts
  hugowrapin <- options$hugowrapin
  hugowrapout <- options$hugowrapout
  if (is.null(hugowrapin)) {
    hugowrapin <- '<div class="hoqc-center" > '
  }
  if (is.null(hugowrapout)) {
    hugowrapout <- ' </div > '
  }
  paste0(
    "{",
    "{< rawhtml >}}",
    hugowrapin,
    "{",
    "{< /rawhtml >}}\n",
    "{",
    "{<figure src=",
    '"',
    x,
    '" ',
    if (!is.null(hugoopts)) {
      glue::glue_collapse(glue::glue('{names(hugoopts)}="{hugoopts}"'),
                          sep = " ")
    },
    ">}}\n" ,
    "{",
    "{< rawhtml >}} ",
    hugowrapout,
    "{",
    "{< /rawhtml >}}\n"
  )
}
