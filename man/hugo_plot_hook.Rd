% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hugo_plot_hook.R
\name{hugo_plot_hook}
\alias{hugo_plot_hook}
\title{Plot hook for Hugo environment}
\usage{
hugo_plot_hook(x, options)
}
\arguments{
\item{x}{Character string with the filename of a generated image
relative with respect to the code folder}

\item{options}{List with the chunk options}
}
\value{
Character string to be processed by Hugo to create picture
}
\description{
This \code{\link[=knitr]{knitr()}} plot hook uses the options in a knitr code chunk to generate some
\href{https://gohugo.io/}{Hugo} lines that after handling by Hugo processor will produce
HTML code to display an image produced by an knitr code chunk. The last section
of \strong{Details} shows the HTML that can be generated.
}
\details{
After setting the hook with \code{knitr::knit_hooks$set} each time that an image is
generated the function is called with the file name and the list with chunk options.

The function uses the chunk options:
\itemize{
\item \code{hugoopts}. This is a list with parameters for he Hugo shortcode \code{figure} .
Possible parameters (no defaults except for \code{caption} and \code{cappos}) are:
\itemize{
\item \code{class} : 'class' to use in HTML figure statement
\item \code{id} : 'id' to use in HTML figure statement
\item \code{link} : 'href' to navigate to when image is clicked
\item \code{target} : 'target' to use when image is clicked. E.g. '_blank'
\item \code{rel} : 'rel' to use when image is clicked. E.g. 'nofollow'
\item \code{src} : 'url' of generated image
\item \code{alt} : 'alt-text' for image
\item \code{caption} : 'caption' to place below or above image. (default \code{fig.cap} chunk option)
\item \code{cappos} : position of caption ('below' or 'above' with default 'below')
\item \code{width} : 'width' of the image
\item \code{height} :  'height' of the image
\item \code{attr} : 'text' of the 'caption' link
\item \code{attrlink} : 'url' of the 'caption' link
}
}

Example of generated HTML:

\if{html}{\out{<div class="sourceCode">}}\preformatted{```   hugoopts= list(link="https://www.hanoostdijk.nl",target="_blank",rel="nofollow",
```               alt="alt-text",caption="caption-text",width="500",
```               attrlink="https://www.hanoostdijk.nl",attr="click here")
results after Hugo processing in the following HTML:
```  <div class="hoqc-center" >
```    <figure>
```      <a href="https://www.hanoostdijk.nl" target="_blank" rel="nofollow">
```        <img src="index_files/figure-html/index-8-1.png"
```             alt="alt-text" width="500"/>
```      </a>
```      <figcaption>
```        <p>caption-text <a href="https://www.hanoostdijk.nl">click here</a></p>
```      </figcaption>
```    </figure>
```  </div >
}\if{html}{\out{</div>}}
}
\section{Acknowledgement}{

Started with the idea from https://ropensci.org/technotes/2020/04/23/rmd-learnings/ \cr
This function use the Hugo shortcode figureHOQC that is an adaption of Hugo shortcode
\href{https://github.com/gohugoio/hugo/blob/master/tpl/tplimpl/embedded/templates/shortcodes/figure.html}{figureHOQC}
}

\examples{
\dontrun{
to define the hook:
```{r setup}
knitr::knit_hooks$set(plot = hugo_plot_hook)
```
to make use of it:
```{r hugoopts= list(alt="alt-text",caption="caption-text",width="500") }
plot(1:3,(3:1)^2)
```
}
}
