% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defreftab.R
\name{def_tab}
\alias{def_tab}
\title{def_tab}
\usage{
def_tab(label_name = "", label_text = "")
}
\arguments{
\item{label_name}{Name of label. Default ''}

\item{label_text}{Text to which label is attached. Default ''}
}
\description{
def_tab produces a text with a label for a table or figure
}
\section{details}{

An example of use: \cr
Create a table with label \code{mylabel} \cr
\code{xtable(uf,caption=def_tab('mylabel','mycaption'))} \cr
and refer to it in a \code{knitr} text chunk with \cr
\code{}
}

\examples{
\dontrun{
# generate a reference to the table with label lbltabl1
def_tab('lbltab1',cap_tab1)
}
}
\seealso{
\code{\link{ref_tab}}
}
