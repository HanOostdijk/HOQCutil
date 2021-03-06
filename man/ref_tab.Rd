% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defreftab.R
\name{ref_tab}
\alias{ref_tab}
\title{ref_tab}
\usage{
ref_tab(label_name, table = T, add_page = T, prefix = "in", def_text = "")
}
\arguments{
\item{label_name}{Name of label}

\item{table}{Boolean to indicate if the label is for table (T) or figure(F)}

\item{add_page}{Boolean to indicate if the reference should include the page}

\item{prefix}{Text to include before the reference}

\item{def_text}{Text to include when the output is not Latex}
}
\description{
ref_tab produces a reference to a label of a table or figure
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
ref_tab("lbltab1",table=T,prefix="")
}
}
\seealso{
\code{\link{def_tab}}
}
