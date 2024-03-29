% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markdown_tools.R
\name{build_bookdown_ref}
\alias{build_bookdown_ref}
\title{Create an R markdown figure/table citation text.}
\usage{
build_bookdown_ref(ref_name, ref_type = c("table", "figure"))
}
\arguments{
\item{ref_name}{reference name.}

\item{ref_type}{reference type: table or figure.}
}
\value{
a citation text.
}
\description{
Generates a customized inline reference to a figure or table
with the bookdown standard.
}
