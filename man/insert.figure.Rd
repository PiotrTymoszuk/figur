% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/oop.R
\name{insert}
\alias{insert}
\alias{insert.figure}
\alias{insert.mdtable}
\alias{insert.mdexpr}
\alias{insert.mdtext}
\alias{insert.mdlink}
\alias{insert.mdhtml}
\title{Insert a figure, data frame, R code chunk, URL link
or text into an R markdown file}
\usage{
insert(object, ...)

\method{insert}{figure}(
  object,
  file = NULL,
  html = FALSE,
  style_ref = "legend",
  append = TRUE,
  relative_dim = FALSE,
  ...
)

\method{insert}{mdtable}(
  object,
  file = NULL,
  format = c("flextable", "kable"),
  rownames = FALSE,
  append = TRUE,
  ...
)

\method{insert}{mdexpr}(
  object,
  file = NULL,
  format = c("chunk", "inline"),
  include = NULL,
  echo = NULL,
  warning = NULL,
  message = NULL,
  append = TRUE,
  ...
)

\method{insert}{mdtext}(object, file = NULL, append = TRUE, ...)

\method{insert}{mdlink}(
  object,
  title = object$ref_name,
  html = FALSE,
  file = NULL,
  append = TRUE,
  ...
)

\method{insert}{mdhtml}(object, file = NULL, append = TRUE, ...)
}
\arguments{
\item{object}{an object to be referenced.}

\item{...}{extra arguments, currently none.}

\item{file}{a file to which the chunks are written. If the file exists
already, it will be appended or overwritten.
If NULL, the text is printed in the console and copied into the clipboard.}

\item{html}{logical: should the figure legend or link be HTML styled?}

\item{style_ref}{name of the CSS style of the legend text, valid only for
the HTML output.}

\item{append}{logical, should the output file be appended?}

\item{relative_dim}{logical, should the figure dimensions be inserted
as a call to an unit-converting function? If FALSE, fixed numeric dimensions
are inserted into the code chunk.}

\item{format}{which format the table should be inserted in?
'flextable' (default) or 'kable' available currently.}

\item{rownames}{logical, should the table rownames be included in the
markdown output? If TRUE, they are displayed in the first, unnamed column.
Relevant only for format = 'flextable'.}

\item{include}{the include option of the chunk, skipped if NULL.}

\item{echo}{echo chunk option, skipped if NULL.}

\item{warning}{warning chunk option, skipped if NULL.}

\item{message}{message chunk option, skipped if NULL.}

\item{title}{a string to be used as a link title, defaults to the `ref_name`
of the mdlink object.}
}
\value{
returns invisibly the requested R code chunk.
}
\description{
Inserts a reference to the figure, mdtable, mdexpr, mdhtml,
mdtext or mdlink object as an R code chunk into a R markdown file or prints
it into the standard output and, concomitantly, copies into the clipboard.
}
\details{
Only for the \code{\link{figure}}, \code{\link{mdtable}} class instances with
defined 'ref_name' and 'caption' parameters.
To enable the clipboard access, you may need to set the CLIPR_ALLOW
environment variable to TRUE, as described for
\code{\link[clipr]{write_clip}}.
For include, echo, message and warning chunk options, see:
https://rmarkdown.rstudio.com/lesson-3.html
}
