% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oop.R
\name{insert.mdtable}
\alias{insert.mdtable}
\title{Insert a data frame chunk into an R markdown file}
\usage{
\method{insert}{mdtable}(
  object,
  file = NULL,
  format = c("flextable", "kable"),
  rownames = FALSE,
  append = TRUE,
  ...
)
}
\arguments{
\item{object}{an mdtable object (\code{\link{mdtable}}).}

\item{file}{a file to which the chunks are written. If the file exists
already, it will be appended or overwritten.
If NULL, the text is printed in the console and copied into the clipboard.}

\item{format}{which format the table should be inserted in?
'flextable' (default) or 'kable' available currently.}

\item{rownames}{logical, should the table rownames be included in the
markdown output? If TRUE, they are displayed in the first, unnamed column.
Relevant only for format = 'flextable'.}

\item{append}{logical, should the output file be appended?}

\item{...}{extra arguments, currently none.}
}
\description{
Inserts a reference to a data frame as an R code chunk into
as R markdown file or prints it into the standard output.
The input data frame needs to be a mdtable object (\code{\link{mdtable}}).
}
\details{
To enable the clipboard access, you may need to set the CLIPR_ALLOW
environment variable to TRUE, as described for
\code{\link[clipr]{write_clip}}.
}
