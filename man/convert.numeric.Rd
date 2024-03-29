% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/oop.R
\name{convert}
\alias{convert}
\alias{convert.numeric}
\alias{convert.figure}
\title{Convert units.}
\usage{
convert(x, ...)

\method{convert}{numeric}(x, from = c("mm", "cm", "in"), to = c("mm", "cm", "in"), ...)

\method{convert}{figure}(x, to = c("mm", "cm", "in"), ...)
}
\arguments{
\item{x}{a numeric vector or a figure object.}

\item{...}{extra arguments passed to methods.}

\item{from}{the input unit.}

\item{to}{the target unit.}
}
\value{
a numeric vector after the unit conversion or
a `figure` class object with the unit information altered.
}
\description{
Convert units.
}
\details{
`convert()` is a S3 generic function with methods defined
for numeric vectors and the `figure` class.
}
\examples{
convert(10, 'mm', 'cm')
convert(180, 'mm', 'in')
convert(7.086614, 'in', 'mm')
}
