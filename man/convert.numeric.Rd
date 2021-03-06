% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oop.R
\name{convert.numeric}
\alias{convert.numeric}
\title{Convert plot units.}
\usage{
\method{convert}{numeric}(x, from = c("mm", "cm", "in"), to = c("mm", "cm", "in"))
}
\arguments{
\item{x}{A numeric vector}

\item{from}{The input unit}

\item{to}{The target unit}
}
\value{
A numeric vector after the unit conversion
}
\description{
Convert plot units.
}
\examples{
convert(10, 'mm', 'cm')
convert(180, 'mm', 'in')
convert(7.086614, 'in', 'mm')
}
