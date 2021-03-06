% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-casting.R
\name{cast-lsd}
\alias{cast-lsd}
\alias{deb_as_lsd}
\alias{deb_as_lsd.deb_lsd}
\alias{deb_as_lsd.deb_decimal}
\alias{deb_as_lsd.numeric}
\alias{deb_as_lsd.logical}
\title{Cast to \code{deb_lsd}}
\usage{
deb_as_lsd(x, ...)

\method{deb_as_lsd}{deb_lsd}(x, ...)

\method{deb_as_lsd}{deb_decimal}(x, ...)

\method{deb_as_lsd}{numeric}(x, bases = c(20, 12), ...)

\method{deb_as_lsd}{logical}(x, bases = c(20, 12), ...)
}
\arguments{
\item{x}{An object to coerce to \code{deb_lsd}.}

\item{...}{Arguments passed on to further methods.}

\item{bases}{Numeric vector of length 2 used to specify the bases for the
solidus or s and denarius or d units. Default is \code{c(20, 12)}, which
conforms to the most widely used system of 1 pound = 20 shillings and
1 shilling = 12 pence.}
}
\description{
Cast \code{x} to a \code{deb_lsd} vector.
}
