% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-mathematical-funcs.R
\name{mathematics}
\alias{mathematics}
\alias{sum.deb_lsd}
\alias{mean.deb_lsd}
\alias{cumsum.deb_lsd}
\alias{round.deb_lsd}
\alias{signif.deb_lsd}
\alias{ceiling.deb_lsd}
\alias{floor.deb_lsd}
\alias{trunc.deb_lsd}
\title{Mathematical functions with deb_lsd vectors}
\usage{
\method{sum}{deb_lsd}(..., na.rm = FALSE)

\method{mean}{deb_lsd}(x, ..., na.rm = FALSE)

\method{cumsum}{deb_lsd}(x)

\method{round}{deb_lsd}(x, digits = 0)

\method{signif}{deb_lsd}(x, digits = 6)

\method{ceiling}{deb_lsd}(x)

\method{floor}{deb_lsd}(x)

\method{trunc}{deb_lsd}(x, ...)
}
\arguments{
\item{...}{\code{deb_lsd} vectors in \code{sum()} and arguments passed on to
further methods in \code{mean()}.}

\item{na.rm}{Logical. Should missing values (including `NaN``) be removed?}

\item{x}{An object of class \code{deb_lsd}.}

\item{digits}{Integer. Indicating the number of decimal places
(\code{round()}) or significant digits (\code{signif()}) to be used.}
}
\description{
Mathematical functions with deb_lsd vectors
}
