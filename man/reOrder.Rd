% Generated by roxygen2 (4.0.0): do not edit by hand
\name{reOrder}
\alias{reOrder}
\alias{reOrder.data.frame}
\alias{reOrder.default}
\alias{reOrder.dist}
\alias{reOrder.longDist}
\alias{reOrder.phylo}
\alias{reOrder.poly.data.frame}
\alias{reOrder.speciesList}
\alias{ro}
\title{Reorder an object}
\usage{
reOrder(x, i, ...)

ro(x, i, ...)

\method{reOrder}{default}(x, i, ...)

\method{reOrder}{data.frame}(x, i, ...)

\method{reOrder}{dist}(x, i, ...)

\method{reOrder}{phylo}(x, i, ...)

\method{reOrder}{speciesList}(x, i, ...)

\method{reOrder}{longDist}(x, i, ...)

\method{reOrder}{poly.data.frame}(x, i, ...)
}
\arguments{
\item{x}{An object with data to be reordered.}

\item{i}{Indices for the dimensions of replication of \code{x}.}

\item{...}{Not used.}
}
\value{
A reordered version of \code{x}.
}
\description{
Generic function for reordering the dimensions of data objects
according to a subscript list.  \code{ro} is a synonym for
\code{reOrder}.  See also \code{\link{reorder}}.
}
\seealso{
\code{\link{reorder}}
}

