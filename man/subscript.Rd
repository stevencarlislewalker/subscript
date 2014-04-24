\name{subscript}
\alias{ss}
\alias{subscript}
\alias{subscript.default}
\title{Subscript}
\usage{
subscript(x, i, ...)

ss(x, i, ...)

\method{subscript}{default}(x, i, ...)
}
\arguments{
  \item{x}{An object with data to be subscripted.}

  \item{i}{Indices for the dimensions of replication of
  \code{x}.}

  \item{...}{Not used.}
}
\value{
A subscripted version of \code{x}.
}
\description{
Generic function for subscripting data objects.  \code{ss}
is a synonym for \code{subscript}.
}

