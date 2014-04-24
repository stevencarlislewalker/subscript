\name{follows}
\alias{\%f\%}
\alias{follows}
\title{Follows}
\usage{
follows(f, g)

f \%f\% g
}
\arguments{
  \item{f}{a function of one variable}

  \item{g}{another function with range equal to the domain
  of \code{f}}

  \item{...}{additional arguments to \code{g}}
}
\value{
The function given by \code{f} follows \code{g}.
}
\description{
Composition of two functions.
}
\examples{
sqr <- function(x) x^2
min10 <- function(x) x - 10
(sqr \%f\% min10)(2)
}

