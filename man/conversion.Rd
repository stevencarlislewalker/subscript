\name{conversion}
\alias{conversion}
\alias{conversion.character}
\alias{conversion.default}
\alias{conversion.logical}
\title{Convert non-integer subscript to integer}
\usage{
  conversion(i, ...)

  \method{conversion}{default} (i, ...)

  \method{conversion}{character} (i, nms, ...)

  \method{conversion}{logical} (i, d, ...)
}
\arguments{
  \item{i}{non-integer subscript (or list of non-integer
  subscripts for each dimension}

  \item{...}{additional info about the object}

  \item{nms}{list of dimnames}

  \item{d}{vector of the lengths of each dimension}
}
\value{
  Subscripts in integer form
}
\description{
  Convert non-integer subscript to integer
}

