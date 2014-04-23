##' @export
setDimids <- function(x, dimids) structure(x, dimids = dimids)

## FIXME: maybe should use a different name??  dimnames already means
## something

##' @S3method dimnames dist
##' @export
dimnames.dist <- function(x) attr(x, "Labels")

##' @S3method dimnames phylo
##' @export
dimnames.phylo <- function(x) x$tip.label

##' @S3method dimnames speciesList
##' @export
dimnames.speciesList <- function(x) list(names(x), unique(unlist(x)))
