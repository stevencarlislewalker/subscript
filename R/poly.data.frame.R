##' Poly data frame
##'
##' Data frame with unusual shape
##'
##' @param ... Objects to combine into a \code{poly.data.frame}
##' object.
##' @export
poly.data.frame <- function(...) {

    out <- list(...)
    if(any(!sapply(dIdsNested(out), length)))
        stop("\nall objects must have dIds (dimension identifiers)\n",
             "which can be set using the dIds function")
    class(out) <- "poly.data.frame"
    return(out)
}
    

##' Summary of poly data frame
##'
##' @param object A \code{\link{poly.data.frame}} object.
##' @param ... Not used.
##' @return Logical matrix indicating replication relationships.
##' @S3method summary poly.data.frame
##' @export
summary.poly.data.frame <- function(object, ...){
    dUnique <- dIdsUnique(pdf)
    dNested <- dIdsNested(pdf)
    out <- sapply(dNested, "==", dUnique)
    rownames(out) <- dUnique
    return(out)
}


##' Utility functions for poly.data.frame
##'
##' @param x A \code{\link{poly.data.frame}} object
##' @rdname utility
##' @export
dNamesNested <- function(x) lapply(x, dNames)

##' @rdname utility
##' @export
dNamesConcat <- function(x) do.call(c, dNamesNested(x))

##' @rdname utility
##' @export
dIdsNested <- function(x) lapply(dNamesNested(x), names)

##' @rdname utility
##' @export
dIdsConcat <- function(x) do.call(c, lapply(dNamesNested(x), names))

##' @rdname utility
##' @export
dIdsUnique <- function(x) unique(dIdsConcat(x))
