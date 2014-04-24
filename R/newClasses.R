##' Poly data frame
##'
##' Data frame with unusual shape
##'
##' @param ... Objects to combine into a \code{poly.data.frame}
##' object.
##' @export
poly.data.frame <- function(...) {

    out <- list(...)
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

##' Species list
##'
##' @param x List of character vectors giving the names of species
##' present at various sites.
##' @param nms Optional names for the sites.
##' @param ... Not used.
##' @return \code{speciesList} object
##' @export
speciesList <- function(x, nms, ...) {
    if(!missing(nms)) names(x) <- nms
    class(x) <- "speciesList"
    return(x)
}

##' @S3method as.data.frame speciesList
##' @export
as.data.frame.speciesList <- function(x, ...) {
    lens <- sapply(x, length)
    data.frame(sites = rep(names(x), lens),
               species = unlist(x))
}

##' @S3method as.table speciesList
##' @export
as.table.speciesList <- function(x, ...)
    xtabs( ~ sites + species, as.data.frame(x))

##' @S3method as.matrix speciesList
##' @export
as.matrix.speciesList <- function(x, ...) {
    out <- unclass(as.table(x))
    attr(out, "call") <- NULL
    return(out)
}

##' @S3method print speciesList
##' @export
print.speciesList <- function(x, ...) {
    print("species list with presence-absence pattern:")
    as.table(x)
}
