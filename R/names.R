##' Set dimnames names
##'
##' @param x An object
##' @param value Identifiers for object dimensions
##' @return The object with dimension identifiers
##' @rdname dimIds
##' @export
`dimIds<-` <- function(x, value) {
    if(is.data.frame(x)) return(structure(x, dimIds = value))
    out <- try(names(dimnames(x)) <- value, silent = TRUE)
    if(inherits(out, "try-error")) {
        if(length(value) == length(dNames(x)))
            out <- structure(x, dimIds = value)
        else
            stop("unable to set names for dNames")
    }
    return(out)
}

##' @rdname dimIds
##' @export
setDimIds <- function(x, value) {
    dimIds(x) <- value
    return(x)
}

##' Get dimnames names
##'
##' @param x An object
##' @return Names of the dimnames of \code{x}
##' @rdname dimIds
##' @export
dimIds <- function(x) names(dimnames(x))



dimIdsExtract <- function(x, dn) {
    dnn <- attr(x, "dimIds")
    if(!is.null(dnn)) names(dn) <- dnn
    return(dn)
}

##' Extract subscript dimension names
##'
##' @param x Dimensioned object
##' @return character vector with dimension names
##' @export
dNames <- function(x) {
    UseMethod("dNames")
}

##' @S3method dNames default
##' @export
dNames.default <- function(x) {
    if(is.null(dn <- dimnames(x))) return(names(x))
    return(dn)
}

##' @S3method dNames data.frame
##' @export
dNames.data.frame <- function(x) dimIdsExtract(x, list(rownames(x)))

##' @S3method dNames dist
##' @export
dNames.dist <- function(x) dimIdsExtract(x, list(attr(x, "Labels")))

##' @S3method dNames phylo
##' @export
dNames.phylo <- function(x) dimIdsExtract(x, list(x$tip.label))

##' @S3method dNames speciesList
##' @export
dNames.speciesList <- function(x) dimIdsExtract(x,
                                              list(names(x),
                                                   (unique %f% unlist)(x)))

##' @S3method dNames poly.data.frame
##' @export
dNames.poly.data.frame <- function(x) {
    apply(sapply(dimIdsUnique(pdf), "==", dimIdsConcat(pdf)),
          2,
          unique %f% unlist %f% `[`,
          x = dNamesConcat(x))
}
