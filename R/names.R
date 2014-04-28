##' Set and get dimnames names
##'
##' @param x An object
##' @param value Identifiers for object dimensions
##' @rdname dimIds
##' @export
dimIds <- function(x) names(dNames(x))


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

##' Estimate and set dimnames names
##'
##' Uses \code{\link{kmeans}} clustering on the first axis of a
##' correspondence analysis (\code{\link{corresp}}) of the union of
##' all dimnames against variables
##' 
##' @param x A \code{\link{list}} object
##' @param nDims Number of dimensions into which variables are to be
##' clustered
##' @return \code{x} with \code{\link{dimIds}}
##' @importFrom MASS corresp
##' @export
calcDimIds <- function(x, nDims) {
    ## if (!require(MASS))
    ##     stop("MASS package is required")
    dnc <- dNamesConcat(x)
    xTable <- as.table(speciesList(dnc))
    ca <- corresp(xTable)
    seriate <- xTable[order(ca$rscore), order(ca$cscore)]
    clusts <- kmeans(ca$rscore, nDims)$cluster[names(dnc)]
    ids <- setNames(paste("D", clusts, sep = ""), names(clusts))
    nDimsPerVariable <- rep(names(x), sapply(dNamesNested(x), length))
    idList <- tapply(ids, nDimsPerVariable, unname)[names(x)]
    mapply(setDimIds, x, idList)
}




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

##' @export
dNames.default <- function(x) {
    if(is.null(dn <- dimnames(x))) return(names(x))
    return(dn)
}

##' @export
dNames.data.frame <- function(x) dimIdsExtract(x, list(rownames(x)))


##' @export
dNames.dist <- function(x) dimIdsExtract(x, list(attr(x, "Labels")))


##' @export
dNames.phylo <- function(x) dimIdsExtract(x, list(x$tip.label))


##' @export
dNames.speciesList <- function(x) dimIdsExtract(x,
                                                list(names(x),
                                                     (unique %f% unlist)(x)))


##' @export
dNames.poly.data.frame <- function(x) {
    apply(sapply(dimIdsUnique(x), "==", dimIdsConcat(x)),
          2,
          unique %f% unlist %f% `[`,
          x = dNamesConcat(x))
}

