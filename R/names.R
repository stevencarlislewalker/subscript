##' Set and get dimension identifiers
##'
##' Dimension identifiers (\code{dimIds}) allow combining objects into
##' \code{\link{poly.data.frame}} objects, which are related along the
##' dimensions of each of their consitituent objects.  In order to
##' make use of these relationships, the dimensions of each object
##' must be identified, which is accomplished by \code{dimIds}.
##' 
##' \code{dimIds} are stored in two different ways:
##' \itemize{
##' \item For \code{data.frame} objects and everything else
##' without \code{dimnames}, \code{dimIds} are stored as an attribute.
##' \item For any object with \code{dimnames} except \code{data.frame}s,
##' \code{dimIds} are stored as the names of the list of \code{dimnames}.
##' }
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
        if(length(value) == length(dNames(x))) {
            out <- structure(x, dimIds = value)
        }
        else {
            stop("unable to set names for dNames")
        }
    } else {
        out <- x
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
    if(is.null(dn <- dimnames(x)))
        return(dimIdsExtract(x, list(names(x))))
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
dNames.longDist <- function(x) dimIdsExtract(x, list(union(x$row, x$col)))


##' @export
dNames.poly.data.frame <- function(x) {
                                        # the matrix being applied
                                        # here is a digraph matrix
                                        # relating all dimensions to
                                        # the dimIds
    out <- apply(sapply(dimIdsUnique(x), "==", dimIdsConcat(x)),

                                        # use this logical matrix to
                                        # pick out dimensions
                                        # associated with each dimIds,
                                        # and find the unique dNames
                 2,
                 unique %f% unlist %f% `[`,
                 x = dNamesConcat(x))

                                        # don't let dNames come out as
                                        # a matrix, because the
                                        # structure is artificial
    if(inherits(out, "matrix")) {
        out <- unclass(as.data.frame(out, stringsAsFactors = FALSE))
        attr(out, "row.names") <- NULL
    }

    return(out)
}


##' Number of dimensions
##'
##' @param x an object
##' @return number of dimensions in \code{x}
##' @export
nDims <- function(x) length(dNames(x))

