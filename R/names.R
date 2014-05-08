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
    if(is.poly.data.frame(x))
        stop("cannot set dimIds for poly data frames, ",
             "as these come from the dimIds of the ",
             "constituent objects")
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

##' Estimate and set pattern of dimension sharing
##'
##' Uses \code{\link{kmeans}} clustering on the first axis of a
##' correspondence analysis (\code{\link{corresp}}) of the union of
##' all \code{\link{dNames}} against variables
##' 
##' @param x A \code{\link{list}} object
##' @param nDims Number of dimensions into which variables are to be
##' clustered
##' @param verb print seriated table?
##' @return \code{x} with \code{\link{dimIds}}
##' @importFrom MASS corresp
##' @export
calcDimIds <- function(x, nDims, verb = FALSE) {
                                        # what are the levels of the
                                        # dimensions of the
                                        # constituent objects?
    dnc <- dNamesConcat(x)
                                        # convert this list of level
                                        # names into a
                                        # dimensions-by-levels
                                        # incidence matrix
    xTable <- as.table(speciesList(dnc))
                                        # compute a correspondence
                                        # analysis of this incidence
                                        # matrix
    ca <- corresp(xTable)
                                        # seriate the matrix according
                                        # to the correspondence
                                        # analysis
    seriate <- xTable[order(ca$rscore), order(ca$cscore)]
    if(verb) {
        dimIds(seriate) <- c("dimensions","levels")
        print(seriate)
    }
                                        # cluster dimensions based on
                                        # the correspondence analysis
                                        # -- clusters estimate
                                        # dimension sharing
    clusts <- kmeans(ca$rscore, nDims)$cluster[names(dnc)]
                                        # name these clusters -- which
                                        # are the dimIds
    ids <- setNames(paste("D", clusts, sep = ""), names(clusts))
                                        # append these dimIds to the
                                        # appropriate variables by
                                        # moving from nested to
                                        # concatenated form
    index <- concat2NestedIndex(x)
    idList <- tapply(ids, index, unname)[names(x)]
    mapply(setDimIds, x, idList)
}


## makes things slow, but avoids DRY, and keeps this decision in one
## place.  'this decision' being whether to look for dimIds in the
## attributes or as names of characteristic names, dn.  FIXME: perhaps
## just put a dimIds attribute on every object ??
dimIdsExtract <- function(x, dn) {
    ## x  -- an object
    ## dn -- characteristic names of object dimensions
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
dNames.data.frame <- function(x) {

    ## dimIdsExtract(x, list(rownames(x)))
    
}


##' @export
dNames.dist <- function(x) dimIdsExtract(x, list(attr(x, "Labels")))


##' @export
dNames.phylo <- function(x) dimIdsExtract(x, list(x$tip.label))


##' @export
dNames.speciesList <- function(x) {
    dimIdsExtract(x, list(names(x), attr(x, "totalSpeciesList")))
}


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

