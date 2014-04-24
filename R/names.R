##' Set dimnames names
##'
##' @param x An object
##' @param ids Identifiers for object dimensions
##' @return The object with dimension identifiers
##' @export
dIds <- function(x, ids) {
    if(is.data.frame(x)) return(structure(x, dIds = ids))
    out <- try(names(dimnames(x)) <- ids, silent = TRUE)
    if(inherits(out, "try-error")) {
        if(length(ids) == length(dNames(x)))
            out <- structure(x, dIds = ids)
        else
            stop("unable to set names for dNames")
    }
    return(out)
}


dIdsExtract <- function(x, dn) {
    dnn <- attr(x, "dIds")
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
dNames.default <- function(x) dimnames(x)

##' @S3method dNames data.frame
##' @export
dNames.data.frame <- function(x) dIdsExtract(x, list(rownames(x)))

##' @S3method dNames dist
##' @export
dNames.dist <- function(x) dIdsExtract(x, list(attr(x, "Labels")))

##' @S3method dNames phylo
##' @export
dNames.phylo <- function(x) dIdsExtract(x, list(x$tip.label))

##' @S3method dNames speciesList
##' @export
dNames.speciesList <- function(x) dIdsExtract(x, list(names(x), unique(unlist(x))))

##' @S3method dNames poly.data.frame
##' @export
dNames.poly.data.frame <- function(x) {
    summaryConcat <- sapply(dIdsUnique(pdf), "==", dIdsConcat(pdf))
    fn <- function(i) unique(unlist(dNamesConcat(x)[i]))
    apply(summaryConcat, 2, fn)
}
