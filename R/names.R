##' Set dimnames names
##' 
##' @export
dimnamesNames <- function(x, nms) {
    out <- try(names(dimnames(x)) <- nms, silent = TRUE)
    if(inherits(out, "try-error")) {
        if(length(nms) == length(dimnames(x)))
            out <- structure(x, dimnamesNames = nms)
        else
            stop("unable to set names for dimnames")
    }
    return(out)
}

## FIXME: maybe should use a different name??  dimnames already means
## something.  this is especially problematic with data frames, which
## use dimnames for rows and columns, even though in \pkg{subscript}
## land this doesn't work really.

##' @S3method dimnames dist
##' @export
dimnames.dist <- function(x) {
                                        # FIXME: DRY, see below
    out <- list(attr(x, "Labels"))
    dnn <- attr(x, "dimnamesNames")
    if(!is.null(dnn)) names(out) <- dnn
    return(out)
}

##' @S3method dimnames phylo
##' @export
dimnames.phylo <- function(x) {
    out <- list(x$tip.label)
    dnn <- attr(x, "dimnamesNames")
    if(!is.null(dnn)) names(out) <- dnn
    return(out)
}

##' @S3method dimnames speciesList
##' @export
dimnames.speciesList <- function(x) {
    out <- list(names(x), unique(unlist(x)))
    dnn <- attr(x, "dimnamesNames")
    if(!is.null(dnn)) names(out) <- dnn
    return(out)
}
