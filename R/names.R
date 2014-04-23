##' Set dimnames names
##' 
##' @export
dnamesNames <- function(x, nms) {
    if(is.data.frame(x)) return(structure(x, dnamesNames = nms))
    out <- try(names(dnames(x)) <- nms, silent = TRUE)
    if(inherits(out, "try-error")) {
        if(length(nms) == length(dnames(x)))
            out <- structure(x, dnamesNames = nms)
        else
            stop("unable to set names for dnames")
    }
    return(out)
}

## FIXME: maybe should use a different name??  dimnames already means
## something.  this is especially problematic with data frames, which
## use dimnames for rows and columns, even though in \pkg{subscript}
## land this doesn't work really.

dnamesNamesExtract <- function(x, dn) {
    dnn <- attr(x, "dnamesNames")
    if(!is.null(dnn)) names(dn) <- dnn
    return(dn)
}

##' Extract subscript dimension names
##'
##' @param x Dimensioned object
##' @return character vector with dimension names
##' @export
dnames <- function(x) {
    UseMethod("dnames")
}

##' @S3method dnames default
##' @export
dnames.default <- function(x) dimnames(x)

##' @S3method dnames data.frame
##' @export
dnames.data.frame <- function(x) dnamesNamesExtract(x, list(rownames(x)))

##' @S3method dnames dist
##' @export
dnames.dist <- function(x) dnamesNamesExtract(x, list(attr(x, "Labels")))

##' @S3method dnames phylo
##' @export
dnames.phylo <- function(x) dnamesNamesExtract(x, list(x$tip.label))

##' @S3method dnames speciesList
##' @export
dnames.speciesList <- function(x) dnamesNamesExtract(x, list(names(x), unique(unlist(x))))
