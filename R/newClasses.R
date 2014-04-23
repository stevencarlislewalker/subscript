##' Poly data frame
##'
##' Data frame with unusual shape
##'
##' 
poly.data.frame <- function(..., dimids) {

    out <- list(...)
    

}


##' Species list
##'
##' @param x List of character vectors giving the names of species
##' present at various sites.
##' @param nms Optional names for the sites.
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
