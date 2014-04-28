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

##' @export
as.data.frame.speciesList <- function(x, ...) {
    lens <- sapply(x, length)
    data.frame(sites = rep(names(x), lens),
               species = unlist(x))
}


##' @export
as.table.speciesList <- function(x, ...)
    xtabs( ~ sites + species, as.data.frame(x))


##' @export
as.matrix.speciesList <- function(x, ...) {
    out <- unclass(as.table(x))
    attr(out, "call") <- NULL
    return(out)
}


##' @export
print.speciesList <- function(x, ...) {
    cat("\nspecies list with presence-absence pattern:\n\n")
    print(as.table(x))
}
