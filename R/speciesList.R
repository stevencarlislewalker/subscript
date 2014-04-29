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
               species = unlist(x, use.names = FALSE))
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
print.speciesList <- function(x, full = FALSE,...) {
    uline <- function(n, s = "-") paste(rep(s, n), collapse = "")
    dn <- dNames(x)
    mainTitle <- "Species list"
    siteTitle <- "Sites"
    sppTitle <- "Species"
    cat("\n", mainTitle, ":\n", uline(nchar(mainTitle), "="), "\n\n")
    if(full) return(unclass(x))
    cat("\n", siteTitle, ":\n", uline(nchar(siteTitle)), "\n\n")
    print(dn[[1]])
    cat("\n", sppTitle, ":\n", uline(nchar(sppTitle)), "\n\n")
    print(dn[[2]])
    invisible(x)
}


##' Coerce to species list
##'
##' @param x an object
##' @return a \code{speciesList} object
##' @rdname as.speciesList
##' @export
as.speciesList <- function(x) {
    UseMethod("as.speciesList")
}

##' @rdname as.speciesList
##' @export
as.speciesList.default <- function(x) stop("not yet writen")

##' @rdname as.speciesList
##' @export
as.speciesList.matrix <- function(x) {
    xLogical <- lapply(as.data.frame(t(x)), as.logical)
    out <- lapply(xLogical, ss, x = colnames(x))
    class(out) <- "speciesList"
    return(out)
}

##' @rdname as.speciesList
##' @export
as.speciesList.data.frame <- function(x) as.speciesList(as.matrix(x))

