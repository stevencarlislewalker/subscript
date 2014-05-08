##' Species list
##'
##' @param x List of character vectors giving the names of species
##' present at various sites.
##' @param nms Optional names for the sites.
##' @param ... Not used.
##' @return \code{speciesList} object.  A list with one element for
##' each site giving a character vector of the names of the species
##' present at that site.  Also includes an attribute
##' \code{"totalSpeciesList"} with the list of all species over all
##' sites.
##' @export
speciesList <- function(x, nms, ...) {
    if(!missing(nms)) names(x) <- nms
    attr(x, "totalSpeciesList") <- unique(unlist(x, use.names = FALSE))
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
as.table.speciesList <- function(x, ...) {
    out <- xtabs( ~ sites + species, as.data.frame(x))
    out[, dNames(x)[[2]]] # preserve order in attr(., "totalSpeciesList")
}

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
##' @param ... additional parameters to various methods (e.g. for
##' class \code{data.frame})
##' @return a \code{speciesList} object
##' @rdname as.speciesList
##' @export
as.speciesList <- function(x, ...) {
    UseMethod("as.speciesList")
}

##' @rdname as.speciesList
##' @export
as.speciesList.default <- function(x, ...) stop("not yet writen")

##' @rdname as.speciesList
##' @export
as.speciesList.matrix <- function(x, ...) {
    xLogical <- lapply(as.data.frame(t(x)), as.logical)
    cn <- colnames(x)
    out <- list()
    for(i in names(xLogical)) out[[i]] <- cn[xLogical[[i]]]
    attr(out, "totalSpeciesList") <- unique(unlist(out, use.names = FALSE))
    class(out) <- "speciesList"
    return(out)
}

##' @rdname as.speciesList
##' @param siteCol Name of column indicating sites
##' @param speciesCol Name of column indicating species
##' @export
as.speciesList.data.frame <- function(x, siteCol, speciesCol, ...) {
    as.speciesList(as.matrix(x))
}


