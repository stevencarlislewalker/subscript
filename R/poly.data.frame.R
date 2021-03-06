##' Poly data frame
##'
##' Data frame with unusual shape
##'
##' @param ... Objects to combine into a \code{poly.data.frame}
##' object.
##' @param unique see \code{\link{make.names}}
##' @rdname poly.data.frame
##' @export
poly.data.frame <- function(..., unique = TRUE) {
                                        # uses data.frame idiom for
                                        # determining names
    object <- as.list(substitute(list(...)))[-1L]
    out <- list(...)
    vnames <- names(out)
    if(length(vnames) != (n <- length(out))) {
        vnames <- character(n)
    }
    no.vn <- !nzchar(vnames)
    vnames[no.vn] <- object[no.vn]
    names(out) <- vnames
    check.poly.data.frame(out)

    names(out) <- make.names(names(out), unique = unique)
    class(out) <- "poly.data.frame"
    return(out)
}

##' @param x An \code{R} object
##' @rdname poly.data.frame
##' @export
is.poly.data.frame <- function(x) inherits(x, "poly.data.frame")

##' @param nDims see \code{\link{calcDimIds}}
##' @param verb see \code{\link{calcDimIds}}
##' @rdname poly.data.frame
##' @export
as.poly.data.frame <- function(x, nDims, verb = FALSE) {
    UseMethod("as.poly.data.frame")
}

##' @rdname poly.data.frame
##' @export
as.poly.data.frame.default <- function(x, nDims, verb = FALSE) {
    if(is.data.frame(x)) return(x)
    if(!is.recursive(x)) stop("only list-like objects")
    out <- calcDimIds(x, nDims, verb)
    class(out) <- "poly.data.frame"
    return(out)
}


##' Summary of poly data frame
##'
##' @param object A \code{\link{poly.data.frame}} object.
##' @param ... Not used.
##' @return Logical matrix indicating replication relationships.
##' @export
summary.poly.data.frame <- function(object, ...){
    ## dUnique <- dimIdsUnique(object)
    ## dNested <- dimIdsNested(object)
    ## out <- sapply(dNested, "==", dUnique)
    ## rownames(out) <- dUnique
                                        # sorting is required to
                                        # eliminate order problem
    dimIdsList <- lapply(lapply(object, dimIds), sort)
    dimIdsVect <- sort(dimIds(object))
    out <- sapply(dimIdsList, "==", dimIdsVect)
    rownames(out) <- dimIdsVect
    return(out)
}


##' Print poly data frame
##'
##' @param x A \code{\link{poly.data.frame}} object.
##' @param full Should entire object be printed?
##' @param ... Not used
##' @export
print.poly.data.frame <- function(x, full = FALSE,...) {
    uline <- function(n, s = "-") paste(rep(s, n), collapse = "")
    mainTitle <- "Poly data frame"
    sumTitle <- "Relationships among variables"
    infoTitle <- "Info on variables"
    cat("\n", mainTitle, ":\n", uline(nchar(mainTitle), "="), "\n\n")
    if(full) return(unclass(x))
    cat("\n", sumTitle, ":\n", uline(nchar(sumTitle)), "\n\n")
    print(summary(x))
    cat("\n", infoTitle, ":\n", uline(nchar(infoTitle)), "\n\n")
    print(summary(unclass(x)))
    invisible(x)
}

##' Within a poly data frame
##'
##' @param data \code{\link{poly.data.frame}} object
##' @param expr an expression to evaluate within \code{data}
##' @param ... passed to \code{\link{within}}
##' @return \code{\link{poly.data.frame}} object
##' @export
within.poly.data.frame <- function(data, expr, ...) {
                                        # CURRENTLY EXPERIMENTAL
    
                                        # save for later
    nms <- dNames(data)
    ids <- names(nms) # dimIds but faster
    ## dimids <- names(attr(bm(data), "subsetdim"))
    
                                        # most of this code is just taken from within.list,
                                        # with my own annotations to try and understand it.
    
    parent <- parent.frame()
    
                                        # essentially this line converts the data list to
                                        # an 'environment'.  in particular it creates
                                        # an environment with the variables in data
                                        # as elements.  the subsetdim attribute remains
                                        # attached to each of those elements but the overall
                                        # attributes of the data list are gone (including
                                        # the names, and match.dimids, attributes)
    e <- evalq(environment(), data, parent)
    
                                        # evaluates the expr within the environment
                                        # created from the data list.
    eval(substitute(expr), e)
    
    data <- as.list(e)
    
                                        # here's the main change: instead of as.list (to
                                        # convert back to a list), use as.data.list.
                                        # as.data.list(data, dimids = dimids)
    chk <- try(check.poly.data.frame(data), silent = TRUE)
    if(inherits(chk, "try-error")) {
        ## concat2NestedIndex()
        ## nmsNew <- dNamesConcat(data)
        ## idsConcat <- apply(compareConcat(nmsNew, nms), 1, which.min)
        ids <- dimIdsUnique(data)
        data <- calcDimIds(data, length(ids))
        # index <- apply(compareNames(dNames(data), nms), 1, which.min)
        # ids[index]
    }
    class(data) <- "poly.data.frame"
    return(data)
}

##' Utility functions for poly.data.frame
##'
##' @param x A \code{\link{poly.data.frame}} object
##' @rdname utility
##' @export
dNamesNested <- function(x) lapply(x, dNames)

##' @rdname utility
##' @export
dNamesConcat <- function(x) do.call(c, dNamesNested(x))

##' @rdname utility
##' @export
dimIdsNested <- function(x) lapply(dNamesNested(x), names)

##' @rdname utility
##' @export
dimIdsConcat <- function(x) do.call(c, lapply(dNamesNested(x), names))

##' @rdname utility
##' @export
dimIdsUnique <- function(x) unique(dimIdsConcat(x))

##' @rdname utility
##' @export
concat2NestedIndex <- function(x) {
    ## returns a character vector that can be used as the index of a
    ## tapply call, where X in that call is from a *Concat function
    nDimsPerVariable <- sapply(dNamesNested(x), length)
    rep(names(x), nDimsPerVariable)
}

##' Compare names
##'
##' @param nms1 list of names vectors
##' @param nms2 list of names vectors
##' @return A \code{\link{matrix}} with rows representing list
##' elements of \code{nms1} and columns representing list elements of
##' \code{nms2}.  Entry \code{i, j} gives the number of elements that
##' are in \code{nms2[[j]]} but not in \code{nms1[[i]]} (in a
##' \code{setdiff(length(.))} way)
##' @seealso \code{\link{setdiff}}, \code{\link{length}}
##' @export
compareNames <- function(nms1, nms2){
    ## 
    n <- length(nms1)
    m <- length(nms2)
    met <- matrix(nrow = n, ncol = m)
    for(i in 1:n) for(j in 1:m)
        met[i, j] <- (length %f% setdiff)(nms2[[j]], nms1[[i]])
    dimnames(met) <- list(names(nms1), names(nms2))
    return(met)
}


## Internal functions
check.poly.data.frame <- function(x) {
    if(any(!sapply(dimIdsNested(x), length)))
        stop("\nall objects must have dimIds (dimension identifiers)\n",
             "which can be set using the setDimIds function")
}


compareStrings <- function(x, y) length(setdiff(x, y))/length(union(x, y))
