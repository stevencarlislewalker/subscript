##' Poly data frame
##'
##' Data frame with unusual shape
##'
##' @param ... Objects to combine into a \code{poly.data.frame}
##' object.
##' @rdname poly.data.frame
##' @export
poly.data.frame <- function(...) {

    out <- list(...)
    if(any(!sapply(dimIdsNested(out), length)))
        stop("\nall objects must have dimIds (dimension identifiers)\n",
             "which can be set using the setDimIds function")
    class(out) <- "poly.data.frame"
    return(out)
}

##' @param x An \code{R} object
##' @rdname poly.data.frame
##' @export
is.poly.data.frame <- function(x) inherits(x, "poly.data.frame")

##' @param nDims see \code{\link{calcDimIds}}
##' @rdname poly.data.frame
##' @export
as.poly.data.frame <- function(x, nDims) {
    UseMethod("as.poly.data.frame")
}

##' @rdname poly.data.frame
##' @export
as.poly.data.frame.default <- function(x, nDims) {
    if(is.data.frame(x)) return(x)
    if(!is.recursive(x)) stop("only list-like objects")
    out <- calcDimIds(x, nDims)
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
    dUnique <- dimIdsUnique(object)
    dNested <- dimIdsNested(object)
    out <- sapply(dNested, "==", dUnique)
    rownames(out) <- dUnique
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

