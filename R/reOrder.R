##' Reorder an object
##' 
##' Generic function for reordering the dimensions of data objects
##' according to a subscript list.  \code{ro} is a synonym for
##' \code{reOrder}.  See also \code{\link{reorder}}.
##' 
##' @param x An object with data to be reordered.
##' @param i Indices for the dimensions of replication of \code{x}.
##' @param ... Not used.
##' @return A reordered version of \code{x}.
##' @aliases ro reOrder.default
##' @seealso \code{\link{reorder}}
##' @export
reOrder <- function(x, i, ...) {
    if(missing(i)) i <- dNames(x)
    ## FIXME: all this reOrder stuff is pretty DRY wrt subscript.
    ##        however, could do something about this in methods that really do
    ##        just require a subscript
    ## FIXME: maybe check for subscripts that don't cover the entire range
    if(hasBeenProcessed(i)) {
        UseMethod("reOrder")
    } else {
        i <- processSubscript(x, i)
        reOrder(x, i, ...)
    }
}

##' @rdname reOrder
##' @export
ro <- function(x, i, ...) reOrder(x, i, ...)

##' @rdname reOrder
##' @export
reOrder.default <- function(x, i, ...) {
    if(!is.recursive(i)) i <- list(i)
    out <- do.call("reOrder", c(list(x), i))
    attr(out, "dimIds") <- attr(x, "dimIds")
    return(out)
}


##' @rdname reOrder
##' @export
reOrder.poly.data.frame <- function(x, i, ...) {
    if(missing(i)) i <- dNames(x)
    ss(x, i)
}



##' @importFrom stats reorder
##' @rdname reOrder
##' @export
reOrder.longDist <- function(x, i, ...) {
                                        # convert object names to
                                        # numeric
    inds <- cbind(match(x$row, i),
                  match(x$col, i))
                                        # get lower ranked names in
                                        # row's column
    rowSortedMat <- t(apply(inds, 1, sort))
                                        # intermediate form of output
    out <- setNames(data.frame(rowSortedMat, x$dist),
                    c("row","col","dist"))
                                        # sort by col and then row
                                        # indices, which gives an
                                        # order of the dist column
                                        # that is compatible with dist
                                        # objects
    out <- out[order(out$col),]
    out <- out[order(out$row),]
                                        # convert back from numeric to
                                        # character
    out$row <- i[out$row]
    out$col <- i[out$col]
    attr(out, "dimIds") <- attr(x, "dimIds")
    class(out) <- c("longDist", "data.frame")
    return(out)
}


##' @rdname reOrder
##' @export
reOrder.phylo <- function(x, i, ...) {
    # From http://ape-package.ird.fr/misc/FormatTreeR_24Oct2012.pdf:
    # "There is no mandatory order for the rows of edge, but they may
    # be arranged in a way that is efficient for computation and
    # manipulation."  Therefore, I take it that the code below
    # conforms to the phylo API.

    xNew <- x
    xNew$edge[match(1:Ntip(x), x$edge[,2]),2] <- match(x$tip.label, i)
    xNew$tip.label <- i[]

    attr(xNew, "dimIds") <- attr(x, "dimIds")
    return(xNew)
}
