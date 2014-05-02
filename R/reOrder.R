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
    ## FIXME: all this reOrder stuff is pretty DRY
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
    if(missing(X)) X <- dNames(x)
    ss(x, X)
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
                                        # indices
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
