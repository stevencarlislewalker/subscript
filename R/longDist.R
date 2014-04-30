##' Distance matrix in long form
##'
##' @param x an object
##' @param ... not used
##' @return \code{dist.data.frame} object.  A data frame with three
##' columns, \code{row},\code{col}, and \code{dist}
##' @rdname longDist
##' @export
longDist <- function(x, ...) as.longDist(x)

##' @rdname longDist
##' @export
as.longDist <- function(x, ...) UseMethod("as.longDist")

##' @export
as.longDist.default <- function(x, ...) {
    x <- try(as.dist(x))
    if(inherits(x, "try-error"))
        stop("no method available for converting to longDist")
    as.longDist(x)
}

##' @export
as.longDist.longDist <- function(x, ...) x

##' @export
as.longDist.dist <- function(x, ...) {
    n <- attr(x, 'Size') # size of matrix
    nms <- dNames(x)[[1]]
    if(is.null(nms)) nms <- 1:n
    iRow <- rep(1:(n-1), (n-1):1) # row indices to keep
    iCol <- iRow + sequence((n-1):1) # col indices to keep
    out <- data.frame(row = nms[iRow], col = nms[iCol], dist = x[],
                      stringsAsFactors = FALSE)
    class(out) <- c("longDist", "data.frame")
    return(out)
}

##' @export
as.longDist.matrix <- function(x, ...) {
    if(!isSymmetric(x)) stop("only symmetric matrices can be converted to dist")
    as.longDist(as.dist(x))
}

##' @export
as.longDist.data.frame <- function(x, ...) as.longDist(dist(x))

##' @export
as.longDist.phylo <- function(x, ...) as.longDist(cophenetic(x))

##' @importFrom stats as.dist
##' @export
as.dist.longDist <- function(m, diag = FALSE, upper = FALSE) {
    stop("not done")
    nms <- dNames(m)[[1]]
}


##' @importFrom stats reorder
##' @export
reorder.longDist <- function(x, X, ...) {
                                        # convert object names to
                                        # numeric
    inds <- cbind(match(x$row, X),
                  match(x$col, X))
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
    out$row <- X[out$row]
    out$col <- X[out$col]

    class(out) <- c("longDist", "data.frame")
    return(out)
}

