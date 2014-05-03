##' Distance matrix in long form
##'
##' @param x an object
##' @param ... not used
##' @return \code{dist.data.frame} object.  A data frame with three
##' columns, \code{row},\code{col}, and \code{dist}
##' @rdname longDist
##' @export
longDist <- function(x, ...) as.longDist(x, ...)

##' @rdname longDist
##' @export
as.longDist <- function(x, ...) UseMethod("as.longDist")

##' @export
as.longDist.default <- function(x, ...) {
    x <- try(as.dist(x))
    if(inherits(x, "try-error"))
        stop("no method available for converting to longDist")
    as.longDist(x, ...)
}

##' @export
as.longDist.longDist <- function(x, ...) x

##' @param norm normalize distances such that the maximum distance
##' equals one?
##' @param reOrder see \code{\link{reOrder.longDist}}
##' @param sortorder similar to \code{\link{reOrder}} but with sorted
##' \code{\link{dNames}}
##' @rdname longDist
##' @export
as.longDist.dist <- function(x,   norm = FALSE,
                               reOrder = FALSE,
                             sortorder = FALSE, ...) {
    n <- attr(x, 'Size') # size of matrix
    nms <- dNames(x)[[1]]
    if(is.null(nms)) nms <- 1:n
    iRow <- rep(1:(n-1), (n-1):1) # row indices to keep
    iCol <- iRow + sequence((n-1):1) # col indices to keep
    out <- data.frame(row = nms[iRow], col = nms[iCol], dist = x[],
                      stringsAsFactors = FALSE)
    if(norm) out$dist <- out$dist/max(out$dist)
    class(out) <- c("longDist", "data.frame")
    if(reOrder) out <- reOrder(out, nms)
    if(sortorder) out <- reOrder(out, sort(nms))
    return(out)
}

##' @export
as.longDist.matrix <- function(x, ...) {
    if(!isSymmetric(x)) stop("only symmetric matrices can be converted to dist")
    as.longDist(as.dist(x), ...)
}

##' @export
as.longDist.data.frame <- function(x, ...) as.longDist(dist(x), ...)

##' @export
as.longDist.phylo <- function(x, ...) as.longDist(cophenetic(x), ...)

##' @importFrom stats as.dist
##' @export
as.dist.longDist <- function(m, diag = FALSE, upper = FALSE) {
    m <- reOrder(m)
    dn <- dNames(m)[[1]]
    structure(m$dist[],
              Size = length(dn),
              Labels = dn,
              Diag = diag,
              Upper = upper,
              class = "dist")
}


##' @export
as.matrix.longDist <- function(x, ...) as.matrix(as.dist(x))

