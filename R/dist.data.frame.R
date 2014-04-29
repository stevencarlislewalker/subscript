##' Distance matrix in long form
##'
##' @param x \code{\link{dist}} object
##' @param ... not used
##' @return \code{dist.data.frame} object.  A data frame with three
##' columns, \code{row},\code{col}, and \code{dist}
##' @export
dist.data.frame <- function(x, ...) {

    n <- attr(x, 'Size') # size of matrix
    nms <- dNames(x)[[1]]
    if(is.null(nms)) nms <- 1:n
    iRow <- rep(1:(n-1), (n-1):1) # row indices to keep
    iCol <- iRow + sequence((n-1):1) # col indices to keep
    out <- data.frame(row = nms[iRow], col = nms[iCol], dist = x[],
                      stringsAsFactors = FALSE)
    class(out) <- c("dist.data.frame", "data.frame")
    return(out)
}
