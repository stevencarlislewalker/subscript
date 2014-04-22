#' Convert non-integer subscript to integer
#'
#' @param i non-integer subscript (or list of non-integer
#'  subscripts for each dimension
#' @param ... additional info about the object
#' @return Subscripts in integer form
#' @export
conversion <- function(i, nms, ...) UseMethod('conversion')

#' @rdname conversion
#' @S3method conversion default
#' @method conversion default
conversion.default <- function(i, nms, ...) {
    if(!is.recursive(i)) return(i)
                                        # FIXME: testing for
                                        # complementary i and
                                        # dimnames(x)
    return(mapply(conversion, i, nms, SIMPLIFY = FALSE))
}


#' @param nms list of dimnames
#' @rdname conversion
#' @S3method conversion character
#' @method conversion character
conversion.character <- function(i, nms, ...){
    ## stop('code not yet writen')
    ## function(ii, nm) nm[ii]
    ## mapply(match, i, nms, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    match(i, nms)
}

#' @param d vector of the lengths of each dimension
#' @rdname conversion
#' @S3method conversion logical
#' @method conversion logical
conversion.logical <- function(i, nms, ...){
    fun <- function(ii, dd) c(which(ii), (1 + length(ii)):dd)
    mapply(fun, i, d, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

