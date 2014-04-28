##' Convert non-integer subscript to integer
##'
##' @param i non-integer subscript (or list of non-integer
##'  subscripts for each dimension
##' @param ... additional info about the object
##' @return Subscripts in integer form
##' @export
conversion <- function(i, nms, ...) UseMethod('conversion')

##' @rdname conversion
##' @export
##' @method conversion default
conversion.default <- function(i, nms, ...) {
    if(!is.recursive(i)) return(i)
                                        # FIXME: testing for
                                        # complementary i and
                                        # nms
    return(mapply(conversion, i, nms, SIMPLIFY = FALSE))
}


##' @param nms list of dNames
##' @rdname conversion
##' @export
##' @method conversion character
conversion.character <- function(i, nms, ...){
    match(i, nms)
}

##' @rdname conversion
##' @export
##' @method conversion logical
conversion.logical <- function(i, nms, ...){
    stop("not finished")
}

##' @rdname conversion
##' @export
##' @method conversion matrix
conversion.matrix <- function(i, nms, ...){
    stop("not finished")
}
