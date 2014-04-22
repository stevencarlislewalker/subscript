#' Consistent subscripting
#' @docType package
#' @name subscript-package
NULL

#' Subscript
#' 
#' Generic function for subscripting data objects.
#' 
#' @param x An object with data to be subscripted.
#' @param ... Indices for the dimensions of replication of \code{x}.
#' @return A subscripted version of \code{x}.
#' @aliases ss subscript.default
#' @export
subscript <- function(x, i, ...) {
    UseMethod('subscript')
}

#' @rdname subscript
#' @export
ss <- function(x, i, ...) subscript(x, i, ...)

#' @rdname subscript
#' @S3method subscript default
#' @method subscript default
subscript.default <- function(x, i) {
    if(!is.recursive(i)) i <- list(i)
    do.call("[", c(list(x), i))
}

#' Subscript a data frame
#' 
#' Subscript a data frame by rows (i.e. replicates) only.
#' 
#' @param x A data frame.
#' @param i Indices for the rows.
#' @param ... Not used.
#' @return A subscripted data frame
#' @S3method subscript data.frame
#' @method subscript data.frame
#' @export
subscript.data.frame <- function(x, i, ...) x[i, ]


#' Subscript a distance matrix
#' 
#' Subscript a distance matrix as a single-dimensional object.
#' 
#' @param x A \code{\link{dist}} object.
#' @param i Indices for the objects.
#' @param ... Not used.
#' @S3method subscript dist
#' @return A subscripted \code{\link{dist}} object.
#' @export 
#' @method subscript dist
subscript.dist <- function(x, i, ...){
  
                                        # TODO: handle different kinds
                                        # of subscripting
                                        # (e.g. logical, character,
                                        # factor)
  
                                        # if(is.character(i))
                                        # stop('code not writen yet')
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(subscript::conversion)
    i <- eval(mc)
    # i <- conversion(i)
    
    n <- attr(x, 'Size') # size of matrix
    ir <- rep(1:(n-1), (n-1):1) # row indices to keep
    ic <- ir + sequence((n-1):1) # col indices to keep
    
    ii <- order((match(ir, i) * match(ic, i)), na.last = NA)
    out <- x[ii]
    
    attributes(out) <- attributes(x)
    attr(out, 'Size') <- length(i)
    if(is.null(attr(out, 'Labels'))) 
        attr(out, 'Labels') <- as.character(i)
    else
        attr(out, 'Labels') <- attr(x, 'Labels')[i]
    
    return(out)
    
}

#' Subscript a phylogenetic tree
#' 
#' Subscript the tips of a \code{\link{phylo}} object.
#' 
#' @param x A \code{\link{phylo}} object.
#' @param i Indices for the tips.
#' @param ... Not used.
#' @return A phylogenetic tree.
#' @S3method subscript phylo
#' @method subscript phylo
#' @export
subscript.phylo <- function(x, i, ...){
    mc <- match.call(expand.dots = FALSE)
    mc[[1]] <- quote(subscript::conversion)
    i <- eval(mc)
    
    inot <- setdiff(seq_len(Ntip(x)), i)
    drop.tip(x, inot)
}
