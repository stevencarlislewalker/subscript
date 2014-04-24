##' Consistent subscripting
##' @docType package
##' @import ape
##' @name subscript-package
##' @examples
##' library(subscript)
##' library(ape)
##' set.seed(1)
##'
##' n <- 8 # number of sites
##' m <- 5 # number of species
##'
##' env <- data.frame(x = rnorm(n))
##' coord <- data.frame(lat = rnorm(n), long = rnorm(n))
##' rownames(env) <- rownames(coord) <- letters[1:n]
##'
##' traits <- data.frame(z1 = rnorm(m), z2 = rnorm(m))
##' rownames(traits) <- LETTERS[1:m]
##' newick <- "((A,(B,E)),(D,C));"
##' tree <- chronos(compute.brlen(read.tree(text = newick)))
##'
##' slist <- speciesList(replicate(n, LETTERS[sample.int(m, sample.int(m, 1))]), letters[1:n])
##' slist <- speciesList(slist)
##'
##' pdf <- poly.data.frame(env    = dIds(env,         "sites"),
##'                        coord  = dIds(coord,       "sites"),
##'                        geog   = dIds(dist(coord), "sites"),
##'                        traits = dIds(traits,      "species"),
##'                        coph   = dIds(as.dist(cophenetic(tree)), "species"),
##'                        tree   = dIds(tree,        "species"),
##'                        slist  = dIds(slist,       c("sites", "species")))
##'
##' summary(pdf)
##'
##' dIdsNested(pdf)
##' dIdsConcat(pdf)
##' dIdsUnique(pdf)
##'
##' dNamesNested(pdf)
##' dNamesConcat(pdf)
##' dNames(pdf)
NULL

##' Subscript
##' 
##' Generic function for subscripting data objects.  \code{ss} is a
##' synonym for \code{subscript}.
##' 
##' @param x An object with data to be subscripted.
##' @param i Indices for the dimensions of replication of \code{x}.
##' @param ... Not used.
##' @return A subscripted version of \code{x}.
##' @aliases ss subscript.default
##' @export
subscript <- function(x, i, ...) {
    UseMethod('subscript')
}

##' @rdname subscript
##' @export
ss <- function(x, i, ...) subscript(x, i, ...)

##' @rdname subscript
##' @S3method subscript default
##' @method subscript default
subscript.default <- function(x, i, ...) {
    if(!is.recursive(i)) i <- list(i)
    do.call("[", c(list(x), i))
}

##' Subscript a data frame
##' 
##' Subscript a data frame by rows (i.e. replicates) only.
##' 
##' @param x A data frame.
##' @param i Indices for the rows.
##' @param ... Not used.
##' @return A subscripted data frame
##' @S3method subscript data.frame
##' @method subscript data.frame
##' @export
subscript.data.frame <- function(x, i, ...) x[unlist(i), , drop = FALSE]


##' Subscript a distance matrix
##' 
##' Subscript a distance matrix as a single-dimensional object.
##' 
##' @param x A \code{\link{dist}} object.
##' @param i Indices for the objects.
##' @param ... Not used.
##' @S3method subscript dist
##' @return A subscripted \code{\link{dist}} object.
##' @export 
##' @method subscript dist
subscript.dist <- function(x, i, ...){

                                        # convert subscript type to
                                        # numeric
    ## i <- unlist(i)
    ## mc <- match.call(expand.dots = FALSE)
    ## mc[[1]] <- quote(subscript::conversion)
    ## mc$nms <- dNames(x)[[1]]
    ## mc$x <- NULL
    ## i <- eval(mc, parent.frame(1L))
    i <- conversion(unlist(i), dNames(x)[[1]])
    
                                        # compute subscript
    n <- attr(x, 'Size') # size of matrix
    iRow <- rep(1:(n-1), (n-1):1) # row indices to keep
    iCol <- iRow + sequence((n-1):1) # col indices to keep
    iVector <- order((match(iRow, i) * match(iCol, i)), na.last = NA)
    out <- x[iVector]

                                        # fix up attributes
    attributes(out) <- attributes(x)
    attr(out, 'Size') <- length(i)
    if(is.null(attr(out, 'Labels'))) 
        attr(out, 'Labels') <- as.character(i)
    else
        attr(out, 'Labels') <- attr(x, 'Labels')[i]
    
    return(out)
    
}

##' Subscript a phylogenetic tree
##' 
##' Subscript the tips of a \code{\link{phylo}} object.
##' 
##' @param x A \code{\link{phylo}} object.
##' @param i Indices for the tips.
##' @param ... Not used.
##' @return A phylogenetic tree.
##' @S3method subscript phylo
##' @method subscript phylo
##' @export
subscript.phylo <- function(x, i, ...){
                                        # convert to numeric
    ## i <- unlist(i)
    ## mc <- match.call(expand.dots = FALSE)
    ## mc[[1]] <- quote(subscript::conversion)
    ## mc$nms <- dNames(x)[[1]]
    ## mc$x <- NULL
    ## i <- eval(mc, parent.frame(1L))
    i <- conversion(unlist(i), dNames(x)[[1]])

                                        # compute subscript
    inot <- setdiff(seq_len(Ntip(x)), i)
    drop.tip(x, inot)
}

##' Subscript a species list
##'
##' @param x \code{speciesList} object
##' @param i Subscript list
##' @param ... Not used
##' @return subscripted \code{specieslist}
##' @S3method subscript speciesList
##' @export
subscript.speciesList <- function(x, i, ...){
    ## i[[1]] <- conversion(i[[1]], names(x))
    x <- x[i[[1]]]
    out <- lapply(x, intersect, i[[2]])
    class(out) <- "speciesList"
    return(out)
}

##' Subscript a poly data frame
##'
##' @param x \code{poly.data.frame} object
##' @param i subscript list
##' @param ... Not used
##' @return subscripted \code{poly.data.frame}
##' @S3method subscript poly.data.frame
##' @export
subscript.poly.data.frame <- function(x, i, ...){
    if(is.null(names(i))) names(i) <- dIdsUnique(x)
    ids <- dIdsNested(x)
    i <- lapply(i, unique)
    for(j in seq_along(x)) x[[j]] <- subscript(x[[j]], i[ids[[j]]])
    return(x)
}


