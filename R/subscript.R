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
##' pdf <- poly.data.frame(env    = setDimIds(env,         "sites"),
##'                        coord  = setDimIds(coord,       "sites"),
##'                        geog   = setDimIds(dist(coord), "sites"),
##'                        traits = setDimIds(traits,      "species"),
##'                        coph   = setDimIds(as.dist(cophenetic(tree)), "species"),
##'                        tree   = setDimIds(tree,        "species"),
##'                        slist  = setDimIds(slist,       c("sites", "species")))
##'
##' summary(pdf)
##'
##' dimIdsNested(pdf)
##' dimIdsConcat(pdf)
##' dimIdsUnique(pdf)
##'
##' dNamesNested(pdf)
##' dNamesConcat(pdf)
##' dNames(pdf)
##'
##' ss(pdf, list(species = c("D","C","A"), sites = c("a","e","f")))
##' ss(pdf, list(sites = c("a","e","f"), species = c("D","C","A")))
##' ss(pdf, list(c("a","e","f"), c("D","C","A")))
##'
##' (FPDist <- combineDists(as.dist(cophenetic(tree)),
##'                         dist(traits), 0.5))
##'
##' l <- list(env    = env,
##'           coord  = coord,
##'           geog   = dist(coord),
##'           traits = traits,
##'           coph   = as.dist(cophenetic(tree)),
##'           tree   = tree,
##'           comm   = slist)
##' pdf2 <- as.poly.data.frame(l, 2)
##' summary(pdf2)
##' dNames(pdf2)
##'
##' meanPairwiseDist(slist, FPDist)
##'
##' regs <- dbDiversityRegression(slist,                         # species list
##'                               as.dist(cophenetic(pdf$tree)), # phylogenetic distances
##'                               dist(pdf$traits),              # functional distances
##'                               setNames(env$x, rownames(env)) # ecosystem function
##'                               )
##' 
##' mean(regs)
##' coef(regs)
##' a.hpd(regs)
##' plot(regs)
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
##' @method subscript data.frame
##' @export
subscript.data.frame <- function(x, i, ...) x[unlist(i, use.names = FALSE), , drop = FALSE]


##' Subscript a distance matrix
##' 
##' Subscript a distance matrix as a single-dimensional object.
##' 
##' @param x A \code{\link{dist}} object.
##' @param i Indices for the objects.
##' @param ... Not used.
##' @return A subscripted \code{\link{dist}} object.
##' @export 
##' @method subscript dist
subscript.dist <- function(x, i, ...){


                                        # convert subscript type to
                                        # numeric
    i <- conversion(unlist(i, use.names = FALSE), dNames(x)[[1]])
    
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
##' @method subscript phylo
##' @export
subscript.phylo <- function(x, i, ...){
                                        # convert to numeric
    i <- conversion(unlist(i, use.names = FALSE), dNames(x)[[1]])

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
##' @export
subscript.speciesList <- function(x, i, ...){
    ## i[[1]] <- conversion(i[[1]], names(x))
    ids <- attr(x, "dimIds")
    x <- x[i[[1]]]
    out <- lapply(x, intersect, i[[2]])
    attr(out, "dimIds") <- ids
    class(out) <- "speciesList"
    return(out)
}


##' Subscript a distance data frame
##'
##' @param x \code{dist.data.frame} object
##' @param i subscript list
##' @param ... Not used
##' @return subscripted \code{dist.data.frame} object
##' @export
subscript.dist.data.frame <- function(x, i, ...){

                                        # convert subscript type to
                                        # numeric
    nms <- dNames(x)[[1]]
    i <- conversion(unlist(i, use.names = FALSE), nms)
                                        # convert back, FIXME!
    i <- nms[i]

    return(x[(x$row %in% i) & (x$col %in% i), ])
}


##' Subscript a poly data frame
##'
##' @param x \code{poly.data.frame} object
##' @param i subscript list
##' @param ... Not used
##' @return subscripted \code{poly.data.frame}
##' @export
subscript.poly.data.frame <- function(x, i, ...){
    if(is.null(names(i))) names(i) <- dimIdsUnique(x)
    ids <- dimIdsNested(x)
    i <- lapply(i, unique)
    for(j in seq_along(x)) x[[j]] <- subscript(x[[j]], i[ids[[j]]])
    return(x)
}


