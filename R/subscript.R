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
##'                        geog   = setDimIds(longDist(coord), "sites"),
##'                        traits = setDimIds(traits,      "species"),
##'                        coph   = setDimIds(longDist(tree), "species"),
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
##' ss1 <- ss(pdf, list(species = c("D","C","A"), sites = c("a","e","f")))
##' ss2 <- ss(pdf, list(sites = c("a","e","f"), species = c("D","C","A")))
##' ss3 <- ss(pdf, list(c("a","e","f"), c("D","C","A")))
##' try(ss4 <- ss(pdf, list(c("D","C","A"), c("a","e","f")))) # should be error
##' ss5 <- ss(pdf, list(species = c("D","C","A")))
##' 
##' nDims(pdf)
##' nDims(traits) # data frames (perhaps confusingly) have one dimension
##' nDims(tree)
##'
##' longDist(tree)
##' longDist(traits)
##' reorder(longDist(tree), dNames(traits)[[1]])
##'
##' (FPDist <- combineDists(tree, traits, 0.5))
##' meanPairwiseDist(pdf$slist, setDimIds(FPDist, "species"))
##'
##' l <- list(env    = env,
##'           coord  = coord,
##'           geog   = dist(coord),
##'           traits = traits,
##'           coph   = as.dist(cophenetic(tree)),
##'           tree   = tree,
##'           comm   = slist)
##' pdf2 <- as.poly.data.frame(l, 2, verb = TRUE)
##' summary(pdf2)
##' dNames(pdf2)
##' regs <- dbDiversityRegression(slist,                         # species list
##'                               as.dist(cophenetic(pdf$tree)), # phylogenetic distances
##'                               dist(pdf$traits),              # functional distances
##'                               setNames(env$x, rownames(env)) # ecosystem function
##'                               )
##'
##' mean(regs)
##' coef(regs)
##' aHpd(regs)
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
    if(hasBeenProcessed(i)) {
        UseMethod('subscript')
    } else {
        i <- processSubscript(x, i)
        subscript(x, i, ...)
    }
}

##' @rdname subscript
##' @export
ss <- function(x, i, ...) subscript(x, i, ...)

##' @rdname subscript
##' @method subscript default
subscript.default <- function(x, i, ...) {
    ## stop("subscripting not yet written")
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
subscript.data.frame <- function(x, i, ...) x[i, , drop = FALSE]


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

    stop("not finished")
    
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
    inot <- setdiff(dNames(x)[[1]], i)
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
    
    ids <- attr(x, "dimIds")
    x <- x[i[[1]]]
    out <- lapply(x, intersect, i[[2]])
    attr(out, "dimIds") <- ids
    class(out) <- "speciesList"
    return(out)
}


##' Subscript a distance data frame
##'
##' @param x \code{longDist} object
##' @param i subscript list
##' @param reorder should the order be arranged to be consistent with
##' \code{\link{dist}} objects?
##' @param ... Not used
##' @return subscripted \code{longDist} object
##' @export
subscript.longDist <- function(x, i, reorder = TRUE, ...){
    if(length(i) < 2L) {
        out <- data.frame(row = NA, col = NA, dist = 0)
    } else {
        out <- x[(x$row %in% i) & (x$col %in% i), ]
        if(reorder) out <- reorder(out, dNames(out)[[1]])
    }
    attr(out, "dimIds") <- attr(x, "dimIds")
    return(out)
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



##' Process subscript
##' 
##' @param x an object to be subscripted
##' @param i subscript
##' @param ... not used
##' @return the processed subscript with a \code{processed} attribute
##' @export
processSubscript <- function(x, i, ...) {
    li <- length(i)
    dn <- dNames(x)
    nd <- length(dn)  #  nDims(x), but faster
    di <-  names(dn)  # dimIds(x), but faster
    if(is.recursive(i)) {
        if(nd == 1L) {
                                        # recursive subscript, 1D
                                        # object
            if(li > 1L) stop("more than one subscript dimension ",
                             "for a one-dimensional object")
            i <- as.character(unlist(i, use.names = FALSE))
            if(!allIn(i, dn[[1]]))
                stop("subscript out of range")
        } else {
            if(is.null(di)) {
                                        # recursive subscript,
                                        # multidimensional object, no
                                        # dimIds
                if(nd != li) stop("dimensions being subscripted ambiguous")
                i <- lapply(i, as.character)
                if(!allIn(i, dn[[1]]))
                    stop("subscript out of range")
            } else {
                                        # recursive subscript,
                                        # multidimensional object,
                                        # with dimIds
                nm <-  names(i)
                if(is.null(nm)) {
                    if(li != length(di))
                        stop("dimensions being subscripted ambiguous")
                    nm <- di
                }
                i <- setNames(lapply(i, as.character), nm)
                i <- setNames(                  i[di], di)
                whichNull <- which(sapply(i, is.null))
                i[whichNull] <- dn[whichNull]
            }
            if(!all(mapply(allIn, i, dn)))
                stop("subscript out of range")
        } 
    } else {
        if(nd == 1L) {
                                        # vector subscript, 1D object
            i <- as.character(i)
        } else {
                                        # vector subscript,
                                        # multidimensional object
            stop("dimension being subscripted ambiguous")
        }
        if(!allIn(i, dn[[1]]))
            stop("subscript out of range")
    } 
    attr(i, "processed") <- "tag"
    return(i)
}

hasBeenProcessed <- function(i)
    ifelse(is.null(attr(i, "processed")), FALSE, TRUE)

# is all of x in y?
allIn <- function(x, y) all(x %in% y)
