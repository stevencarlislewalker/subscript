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
##' reOrder(longDist(tree), dNames(traits)[[1]])
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
##' Data frames are subscripted by rows (i.e. replicates) only.
##' 
##' @param x An object with data to be subscripted.
##' @param i Indices for the dimensions of replication of \code{x}.
##' @param ... Not used.
##' @return A subscripted version of \code{x}.
##' @aliases ss subscript.default
##' @seealso Specific \code{subscript} methods are available for
##' \code{\link{data.frame}}, \code{\link{dist}}, \code{\link{phylo}},
##' \code{\link{speciesList}}, \code{\link{longDist}}, and
##' \code{\link{poly.data.frame}} objects.  The
##' \code{\link{subscript-package}} help page has many examples.
##' @export
subscript <- function(x, i, ...) {
    if(hasBeenProcessed(i)) {
        UseMethod("subscript")
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
##' @export
subscript.default <- function(x, i, ...) {
    if(!is.recursive(i)) i <- list(i)
    out <- do.call("[", c(list(x), i))
    attr(out, "dimIds") <- attr(x, "dimIds")
    return(out)
}

##' @rdname subscript
##' @method subscript data.frame
##' @export
subscript.data.frame <- function(x, i, ...) {
                                        # if no dimIds, subscript by
                                        # rownames
    if(is.null(di <- dimIds(x))) {
        return(x[i, , drop = FALSE])
    } else {
                                        # if dimIds don't match
                                        # colnames, subscript by
                                        # rownames as well
        if(any(is.na(match(di, colnames(x))))) {
            return(x[i, , drop = FALSE])
        }
                                        # if dimIds match colnames,
                                        # subscript by matching
                                        # columns
        ii <- apply(mapply("%in%", x[di], i), 1, all)
        return(x[ii, ])
    }

    ## if(is.null(di <- dimIds(x))) {
    ##     return(x[i, , drop = FALSE])
    ## } else {
    ##     ii <- apply(mapply("%in%", x[di], i), 1, all)
    ##     return(x[ii, ])
    ## }
}


##' @rdname subscript
##' @export 
##' @method subscript dist
subscript.dist <- function(x, i, ...) as.dist(subscript(as.longDist(x), i))


##' @rdname subscript
##' @method subscript phylo
##' @export
subscript.phylo <- function(x, i, ...){
    inot <- setdiff(dNames(x)[[1]], i)
    drop.tip(x, inot)
}

##' @rdname subscript
##' @method subscript speciesList
##' @export
subscript.speciesList <- function(x, i, ...){
    ids <- attr(x, "dimIds")
    x <- x[i[[1]]]
    out <- lapply(x, intersect, x = i[[2]])
    attr(out, "dimIds") <- ids
    class(out) <- "speciesList"
    return(out)
}



##' @param reOrder should the order be arranged to be consistent with
##' \code{\link{dist}} objects?
##' @method subscript longDist
##' @rdname subscript
##' @export
subscript.longDist <- function(x, i, reOrder = TRUE, ...){
    if(length(i) < 2L) {
        out <- data.frame(row = NA, col = NA, dist = 0)
    } else {
        out <- x[(x$row %in% i) & (x$col %in% i), ]
        if(reOrder) out <- reOrder(out, dNames(out)[[1]])
    }
    attr(out, "dimIds") <- attr(x, "dimIds")
    return(out)
}


##' @rdname subscript
##' @method subscript poly.data.frame
##' @export
subscript.poly.data.frame <- function(x, i, ...){
    if(is.null(names(i))) names(i) <- dimIdsUnique(x) # FIXME: necessary ??
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
