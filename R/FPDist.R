##' Euclidean combination of distance matrices
##'
##' @param dist1 A distance matrix
##' @param dist2 A second distance matrix
##' @param a Weighting parameter (number between 0 and 1 giving the
##' weighting towards \code{dist1})
##' @param p Metric exponent (\code{p = 2} for Euclidean)
##' @return combined distance matrix
##' @export
combineDists <- function(dist1, dist2, a, p = 2) {
    dist2 <- ss(dist2, dNames(dist1))
    distOut <- ( (a * (dist1^p)) +
                ((1-a) * (dist2^p)) )^(1/p)
    return(distOut)
}



## param formula One-sided \code{\link{formula}} object giving
## `+`-separated expressions for the functional and phylogenetic
## distance matrices
## param data A \code{\link{list}} or \code{\link{poly.data.frame}}
## object within which to evaluate \code{formula}
##     function(formula, data, a, p = 2) {
##     Fexpr <- formula[[2]][[2]]
##     FDist <- eval(Fexpr, data)
##     Pexpr <- formula[[2]][[3]]
##     PDist <- eval(Pexpr, data)
##     PDist <- ss(PDist, dNames(FDist)[[1]])
##     FPDistOut <- ( (a * (FDist^p)) +
##                   ((1-a) * (PDist^p)) )^(1/p)

##     if(is.poly.data.frame(data)) {
##         Fdat <- data[[match(all.vars(formula)[1], names(data))]]
##         dimIds(FPDistOut) <- dimIds(Fdat)
##     }
##     return(FPDistOut)
## }

##' Mean pairwise distance
##'
##' @param slist A \code{\link{speciesList}} object
##' @param sdist A distance matrix among species
##' @return vector of average pairwise distance for each site
##' @export
meanPairwiseDist <- function(slist, sdist) {
    distsPerSite <- lapply(slist, subscript, x = sdist)
    nDistsPerSite <- sapply(distsPerSite, length)
    ave <- sapply(distsPerSite, sum)
    ave[ave > 0L] <- ave[ave > 0L]/nDistsPerSite[ave > 0L]
    return(ave)
}
