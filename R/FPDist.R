##' Functional phylogenetic distance matrix
##'
##' @param formula One-sided \code{\link{formula}} object giving
##' `+`-separated expressions for the functional and phylogenetic
##' distance matrices
##' @param data A \code{\link{poly.data.frame}} object within which to
##' evaluate \code{formula}
##' @param a Phylogenetic weighting parameter
##' @param p Metric exponent (\code{p = 2} for Euclidean)
##' @return functional phylogenetic distance matrix
##' @export
FPDist <- function(formula, data, a, p = 2) {
    Fexpr <- formula[[2]][[2]]
    FDist <- eval(Fexpr, data)
    Pexpr <- formula[[2]][[3]]
    PDist <- eval(Pexpr, data)

    PDist <- ss(PDist, dNames(FDist)[[1]])
    FPDistOut <- ( (a * (FDist^p)) +
                  ((1-a) * (PDist^p)) )^(1/p)

    Fdat <- data[[match(all.vars(formula)[1], names(data))]]
    dimIds(FPDistOut) <- dimIds(Fdat)
    return(FPDistOut)
}
