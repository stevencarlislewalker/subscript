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
    if(!identical(sort(unlist(dNames(dist1), use.names = FALSE)),
                  sort(unlist(dNames(dist2), use.names = FALSE))))
        stop("incompatible distance matrices")
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


##' Simple linear regression on distance-based diversity indices
##'
##' @param slist A \code{\link{speciesList}} object
##' @param sdist1 A distance matrix
##' @param sdist2 A second distance matrix
##' @param resp A response variable
##' @param aGrid Optional grid of weighting parameters (see
##' \code{\link{combineDists}})
##' @return TODO
##' @rdname dbDiversityRegression
##' @export
dbDiversityRegression <- function(slist, sdist1, sdist2, resp, aGrid) {
    if(missing(aGrid)) aGrid <- seq(0, 1, 0.01)
    dists <- lapply(aGrid, combineDists, dist1 = sdist1, dist2 = sdist2)
    diversities <- sapply(dists, meanPairwiseDist, slist = slist)
    colnames(diversities) <- aGrid
    resp <- ss(resp, rownames(diversities))
    
    regFun <- function(diversity) lm(resp ~ diversity)
    
    structure(apply(diversities, 2, regFun),
              names = aGrid, class = "dbDiversityRegression")
              
}

##' @rdname dbDiversityRegression
##' @param object A \code{\link{dbDiversityRegression}} object
##' @param ... not used
##' @export
logLik.dbDiversityRegression <- function(object, ...){
    
    loglikes <- sapply(object, logLik)
    exp(loglikes - max(loglikes))
}


##' @rdname dbDiversityRegression
##' @export
posterior <- function(object, ...){
    delta <- mean(diff(as.numeric(names(object))))
    likelihood <- logLik(object)
    likelihood/(sum(delta*likelihood))
}

##' @param x \code{\link{dbDiversityRegression}} object
##' @rdname dbDiversityRegression
##' @export
mean.dbDiversityRegression <- function(x, ...){
    a <- as.numeric(names(x))
    delta <- mean(diff(a))
    delta * sum(posterior(x) * a)
}

##' @rdname dbDiversityRegression
##' @export
coef.dbDiversityRegression <- function(object, ...) t(sapply(object, coef))


#' Highest posterior density region for a
#'
#' Find points on a grid within a 100\code{p}\% highest posterior 
#' density region for the tuning parameter, a
#'
#' The 100\code{p}\% highest posterior density region for 'a' is 
#' the subset of the interval between 0 and 1, which contains 
#' 100\code{p}\% of the probability.
#'
#' @param level Size of the highest posterior density region.
#' @return A data frame with two columns:  the values of the
#'	grid within the hpd region and the value of the posterior
#'	at each point in this grid.
#' @rdname dbDiversityRegression
#' @export
a.hpd <- function(object, level = 0.95){
    aGrid <- as.numeric(names(object))
    post <- posterior(object)
	out <- data.frame(a = aGrid, post = post)
	if(level == 1L) return(out)
	if(level == 0L) return(out[-(1:length(post)),])
	dscrt.post <- post/sum(post)
	post.ord <- order(-post)
	hpd.levels <- rep(0, length(post))
	for(n in 1:length(post)){
		hpd.levels[n] <- sum(dscrt.post[post.ord[1:n]])
		if(hpd.levels[n] > level) break
	}
	n <- n - 1
	post.ord <- sort(post.ord[1:n])
	hpd.points <- aGrid[post.ord]
	out <- out[post.ord, ]
	print(hpd.levels[n])
	return(out)
}


##' Plot distance-based diversity regression
##'
##' @param x \code{\link{dbDiversityRegression}} object
##' @param y not used
##' @param ... not used
##' @export
plot.dbDiversityRegression <- function(x, y, ...){
	## xx <- x$surf$a
	## yy <- x$surf$posterior

	## plot(xx, yy, type = 'l', las = 1,
	## 	ylab = 'Posterior density',
	## 	xlab = 'Phylogenetic weighting parameter, a',
	## 	...)
	## rug(xx)
    post <- posterior(x)
    xlab <- expression(paste('Weighting parameter, ', italic(a)))
    ylab <- 'Approximate posterior density'
    ylim <- c(0, max(post))
    hpd <- a.hpd(x)
    a <- as.numeric(names(x))
    #par(mar = c(5, 5, 1, 1))
    plot(post ~ a, data = x$surf,
         las = 1, type = 'n',
         xlab = xlab, ylab = ylab, ylim = ylim)
    with(hpd, polygon(c(a, a[length(a):1]), c(post, rep(0, length(a))),
                      col = grey(0.9), border = NA))
    lines(post ~ a, data = x$surf)
}

    


    ## mode.a <- a[which.max(posterior)]
    ## var.a <- delta * sum(posterior * ((a - mean.a)^2))

