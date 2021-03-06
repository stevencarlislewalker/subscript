##' p-norm combination of distance matrices
##'
##' @param dist1 A distance matrix
##' @param dist2 A second distance matrix
##' @param a Weighting parameter (number between 0 and 1 giving the
##' weighting towards \code{dist1})
##' @param p Metric exponent (\code{p = 2} for Euclidean)
##' @param ... Passed to \code{\link{as.longDist}}
##' @note \code{dist1} and \code{dist2} are coererced to
##' \code{\link{longDist}} objects with \code{norm = TRUE}.
##' @return combined distance matrix
##' @export
combineDists <- function(dist1, dist2, a, p = 2, ...) {
    dist1 <- as.longDist(dist1, ...)
    dist2 <- as.longDist(dist2, ...)
    ## ## FIXME:  move this check to reOrder ??
    ## if(!identical(sort(unlist(dNames(dist1), use.names = FALSE)),
    ##               sort(unlist(dNames(dist2), use.names = FALSE))))
    ##     stop("incompatible distance matrices")
    ## if(reOrder) dist2 <- reOrder(dist2, dNames(dist1)[[1]])
    distsOut <- ( (a * (dist1$dist^p)) +
                ((1-a) * (dist2$dist^p)) )^(1/p)
    dist1$dist <- distsOut
    return(dist1)
}

##' Mean pairwise distance
##'
##' @param slist A \code{\link{speciesList}} object
##' @param sdist A \code{\link{longDist}} object among species
##' @param ... Passed to \code{\link{as.longDist}}
##' @return vector of average pairwise distance for each site
##' @export
meanPairwiseDist <- function(slist, sdist, ...) {
    sdist <- as.longDist(sdist, ...)
    distsPerSite <- lapply(slist, subscript, x = sdist,
                           reOrder = FALSE) # for speed, reOrder not necessary here
    distsPerSite <- lapply(distsPerSite, "[[", "dist")
    return(sapply(distsPerSite, mean))
}


##' @return TODO
##' @rdname dbDiversityRegression
##' @export
dbDiversityProfile <- function(slist, sdist1, sdist2, aGrid, ...) {
    if(missing(aGrid)) aGrid <- seq(0, 1, 0.01)
    sdist1 <- as.longDist(sdist1, ...)
    sdist2 <- as.longDist(sdist2, ...)
    dists <- lapply(aGrid, combineDists,
                    dist1 = sdist1, dist2 = sdist2)
    diversities <- sapply(dists, meanPairwiseDist, slist = slist)
    colnames(diversities) <- aGrid
    return(diversities)
}

##' Simple linear regression on distance-based diversity indices
##'
##' @param slist A \code{\link{speciesList}} object
##' @param sdist1 An object that is coercible to \code{\link{longDist}}
##' @param sdist2 A second object that is coercible to \code{\link{longDist}}
##' @param resp A response variable
##' @param diversities [optional] Output of \code{\link{dbDiversityProfile}}
##' @param aGrid Optional grid of weighting parameters (see 
##' \code{\link{combineDists}})
##' @param ... Passed to \code{\link{subscript.longDist}}
##' @return TODO
##' @note There are two ways to provide diversity inputs: (1)
##' \code{slist}, \code{sdist1}, and \code{sdist2}, or (2)
##' \code{diversities}.  If the former,
##' \code{\link{dbDiversityProfile}} is called to compute
##' \code{diversities} from \code{slist}, \code{sdist1}, and
##' \code{sdist2}.  Also note that the coercion of \code{sdist1} and
##' \code{sdist2} forces normalization of the distances (see
##' \code{norm} argument of \code{\link{longDist}}).
##' @rdname dbDiversityRegression
##' @export
dbDiversityRegression <- function(slist, sdist1, sdist2, resp, diversities, aGrid, ...) {

    typeI  <- missing(slist) & missing(sdist1) & missing(sdist2)
    typeII <- missing(diversities)
    if(typeI & typeII) stop("one of either (1) slist, sdist1, and sdist2 ",
                            "or (2) diversities must be specified")
    if(!typeI & !typeII) stop("if diversities are specified, slist, sdist1, ",
                              "and sdist2 cannot be specified")
    if(typeII) diversities <- dbDiversityProfile(slist, sdist1, sdist2, aGrid, ...)

    aGrid <- as.numeric(colnames(diversities))
    resp <- subscript(resp,
                      structure(rownames(diversities), 
                                processed = "tag")) # tag for speed
    
    regFun <- function(diversity) lm(resp ~ diversity)
    
    structure(apply(diversities, 2, regFun),
              names = aGrid, class = "dbDiversityRegression")
              
}

##' @rdname dbDiversityRegression
##' @param object A \code{\link{dbDiversityRegression}} object
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
aHpd <- function(object, level = 0.95){
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
    hpd <- aHpd(x)
    a <- as.numeric(names(x))
    #par(mar = c(5, 5, 1, 1))
    plot(post ~ a, data = x$surf,
         las = 1, type = 'n',
         xlab = xlab, ylab = ylab, ylim = ylim)
    with(hpd, polygon(c(a, a[length(a):1]), c(post, rep(0, length(a))),
                      col = grey(0.9), border = NA))
    lines(post ~ a, data = x$surf)
}

##' Random permutation
##'
##' @param x a vector
##' @return \code{x} randomly permuted
##' @export
permute <- function(x) x[sample.int(length(x))]



    ## mode.a <- a[which.max(posterior)]
    ## var.a <- delta * sum(posterior * ((a - mean.a)^2))



