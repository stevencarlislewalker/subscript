##' Follows
##'
##' Syntactic sugar for composition of two functions. Useful for apply
##' functions.
##' 
##' @param f a function of one argument
##' @param g another function with range equal to the domain of
##' \code{f}
##' @param ... additional arguments to \code{g}
##' @return The function given by \code{f} follows \code{g} of
##' \code{x}.
##' @rdname follows
##' @export
##' @examples
##' sqr <- function(x) x^2
##' min10 <- function(x) x - 10
##' (sqr %f% min10)(2)
##' div5 <- function(x) x/5
##' (sqr %f% min10 %f% div5)(2)
##' (unique %f% unlist %f% `[`)(letters[1:10], c(3, 2))
follows <- function(f, g) function(x, ...) f(g(x, ...))

##' @rdname follows
##' @export
`%f%` <- function(f, g) follows(f, g)

