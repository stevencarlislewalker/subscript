library(subscript)
set.seed(1)
n <- 10
d1 <- dist(setNames(rnorm(n), sample(letters, n)))
d2 <- dist(setNames(rnorm(n), sample(letters, n)))
ssTry <- try(ss(d2, dNames(d1)), silent = TRUE)
cdTry <- try(combineDists(d1, d2, 0.5), silent = TRUE)

## stopifnot(inherits(ssTry, "try-error"))
## stopifnot(inherits(cdTry, "try-error"))


