example("subscript-package", package = "subscript")


pdf <- poly.data.frame(env    = dIds(env,         "sites"),
                       coord  = dIds(coord,       "sites"),
                       geog   = dIds(dist(coord), "sites"),
                       traits = dIds(traits,      "species"),
                       coph   = dIds(as.dist(cophenetic(tree)), "species"),
                       tree   = dIds(tree,        "species"))
 crossprod(summary(pdf))
tcrossprod(summary(pdf))


sqr <- function(x) x^2
min10 <- function(x) x - 10
div5 <- function(x) x/5
(sqr %f% min10 %f% div5)(2)

(a <- runif(10))
(unique %f% unlist %f% `[`)(a, c(3, 2))


mapply(fn, dIdsNested(pdf), dIdsUnique(pdf))

fn <- function(x, y) any(x == y)
outer(dIdsNested(pdf), as.list(dIdsUnique(pdf)), fn)

lapply(dIdsNested(pdf), fn, dIdsUnique(pdf))
dIdsUnique(pdf)[[1]] == dIdsNested(pdf)

as.matrix(slist)
ss(slist, list(c("b", "a"), c("D","A")))

ss(geog, c("b","a","e"))
plot(ss(tree, c("E","C","D")))
plot(tree)

geog <- dimnamesNames(geog, c("sites"))
dimnames(geog)

AA <- matrix(rnorm(n*m), n, m)
dimnames(AA) <- list(letters[1:n], LETTERS[1:m])
conversion(list(c("b","e"), c("B", "A")), dimnames(AA))
ss(AA, list(c("b","e"), c("B", "A")))
