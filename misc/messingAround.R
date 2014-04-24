library(subscript)
example("subscript-package")

summary(pdf)

dIdsNested(pdf)
dIdsConcat(pdf)
dIdsUnique(pdf)

dNamesNested(pdf)
dNamesConcat(pdf)
dNames(pdf)



ss(pdf, list(sites = c("a","e","f"), species = c("D","C","A")))







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
