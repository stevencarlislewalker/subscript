library(subscript)
example("subscript-package")

dNamesNested(pdf)
dNamesConcat(pdf)
dIdsNested(pdf)
dIdsConcat(pdf)
dNames(pdf)

(dnamesAll <- do.call(c, dNamesNested(pdf)))
dimids <- unique(unlist(dIdsAll))
outer(dIdsAll, dimids, "==")
lapply(lapply(dimids, "==", dIdsAll), function(ii) unique(unlist(dnamesAll[ii])))



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
