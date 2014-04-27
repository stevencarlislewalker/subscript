example("subscript-package", package = "subscript")


pdf <- poly.data.frame(env    = setDimIds(env,         "sites"),
                       coord  = setDimIds(coord,       "sites"),
                       geog   = setDimIds(dist(coord), "sites"),
                       traits = setDimIds(traits,      "species"),
                       coph   = setDimIds(as.dist(cophenetic(tree)), "species"),
                       tree   = setDimIds(tree,        "species"))
 crossprod(summary(pdf))
tcrossprod(summary(pdf))


FPDist(form, pdf, 0)
as.dist(cophenetic(pdf$tree))
FPDist(form, pdf, 1)
dist(pdf$traits)


setDimIds(dist(pdf$traits), dimIds(pdf$traits))

dNames(pdf)

dist(pdf$traits)


