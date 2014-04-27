example("subscript-package", package = "subscript")


pdf <- poly.data.frame(env    = setDimIds(env,         "sites"),
                       coord  = setDimIds(coord,       "sites"),
                       geog   = setDimIds(dist(coord), "sites"),
                       traits = setDimIds(traits,      "species"),
                       coph   = setDimIds(as.dist(cophenetic(tree)), "species"),
                       tree   = setDimIds(tree,        "species"))
 crossprod(summary(pdf))
tcrossprod(summary(pdf))

form <- ~ dist(traits) + as.dist(cophenetic(tree))
FPDist <- function(formula, data, a, p = 2) {
    Fexpr <- form[[2]][[2]]
    FDist <- eval(Fexpr, data)
    Pexpr <- form[[2]][[3]]
    PDist <- eval(Pexpr, data)

    PDist <- ss(PDist, dNames(FDist)[[1]])
    FPDistOut <- ( (a * (FDist^p)) +
                  ((1-a) * (PDist^p)) )^(1/p)

    dimIds(FPDistOut) <- dimIds(FDist)
    return(FPDistOut)
}
dNames(FPDist(form, pdf, 0.5))


dNames(pdf)

dist(pdf$traits)


