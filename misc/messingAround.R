example("subscript-package", package = "subscript")


pdf <- poly.data.frame(env    = dIds(env,         "sites"),
                       coord  = dIds(coord,       "sites"),
                       geog   = dIds(dist(coord), "sites"),
                       traits = dIds(traits,      "species"),
                       coph   = dIds(as.dist(cophenetic(tree)), "species"),
                       tree   = dIds(tree,        "species"))
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

    dIds(FPDistOut, )
}
dNames(FPDist(form, pdf, 0.5))


dNames(pdf)




