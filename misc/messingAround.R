example("subscript-package", package = "subscript")


pdf <- poly.data.frame(env    = dIds(env,         "sites"),
                       coord  = dIds(coord,       "sites"),
                       geog   = dIds(dist(coord), "sites"),
                       traits = dIds(traits,      "species"),
                       coph   = dIds(as.dist(cophenetic(tree)), "species"),
                       tree   = dIds(tree,        "species"))
 crossprod(summary(pdf))
tcrossprod(summary(pdf))


