example("subscript-package", package = "subscript")

ss(as.dist(pdf$geog), c("b", "d", "a"))
dist(pdf$coord)





set.seed(1)
par(mfrow = c(2, 1), mar = rep(0, 4))
plot(pdf$tree)
plot(reOrder(pdf$tree, LETTERS[sample.int(5)]))





regs <- dbDiversityRegression(slist,                         # species list
                              as.dist(cophenetic(pdf$tree)), # phylogenetic distances
                              dist(pdf$traits),              # functional distances
                              setNames(env$x, rownames(env)) # ecosystem function
                              )

mean(regs)
coef(regs)
a.hpd(regs)
plot(regs)




pdf <- poly.data.frame(env    = setDimIds(env,         "sites"),
                       coord  = setDimIds(coord,       "sites"),
                       geog   = setDimIds(dist(coord), "sites"),
                       traits = setDimIds(traits,      "species"),
                       coph   = setDimIds(as.dist(cophenetic(tree)), "species"),
                       tree   = setDimIds(tree,        "species"))
 crossprod(summary(pdf))
tcrossprod(summary(pdf))


FPDist(form, pdf, 0)

FPDist(form, pdf, 1)



setDimIds(dist(pdf$traits), dimIds(pdf$traits))

dNames(pdf)

dist(pdf$traits)





pdf <- list(env    = setDimIds(env, NULL),
            coord  = setDimIds(coord, NULL),
            geog   = setDimIds(dist(coord), NULL),
            traits = setDimIds(traits, NULL),
            coph   = setDimIds(as.dist(cophenetic(tree)), NULL),
            tree   = setDimIds(tree, NULL),
            comm   = setDimIds(slist, NULL))




summary(pdf)
dNamesNested(pdf)
pdf <- as.poly.data.frame(pdf, 2)
summary(pdf)



Reduce("union", dNamesConcat(pdf))


library(subscript)
l <- list(env    = env,
          coord  = coord,
          geog   = dist(coord),
          traits = traits,
          coph   = as.dist(cophenetic(tree)),
          tree   = tree,
          comm   = slist)

pdf2 <- as.poly.data.frame(l, 2)

summary(pdf)
summary(pdf2)

dNames(pdf)
dNames(pdf2)
dNames(pdf00)

pdf00 <- ss(pdf, list(species = c("D","C","A"), sites = c("a","e","f")))

ii <- list(sites = c("a","e","f"), species = c("D","C","A"))
attributes(pdf$slist)
attributes(ss(pdf$slist, ii))

with(pdf, coph)

pdf <- within(pdf, coph <- coph/max(coph))
pdf <- within(pdf, {
    fdist <- dist(traits)/max(dist(traits))
    dimIds(fdist) <- dimIds(traits)
})
with(pdf, plot(coph, ss(fdist, dNames(coph))))

pdf <- within(pdf, {
    fpdist <- combineDists(coph, fdist, 0.5)
    dimIds(fpdist) <- "species"
})

with(pdf, meanPairwiseDist(slist, fpdist))


