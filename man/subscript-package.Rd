\docType{package}
\name{subscript-package}
\alias{subscript-package}
\title{Consistent subscripting}
\description{
Consistent subscripting
}
\examples{
library(subscript)
library(ape)
set.seed(1)

n <- 8 # number of sites
m <- 5 # number of species

env <- data.frame(x = rnorm(n))
coord <- data.frame(lat = rnorm(n), long = rnorm(n))
rownames(env) <- rownames(coord) <- letters[1:n]

traits <- data.frame(z1 = rnorm(m), z2 = rnorm(m))
rownames(traits) <- LETTERS[1:m]
newick <- "((A,(B,E)),(D,C));"
tree <- chronos(compute.brlen(read.tree(text = newick)))

slist <- speciesList(replicate(n, LETTERS[sample.int(m, sample.int(m, 1))]), letters[1:n])
slist <- speciesList(slist)

pdf <- poly.data.frame(env    = setDimIds(env,         "sites"),
                       coord  = setDimIds(coord,       "sites"),
                       geog   = setDimIds(dist(coord), "sites"),
                       traits = setDimIds(traits,      "species"),
                       coph   = setDimIds(as.dist(cophenetic(tree)), "species"),
                       tree   = setDimIds(tree,        "species"),
                       slist  = setDimIds(slist,       c("sites", "species")))

summary(pdf)

dimIdsNested(pdf)
dimIdsConcat(pdf)
dimIdsUnique(pdf)

dNamesNested(pdf)
dNamesConcat(pdf)
dNames(pdf)

ss(pdf, list(species = c("D","C","A"), sites = c("a","e","f")))
ss(pdf, list(sites = c("a","e","f"), species = c("D","C","A")))
ss(pdf, list(c("a","e","f"), c("D","C","A")))
}

