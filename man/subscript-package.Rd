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

pdf <- poly.data.frame(env    = dIds(env,         "sites"),
                       coord  = dIds(coord,       "sites"),
                       geog   = dIds(dist(coord), "sites"),
                       traits = dIds(traits,      "species"),
                       coph   = dIds(as.dist(cophenetic(tree)), "species"),
                       tree   = dIds(tree,        "species"),
                       slist  = dIds(slist,       c("sites", "species")))

summary(pdf)

dIdsNested(pdf)
dIdsConcat(pdf)
dIdsUnique(pdf)

dNamesNested(pdf)
dNamesConcat(pdf)
dNames(pdf)
}

