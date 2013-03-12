bresnan <- read.csv(url("http://people.ucsc.edu/~mwagers/ling280/data/BresDative.txt"),
             header=TRUE, sep=" ")

# for mosaic()
install.packages("vcd")
library(vcd)
# for glmmPQL()
install.packages("MASS")
library(MASS)


with(bresnan, table(prorec, proth, real)) -> pronominals
