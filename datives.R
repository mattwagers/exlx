bresnan <- read.csv(url("http://people.ucsc.edu/~mwagers/ling280/data/BresDative.txt"),
             header=TRUE, sep=" ")

# for mosaic()
install.packages("vcd")
library(vcd)
# for glmmPQL()
install.packages("MASS")
library(MASS)


with(bresnan, table(proth, real)) -> pron.theme
with(bresnan, table(prorec, real)) -> pron.recip

mosaic(pron.theme, shade=TRUE)
rbind(c(2104, 740), c(312,109)) ->expected

mosaic(expected)
