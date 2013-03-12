bresnan <- read.csv(url("http://people.ucsc.edu/~mwagers/ling280/data/BresDative.txt"),
             header=TRUE, sep=" ")

install.packages("vcd")
library(vcd)

with(bresnan, table(prorec, proth, real)) -> pronominals
