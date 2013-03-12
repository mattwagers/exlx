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

##### Cross-validate against another sample
subset(bresnan, mod=="wallstreet") -> wsj
subset(bresnan, mod=="switchboard") -> switch

# equiv: lmer(real~proth +prorec + (1|verb))
switch.model <- glmmPQL(real ~ proth + prorec, 
        random=~1|verb,
        data=switch, family=binomial)
predict(switch.model, wsj) -> wsj.predict
am.I.a.PP <- wsj.predict > 0.0
table(wsj$real=="PP", am.I.a.PP)

##### Cross-validate by splitting our sample randomly
nrow(bresnan) -> n.obs
# create 2 bresnans

accuracies <- vector("double",100)
txtProgressBar(0,100, style=3) -> pb
for(i in 1:100){
  setTxtProgressBar(pb, i)
  split(bresnan, factor(rbinom(n.obs, 1, 0.85))) -> bresnan.split
# estimate model from the bigger bresnan
  bresnan.split$`1`
  glm(real ~ proth + prorec, 
      data=bresnan.split$`1`, family=binomial) -> current.model
# predict values of the smaller d.f.
  predict(current.model, bresnan.split$`0`) -> current.prediction

# get accuracy
  match.to.data<-
    (current.prediction>0.0)==(bresnan.split$`0`$real=="PP")
  mean(match.to.data) -> accuracies[i]

}