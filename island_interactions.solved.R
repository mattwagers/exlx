# LING280 Practice with Interactions
# 'whether'-island ratings data set from Sprouse, Wagers & Phillips (2012)
# 10.1353/lan.2012.0004

# Read the data in
read.csv("data/islands.simple.csv") -> islands.raw

# What is the design?
print(str(islands.raw))

# unlist() will make the data.frame into a vector (by column)
islands.df <- data.frame(rating = unlist(islands.raw))

nrow(islands.raw) -> n.obs.percondition #hint
islands.df$comp <- c(rep("that", 2 * n.obs.percondition),
                     rep("whether", 2 * n.obs.percondition))
islands.df$gapsite <- rep(c(rep("matrix", n.obs.percondition),
                            rep("embedded", n.obs.percondition)), 2)

# turn the string values from above into 'factors'
islands.df$comp <- factor(islands.df$comp)
islands.df$gapsite <- factor(islands.df$gapsite)

# double check that everything is correctly distributed
with(islands.df, table(comp, gapsite))

# estimate a linear model that includes the two factors as simple effects only
model.A_B <- lm(rating ~ comp + gapsite, data=islands.df)
# estimate a linear model that includes the two factors and their interaction
model.AxB <- lm(rating ~ comp * gapsite, data=islands.df)

# Which is better? why?
lr <- as.numeric(2*(logLik(model.AxB)-logLik(model.A_B)))
p.val <- 1 - pchisq(lr, 1)
# OR
anova(model.A_B, model.AxB)
# AND
summary(model.A_B)$adj.r.squared
summary(model.AxB)$adj.r.squared


# A little about manipulating the contrast coding in R ...

# By default, R factors are treatment - i.e. (0,1) - coded
# That means one level in each factor is a 'baseline' (the 0)
# Coefs for the factor are other levels' pairwise difference to the baseline  
# You can check this with the contrasts() function
contrasts(islands.df$comp)
contrasts(islands.df$gapsite)
# What does R assign to either factor as a baseline?
# On what basis does it do it? [it's dumb ...]
# To make these by hand, you could use the contr.treatment() function

# Switch the baseline for "gapsite"
# abs() - absolute value function;
# (0,1) - 1 -> (-1, 0); abs(-1, 0) -> (1,0)
abs(contrasts(islands.df$gapsite)-1)->contrasts(islands.df$gapsite)

# Generate sum contrasts for two levels
contr.sum(2)
# Divide by 2 so that the coefficient/slope is the diff. between means
contrasts(islands.df$comp) <- contr.sum(2)/2
contrasts(islands.df$gapsite) <- contr.sum(2)/2
# Treat 'matrix' as the positive contrast, so:
contrasts(islands.df$gapsite) <- -1 * contrasts(islands.df$gapsite)


## Visualizations
# some plotting functions understand formulas
boxplot(rating~comp*gapsite, data=islands.df)

# what kind of interaction?
with(islands.df,
     interaction.plot(x.factor = comp, 
                      trace.factor = gapsite, 
                      response=rating))

# Ratings data are not perfectly suited to untransformed linear models
# (1) they are bounded (1-n)
# Check the residuals to see what happens at the tails ...
qqnorm(resid(model.AxB))
ks.test(scale(resid(model.AxB)), "pnorm")

# Consider another representation of the data
# What does this suggest about how we might analyze ratings data?

# Set up some handy plotting parameters
col.comp <- c("dodgerblue", "red")
names(col.comp) <- c("that", "whether")
lty.gap <- c("solid", "dashed")
names(lty.gap) <- c("matrix", "embedded")

# Loop through the factor levels
# An embedded loop recaps a 2 x 2 design ...
add.par <- FALSE
for(comp.loop in c("that", "whether")){
  for(gap.loop in c("matrix", "embedded")){
    with(islands.df, 
         plot.ecdf(rating[comp==comp.loop & gapsite==gap.loop], 
                   verticals = TRUE, lwd = 5, xlim = c(1,7), 
                   main = "Ratings distribution", 
                   xlab= "Rating", ylab= "P(Response <= Rating)",
                   col = col.comp[comp.loop], lty = lty.gap[gap.loop],
                   add = add.par))
    add.par <- TRUE
  }
}
# Place a legend
legend("topleft", 
       legend=c("that/mat.gap", "that/emb.gap", "whether/mat.gap", "whether/mat.gap"),
       col = col.comp[c("that","that","whether","whether")], 
       lty = lty.gap[rep(c("matrix","embedded"),2)],
       lwd = 3, bty = "n", cex=0.75)
  
### or ... Lattice/Histogram
library(lattice)
with(islands.df,
     histogram(~ rating|comp + gapsite, 
               type="density", breaks=1:7))
