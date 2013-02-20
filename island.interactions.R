# LING280 Practice with Interactions
# 'whether'-island ratings data set from Sprouse, Wagers & Phillips (2012)
# 10.1353/lan.2012.0004

# Read the data in
read.csv("data/islands.simple.csv") -> islands.raw

# What is the design?
print(str(islands.raw))

# The unlist() function will convert the data.frame into a vector (by column)
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

# estimate a linear model that includes the two factors and their interaction

# which is better? why?

# A little about manipulating the contrast coding in R ...
# By default, R factors are treatment - i.e. (0,1) - coded
# To do it by hand, you could use the contr.treatment() function
# You can check this with the contrasts() function
contrasts(islands.df$comp)
contrasts(islands.df$gapsite)
# What does assign to either factor as a baseline?
# On what basis does it do it? [it's dumb ...]

# Switch teh baseline for "GAPSITE"
# abs() - absolute value function
abs(contrasts(islands.df$gapsite)-1)->contrasts(islands.df$gapsite)

# Generate sum contrasts for two levels
contr.sum(2)
# Divide by 2 so that the slope is the diff. between means
contrasts(islands.df$comp) <- contr.sum(2)/2
contrasts(islands.df$gapsite) <- contr.sum(2)/2
# I want to treat 'matrix' as the positive contrast, so:
contrasts(islands.df$gapsite) <- -1 * contrasts(islands.df$gapsite)


## visualizations
# some plotting functions understand formulas
boxplot(rating~comp*gapsite, data=islands.df)

# what kind of interaction?
with(islands.df,
     interaction.plot(x.factor = comp, 
                      trace.factor = gapsite, 
                      response=rating))