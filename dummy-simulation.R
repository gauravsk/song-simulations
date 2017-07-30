# simple simulation of correlated bird songs

# we need some libraries
library(phytools); library(geiger); library(dplyr)

# we need a phylogeny

tree<-pbtree(d=0.5,n=100,extant.only=TRUE, tip.label  =paste("species_", 1:100, sep = ""))

# simulate H/M/L song diversity onto the tree
q <- list(rbind(c(-.5, .4, .1), c(.4, -.5, .25), c(.1,.4,-.5)))

set.seed(1232221)
sim.char(phy = tree, model = "d", par = q)
cats <- as.data.frame(sim.char(phy = tree, model = "d", par = q))
# cats <- cats %>% select(category = `1.1`)
colnames(cats) <- 'category'
table(cats)

# set up a matrix of parameters per individual
# the number of individuals is a function of the species' category in 'cats'
# every low-diversity sp (cat = 1) gets 2 individuals;
# every med-diversity sp (cat = 2) gets 4 individuals; 
# every hi-diversity  sp (cat = 3) gets 6 individuals.

simulated_df <- data.frame(species = rep(rownames(cats), cats$category*2))
simulated_df <- simulated_df %>% group_by(species) %>% mutate(ind = 1:n()) %>% ungroup()
simulated_df <- simulated_df %>% mutate(param1 = rnorm(nrow(simulated_df), 10, 2),
                                        param2 = exp(rnorm(nrow(simulated_df), 3, 1)),
                                        param3 = runif(rnorm(nrow(simulated_df), 5, 20)))
