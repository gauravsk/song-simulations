# simple simulation of correlated bird songs

# we need some libraries
library(phytools); library(geiger); library(dplyr); library(tibble)

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


# now, simulated_df has 344 rows. each species of Category 1 has 2 rows, each of Cat 2 has 4, and each of Cat 3 has 6

# first, let's get the "true" PC Values - these are calculated by averaging params from all individuals we have for each species

simulated_df_ave <- simulated_df %>% group_by(species) %>% summarise_all(funs(mean)) %>% select(-ind) %>% ungroup() %>% as.data.frame() %>% column_to_rownames("species")

simulated_df_ave <- scale(simulated_df_ave)
song_pca_full <- phyl.pca(tree, simulated_df_ave, method = "BM", mode = "cor")
biplot(song_pca_full)

# extract the PC scores - these will be the "true" PC scores that we are "trying" to get 
scores_from_full <- song_pca_full$S



# subset full dataframe and get pc scores from 2 inds/species -----

# the goal now is to make a smaller version of simulated_df which has just 200 rows - 2 per species


simulated_df_subset <- simulated_df %>% group_by(species) %>% sample_n(2)

simulated_df_subset_ave <- simulated_df_subset %>% group_by(species) %>% summarise_all(funs(mean)) %>% select(-ind) %>% ungroup() %>% as.data.frame() %>% column_to_rownames("species")

simulated_df_subset_ave <- scale(simulated_df_subset_ave)
song_pca_sub <- phyl.pca(tree, simulated_df_subset_ave, method = "BM", mode = "cor")
biplot(song_pca_sub)

# extract the PC scores - these will be the "true" PC scores that we are "trying" to get 
scores_from_sub <- song_pca_sub$S

par(mfrow = c(1,3))
corrs <- numeric(3)
for (current_pc in 1:3){
  plot(scores_from_full[,current_pc]~scores_from_sub[,current_pc])
  corrs[current_pc] <- cor(scores_from_full[,current_pc], scores_from_sub[,current_pc])
}
corrs

#yay working together

