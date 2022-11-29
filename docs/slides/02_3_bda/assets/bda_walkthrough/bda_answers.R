# Useful libraries
library("tidyverse")      # Mainly for ggplot2, dplyr and forcats
library("here")           # Paths
library("lme4")           # Multilevel models
library("brms")           # Bayesian regression models in Stan
library("tidybayes")      # Tidying and ploting posteriors
library("palmerpenguins") # Penguin dataset
library("bayestestR")
#
# Overview
#

# We are going to explore the palmer penguins data set 'penguins' (you might 
# need to install it). 
# Specifically we will use a subset of this data: 
tuxedo_birds <- penguins %>% 
  select(species, bill_depth_mm, body_mass_g) %>% 
  filter(species == c("Chinstrap", "Gentoo")) %>% 
  na.omit() %>% 
  droplevels()

# Take a moment to get to know the data set using your favorite methods. 
# We are particularly interested in predicting body mass as a function 
# species and bill depth. 
# We will slowly build up to that model and make plots along the way. 
# (Note: we will recklessly completely disregard priors for now)




# 1. Intercept-only model
#    - Calculate the avg body mass for the entire data set 
#      (keep this value handy)
#    - Fit an intercept-only frequentist model and assess
#    - Fit an intercept-only bayesian model and assess
#    - How do the models compare with each other? And with the avg body mass?
#    - Make a plot of the posterior estimate of the intercept. What does this 
#      represent?

glimpse(tuxedo_birds)

tuxedo_birds$body_mass_g %>% mean
fre_0 <-  lm(body_mass_g ~ 1, data = tuxedo_birds)
bda_0 <- brm(body_mass_g ~ 1, data = tuxedo_birds)

as_tibble(bda_0) %>% 
  ggplot(., aes(x = b_Intercept)) + 
    geom_histogram(fill = "grey95", color = "black")



# 2. Add a categorical predictor
#    - Calculate the avg. body mass as a function of species. 
#    - Fit an additive model: body mass as a function of species
#        - Fit the frequentist model first, then the bayesian model
#        - What is the reference level? 
#        - What do the parameter estimates represent?
#    - Try to plot the posterior distribution (forest plot) 
#    - How confident are you that the group difference is != to 0?

tuxedo_birds %>% 
  group_by(species) %>% 
  summarize(avg = mean(body_mass_g))

fre_1 <-  lm(body_mass_g ~ 1 + species, data = tuxedo_birds)
bda_1 <- brm(body_mass_g ~ 1 + species, data = tuxedo_birds)

as_tibble(bda_1) %>% 
  select(b_Intercept, b_speciesGentoo) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>% 
  ggplot(., aes(x = estimate, y = parameter)) +
    stat_halfeye()



# 3. Add the continuous predictor
#    - Again, fit both frequentist and bayesian models with species and 
#      bill_depth_mm as predictors
#    - Compare and interpret both models
#    - Wrangle the posterior distribution to make 1) a forest plot and 2) a 
#      scatterplot (all data plus regression lines) (hard!)

fre_2 <-  lm(body_mass_g ~ 1 + species + bill_depth_mm, data = tuxedo_birds)
bda_2 <- brm(body_mass_g ~ 1 + species + bill_depth_mm, data = tuxedo_birds)

samples <- as_tibble(bda_2) %>% 
  transmute(int_chinstrap = b_Intercept, 
    int_gentoo = b_Intercept + b_speciesGentoo, 
    slope = b_bill_depth_mm) %>% 
  pivot_longer(cols = -slope, names_to = "species", values_to = "intercept") 

tuxedo_birds %>% 
  ggplot(., aes(x = bill_depth_mm, y = body_mass_g, color = species)) + 
    geom_abline(data = sample_n(samples, 200), alpha = 0.1, 
      aes(intercept = intercept, slope = slope)) + 
    geom_point() + 
    geom_abline(color = "white", size = 1, 
      intercept = fixef(bda_2)[1, 1], 
      slope = fixef(bda_2)[3, 1]) + 
    geom_abline(color = "white", size = 1, 
      intercept = fixef(bda_2)[1, 1] + fixef(bda_2)[2, 1], 
      slope = fixef(bda_2)[3, 1])
