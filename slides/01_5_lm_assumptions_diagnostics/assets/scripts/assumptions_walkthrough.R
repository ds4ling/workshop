# Install: 
# - ggfortify
# - lawstat
# - lmtest
# - DataCombine
# - gvlma

# Load libs
library("lawstat")
library("ggfortify")
library("lmtest")
library("DataCombine")
library("gvlma")


# Fit some models
mod1 <- lm(mpg ~ wt, data = mtcars)
mod2 <- lm(dist ~ speed, data = cars[1:20, ])


# Assumptions

# 1.
# The regression model is linear in parameters
# Eyeball it











# 2. 
# The mean of residuals is zero
# How to check?: Check model summary and test manually
mean(mod1$residuals)










# 3.
# Homoscedasticity of residuals or equal variance
# How to check? 
autoplot(mod1, which = c(1, 3))

# What are you looking for? The line should be more or less flat
autoplot(mod2, which = c(1, 3))


library(tidyverse)
library(patchwork)

mod1$residuals %>% length
mod2$residuals %>% length

mod1$fitted

resid  <- mod1$residuals
fitted <- mod1$fitted
resid_cog <- tibble(resid_mean = mean(resid), resid_median = median(resid)) %>% 
  pivot_longer(cols = everything())
  
length(mod1$residuals) * 0.1
length(mod2$residuals) * 0.1

2 * IQR(mod2$residuals) / length(mod2$residuals)^(1/3)


diagnosis <- function(mod) {

  resid  <- mod$residuals
  fitted <- mod$fitted
  
  resid_cog <- mean(resid)
  
  fd_bw <- 2 * IQR(resid) / length(resid)^(1/3)

  p1 <- as_tibble(x = resid, y = fitted) %>% 
    ggplot() + 
    aes(x = fitted, y = resid) + 
    geom_hline(yintercept = 0, lty = 3) + 
    geom_point(pch = 21, size = 1.25, fill = "grey") + 
    labs(y = "Residuals", x = "Fitted values") + 
    ds4ling::ds4ling_bw_theme()
  
  p2 <- as_tibble(x = resid) %>% 
    ggplot() + 
    aes(x = resid) + 
    geom_histogram(color = "black", fill = "grey70", binwidth = fd_bw) +
    geom_vline(xintercept = resid_cog, lty = 2) +
    labs(y = "Density", x = "Residuals") + 
    ds4ling::ds4ling_bw_theme() 

  p3 <- ds4ling::gg_qqplot(resid) + 
    ds4ling::ds4ling_bw_theme()


  print(p1 + p2 + p3)
}

diagnosis(mod2)
diagnosis(mod1)






# 4.
# No autocorrelation of residuals (important for time series data)
# When the residuals are autocorrelated, it means that the current value 
# is dependent of the previous values and that there is an unexplained 
# pattern in the Y variable that shows up

#
# How to check? 3 methods
#

# 4a. afc plot
acf(mod1$residuals)   # visual inspection
data(economics)       # bad example
bad_auto <- lm(pce ~ pop, data = economics)
acf(bad_auto$residuals)  # highly autocorrelated from the picture.

# 4b. Runs test
lawstat::runs.test(mod1$residuals)
lawstat::runs.test(bad_auto$residuals)

# 4c. Durbin-Watson test
lmtest::dwtest(mod1)
lmtest::dwtest(bad_auto)

#
# How to fix it?
#

# One option: Add lag1 as predictor and refit model
econ_data  <- data.frame(economics, resid_bad_auto = bad_auto$residuals)
econ_data1 <- slide(econ_data, Var = "resid_bad_auto", NewVar = "lag1", slideBy = -1)
econ_data2 <- na.omit(econ_data1)
bad_auto2  <- lm(pce ~ pop + lag1, data = econ_data2)

acf(bad_auto2$residuals)
lawstat::runs.test(bad_auto2$residuals)
lmtest::dwtest(bad_auto2)
summary(bad_auto2)

#
# What happened? Adding the lag variable removes the autocorrelation so now 
# we can interpret the parameter of interest.
#
# (you might never do this)
#










# 5. predictor and residuals are not correlated
# How to check? cor.test
cor.test(mtcars$wt, mod1$residuals)















# 6. 
# Normality of residuals
# (increasingly bad)
autoplot(mod1, which = 2)
autoplot(mod2, which = 2)
autoplot(bad_auto, which = 2)









#
# You can check some assumptions automatically
#
gvlma::gvlma(mod1)
gvlma::gvlma(mod2)
gvlma::gvlma(bad_auto)











# 0. Create project 'diagnostics'
# 1. add folders "slides", "scripts", "data"
# 2. save mtcars to "data" (write_csv)
# 3. load "mtcars" (read_csv)
# 4. walk through diagnostics in Rscript 
# 5. create ioslides
#   - sections
#   - lists
#   - bold, italics
#   - r chunkcs
# 6. add diagnostics to slides 
# 7. convert to slidify 
# 8. create repo, push, github pages




# install xaringan

---
title: "Presentation Ninja"
subtitle: "⚔<br/>with xaringan"
author: "Yihui Xie"
institute: "RStudio, PBC"
date: "2016/12/12 (updated: `r Sys.Date()`)"
output:
  xaringan::moon_reader:
    lib_dir: libs
    nature:
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---
