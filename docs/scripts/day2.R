# Install (if necessary):
# - ggfortify
# - lawstat
# - lmtest
# - DataCombine
# - gvlma

# Load libs
library("tidyverse")
library("ds4ling")
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
# The mean of residuals is zero
# How to check?: Check model summary and test manually
summary(mod1)
mean(mod1$residuals)










# 3.
# Homoscedasticity of residuals or equal variance
# How to check?
autoplot(mod1, which = c(1, 3))

# What are you looking for? The line should be more or less flat
autoplot(mod2, which = c(1, 3))

# Also
ds4ling::diagnosis(mod2)
ds4ling::diagnosis(mod1)






# 4.
# No autocorrelation of residuals (important for time series data)
# When the residuals are autocorrelated, it means that the current value
# is dependent of the previous values and that there is an unexplained
# pattern in the Y variable that shows up

#
# How to check? 3 methods
#
data(economics)       # bad example
bad_auto <- lm(pce ~ pop, data = economics)

# 4a. Runs test
lawstat::runs.test(mod1$residuals)
lawstat::runs.test(bad_auto$residuals)

# 4b. Durbin-Watson test
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
# (you'll probably never do this, but it exists)
#










# 5. predictor and residuals are not correlated
# How to check? cor.test
cor.test(mtcars$wt, mod1$residuals)















# 6.
# Normality of residuals
# QQPlots, histograms
ds4ling::diagnosis(mod2)
ds4ling::diagnosis(mod1)









#
# You can check some assumptions automatically
# But this is probably overkill
gvlma::gvlma(mod1)
gvlma::gvlma(mod2)
gvlma::gvlma(bad_auto)





library(ds4ling)
set.seed(20221201)
vocab_sample <- sample_n(vocab_data, size = 200)

mod1 <- lm(vocab ~ ages, data = vocab_sample)
summary(mod1)

mod2 <- lm(vocab ~ ages + reader_type, data = vocab_sample)
summary(mod2)
summary(mod2$residuals)

mod3 <- lm(vocab ~ ages * reader_type, data = vocab_sample)
anova(mod2, mod3)
summary(mod3)

#


















#
# If theres time (or later at home)
#


library(tidyverse)
library(ds4ling)
library(plot3D) # install first

glimpse(language_diversity)
head(language_diversity)

ld <- language_diversity %>%
  filter(., Continent == "Africa") %>%
  pivot_wider(names_from = "Measurement", values_from = "Value")

ld %>%
  ggplot(., aes(x = Population, y = Langs, color = Area, label = Country)) +
  geom_text() +
  geom_smooth(method = lm)

my_mod <- lm(Langs ~ Area + Population, data = ld)
summary(my_mod)
plot(my_mod, which = 1:4)
ds4ling::diagnosis(my_mod)

ld <- ld %>%
  mutate(., logPop = log(Population),
         logArea = log(Area))

hist(ld$Population)
hist(ld$logPop)
hist(ld$Area)
hist(ld$logArea)

ld %>%
  ggplot(., aes(x = logPop, y = Langs, color = logArea, label = Country)) +
  geom_text() +
  geom_smooth(method = lm)

# Fit a multiplicative model (number of languages as a function of lopPop and
# logArea)

# For fun
x <- ld$logPop
y <- ld$logArea
z <- ld$Langs

plot3D::scatter3D(x, y, z,
                  pch = 21, cex = 1, expand = 0.75, colkey = F,
                  theta = 45, phi = 20, ticktype = "detailed",
                  xlab = "logPop", ylab = "Area", zlab = "Langs")







# NMC examples using forward selection ----------------------------------------

# 1. From the mtcars dataframe select the columns 'mpg', 'wt', and 'drat' and
#    assign this subset of columns to a new object called 'my_cars'


# One you have done that run the following models:
mod_null <- lm(mpg ~ 1                  , data = my_cars)
mod_wt   <- lm(mpg ~ wt                 , data = my_cars)
mod_add  <- lm(mpg ~ wt + drat          , data = my_cars)
mod_int  <- lm(mpg ~ wt + drat + wt:drat, data = my_cars)

# 2. Look at the summary of 'mod_null'. What does the intercept tell you?


# 3. Look at the summary of 'mod_wt'. What does the summary tell you?


# 4. Use a nested model comparison to test the additive effect of 'drat'.
#    (hint: use the anova() function)


# 5. Test the interaction term using a NMC and write out the important info in
# a comment below.


# 6. Run the anova() function on the multiplicative model. Compare with (5).


# -----------------------------------------------------------------------------
