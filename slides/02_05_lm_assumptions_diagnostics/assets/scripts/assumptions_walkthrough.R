# Install (if necessary):
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



