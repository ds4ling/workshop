# Create data for interaction examples ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setup
n  <- 1000
a1 <- 900
a2 <- 875
b1 <- -10
b2 <- -25

# Create 'pop' to sample from
x_pop <- seq(from = 1, to = 7, length.out = 10000) %>% round(., 2)

# Create criterion and predictor
x <- sample(x_pop, n)
y1 <- a1 + b1 * x + rnorm(n, 0, 50)
y2 <- a2 + b2 * x + rnorm(n, 0, 50)

# Create dataframe
rt_df <- tibble(Familiarity = x, rt_old = y1, rt_young = y2) %>% 
  gather(., Age, RT, -Familiarity) %>% 
  separate(., Age, into = c("delete", "Age")) %>% 
  select(., Age, Familiarity, RT) %>% 
  mutate(., RT = RT / n)




# Tidy spirantization data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spir_tidy <- untidydata::spirantization %>% 
  separate(., id, c('prof', 'id'), remove = F) %>% 
  dplyr::filter(., position == "vcv") %>% 
  mutate(., group = if_else(prof == "nat", "native", "learner"), 
            groupSum = if_else(group == "native", 1, -1), 
            groupDev = if_else(group == "native", 0.5, -0.5), 
            intensity_diff = vIntensity - cIntensity, 
            int_diff_c = intensity_diff - mean(intensity_diff)) 





# Generate data for model tuning examples ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Model setup for generating data
n           <- 40
intercept_a <- 15
covariate   <- rnorm(n, 12, 1)
y_a         <- intercept_a + (covariate * 0.5) + rnorm(n, 0, 2) - 2
intercept_b <- 1
y_b         <- intercept_a + (covariate * 1.5) + rnorm(n, 0, 2) - 14

# Create dataframe for example A
# y ~ x + b + x:b
# Includes centered, standardized continuous predictors
# and dummy, sum, and deviation coded categorical predictors
ex_a <- tibble(x = covariate, a = y_a, b = y_b) %>% 
  gather(., group, y, -x) %>% 
  mutate(., y_std = (y - mean(y)) / sd(y), 
            x_c = x - mean(x), 
            x_std = (x - mean(x)) / sd(x), 
            groupSum = if_else(group == "a", 1, -1), 
            groupDev = if_else(group == "a", 0.5, -0.5))


# Generate data for example B
intercept_c <- 8
y_c         <- intercept_c + (covariate * 0.1) + rnorm(n, 0, 2)
intercept_d <- 8
y_d         <- intercept_d + (covariate * 1.5) + rnorm(n, 0, 2)

# Create dataframe for example B
# y ~ x + b + x:b
# Includes centered, standardized continuous predictors
# and dummy, sum, and deviation coded categorical predictors
ex_b <- tibble(x = covariate, c = y_c, d = y_d) %>% 
  gather(., group, y, -x) %>% 
  mutate(., y_std = (y - mean(y)) / sd(y), 
            x_c = x - mean(x), 
            x_std = (x - mean(x)) / sd(x), 
            groupSum = if_else(group == "c", 1, -1), 
            groupDev = if_else(group == "c", 0.5, -0.5))

