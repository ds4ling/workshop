# Source libs
source("./scripts/libs.R")

# Create count data
n_2     <- 200
beta0_a <- 0.09
beta1_a <- 0.06
beta0_b <- 0.75
beta1_b <- 0.06

x_a  <- seq(32, 110, length.out = n_2)
mu_a <- exp(beta0_a + beta1_a * x_a + rnorm(n_2, 0, 0.2))
y_a  <- rpois(n = n_2, lambda = mu_a) + 50

x_b  <- seq(32, 100, length.out = n_2)
mu_b <- exp(beta0_b + beta1_b * x_b + rnorm(n_2, 0, 0.2))
y_b  <- rpois(n = n_2, lambda = mu_b) + 50

tibble(temp = c(x_a, x_b), 
                city = rep(c("Tucson", "NYC"), each = n_2), 
                units =  c(y_a, y_b)) %>% 
  write_csv("./data/poisson_data.csv")
