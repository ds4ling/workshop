## @knitr create_assumptions_data

set.seed(1)
n <- 50
b0 <- 0
b1 <- -2
sigma <- 0.6

assumptions_df <- tibble(
    x = sort(rnorm(n = n)), 
    y =      b0 + (x * b1)   + rnorm(n = n, sd = sigma), 
    y_quad = b0 + (x * b1)^2 + rnorm(n = n, sd = sigma)
  ) %>% 
  add_row(., x = -2, y = -2, y_quad = 7)


# http://r-statistics.co/Assumptions-of-Linear-Regression.html
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_lm.html
