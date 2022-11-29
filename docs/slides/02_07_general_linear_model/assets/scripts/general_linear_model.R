
# vocab_df used in correlation slides
# For reproducibility
set.seed(234532)

# Fit details
n       <- 2e3   # 2k participants per group
beta0   <- 4.5   # yintercept @ 4 (youngest age)
beta1_0 <- 1100  # change in vocab size per year
sigma_0 <- 1.7e3 # avoid -vocab size, but not too perfect

beta1_1 <- 1480  # change in vocab size per year
sigma_1 <- 1.7e3 # avoid -vocab size, but not too perfect

# x and y vars
ages    <- seq(from = 4, to = 15, length.out = n)
vocab_0 <- beta0 + ages * beta1_0 + rnorm(n = n, sd = sigma_0)
vocab_1 <- beta0 + ages * beta1_1 + rnorm(n = n, sd = sigma_1)

# Create df
vocab_df <- data.frame(age = c(ages, ages), 
                       vocab = c(vocab_0, vocab_1)) %>% 
  mutate(., count = row_number(), 
            reader = if_else(count >= length(count) / 2, 
                            true = "frequent", false = "average"))











