set.seed(1)

n <- 10
k <- 20
sub_int <- rnorm(10, mean = 20, sd = 12)

# add an effect
effect1 <- rep(1:20 * 2, times = n)
effect2 <- rnorm(10, mean = 0, sd = 1)
effect2 <- rep(effect2, times = k)


my_df <- tibble(
  subjects = rep(c(sprintf("p_%02d", 1:n)), each = k), 
  time = rep(1:20, times = n),  # add condition
  sub_int = rep(sub_int, each = k),  # give subj a random intercept
  effect = rep(1:20 * 2, times = n), # add an effect
  noise = rnorm(length(subjects), sd = 6) # add some noise
) %>% 
  mutate(., response = sub_int + effect + noise)


library(lme4)
mod_int <- lmer(response ~ time + 
               (1|subjects), 
               data = my_df)
mod_slp <- lmer(response ~ time + 
               (1 + time|subjects), 
               data = my_df)

ran_ints <- ranef(mod_int) %>% 
  as.data.frame(.) %>% 
  select(grp, condval) %>% 
  mutate(., subjects = grp, 
            intercepts = condval + 22, 
            slopes = fixef(mod_int)[2]) %>% 
  select(subjects, intercepts, slopes)
