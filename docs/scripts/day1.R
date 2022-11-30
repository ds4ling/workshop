# Load packages
library(tidyverse)
library(ds4ling)

# Create a vector of numbers
my_data <- c(2, 5, 8)

my_data
sum(my_data)
mean(my_data)
sd(my_data)

# Ttests
ind_samp <- tribble(
  ~'name', ~'group', ~'score', 
  'John',  'g1',      250, 
  'Jane',  'g1',      340, 
  'Jimmy', 'g2',      460, 
  'Jessy', 'g2',      200)

# t-test: # independent 2-group
t.test(score ~ group, data = ind_samp)

prd_samp <- tribble(
  ~'name',     ~'test1', ~'test2', 
  'Mike',      35,       67, 
  'Melanie',   50,       46, 
  'Melissa',   90,       86, 
  'Mitchell',  78,       91)

t.test(prd_samp$test1, prd_samp$test2, 
       paired = TRUE)


prd_samp_long <- prd_samp %>% 
  pivot_longer(cols = -name, names_to = "test", 
               values_to = "score")

t.test(score ~ test, data = prd_samp_long, paired = TRUE)


cor(mtcars$mpg, mtcars$disp)
cor.test(mtcars$mpg, mtcars$disp)

data(test_scores_rm)
test_scores_rm

test_scores_rm %>% 
  ggplot(., aes(x = test1, y = test2)) + 
  geom_point() + 
  geom_smooth(method = "lm")

cor(test_scores_rm$test1, test_scores_rm$test2)
cor.test(test_scores_rm$test1, test_scores_rm$test2)
t.test(test_scores_rm$test1, test_scores_rm$test2)


test_scores_long <- test_scores_rm %>% 
  pivot_longer(cols = -c("id", "spec"), names_to = "test", 
               values_to = "score")

test_scores_long %>% 
  ggplot(., aes(x = test, y = score)) + 
    geom_point(alpha = 0.2) + 
    stat_summary(fun.data = mean_se, geom = "pointrange")

mtcars %>% 
  ggplot(., aes(x = hp, y = mpg))+ 
  geom_point()

cor.test(mtcars$mpg, mtcars$hp)
t.test(mtcars$mpg, mtcars$hp)
