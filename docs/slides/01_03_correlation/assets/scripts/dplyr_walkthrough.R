# Manipulate dataframes with dplyr
library("tidyverse")

# %>% : the pipe, sends information from the left to the right

mean(mtcars[, 'mpg'])

mtcars %>% 
  select(., mpg) %>%
  pull(.) %>%
  mean(.)

# select(): return a subset of the columns of a data frame

mtcars %>% 
  select(., mpg) %>%
  head()

# filter(): extract a subset of rows from a dataframe based on logical conditions

mtcars %>% 
  select(., mpg, vs, carb) %>%
  filter(., vs != 0 | carb == 4)


# arrange(): reorder rows of a dataframe

mtcars %>% 
  select(., mpg, vs, carb) %>% 
  arrange(., -mpg)


# rename(): rename variables in a data frame

mtcars %>% 
  select(., mpg, vs, carb) %>%
  rename(., miles_x_g = mpg)

# mutate(): add new variables/columns or transform existing variables

mtcars %>% 
  select(., mpg) %>% 
  arrange(., mpg) %>% 
  mutate(., mpg_std = (mpg - mean(mpg)) / sd(mpg)) %>% 
  pull(mpg_std) %>% 
  sd()

# summarise(): generate summary statistics of different variables in the dataframe

mtcars %>% 
  select(., mpg) %>% 
  summarize(., mpg = mean(mpg))

mtcars %>% 
  select(., mpg, carb) %>% 
  group_by(., carb) %>% 
  summarize(., mpg = mean(mpg)) %>% 
  ggplot(., aes(x = carb, y = mpg)) + 
    geom_bar(stat = 'identity') + 
    scale_x_continuous(breaks = 1:8, 
                       labels = c('one', 'two', 'three', 
                                 'four', 'five', 'six', 
                                 'seven', 'eight'))



# This is the first step in learning how to tidy data (we will work with tidyr 
# next week)

# Each verb uses the same structure function(data, var)
str(mtcars)




# 1. select all columns except for vs and mpg with 
#    the rows 1 through 10

mtcars %>% 
  select(., -mpg, -vs) %>% 
  slice(., 1:10)

# 2. Calculate the sd of 'hp' for each level of 'vs'

mtcars %>% 
  group_by(., vs) %>% 
  summarize(., hp_sd = sd(hp))

# 3. Select mpg, hp, vs, create a scatter plot 
#    of mpg ~ hp with different colors for vs

mtcars %>% 
  select(., mpg, hp, vs) %>% 
  ggplot(., aes(x = hp, y = mpg)) + 
    geom_point(aes(color = as.factor(vs))) + 
    geom_smooth(se = FALSE) + 
    scale_color_discrete(name = 'VS')


# 4. Plot the mean mpg for each level of vs, with a 
#    stat summary of mpg
     
mtcars %>% 
  ggplot(., aes(x = as.factor(vs), y = mpg)) + 
    stat_summary(fun.data = mean_cl_boot, geom = 'pointrange') + 
  ylim(0, 40)



