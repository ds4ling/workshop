# tidyr walk through 


# Remember the verbs we learned from dplyr

# Sometimes we will not be able to use them because our data is not in
# the correct format, tidy format

# tidy data

# What this means is that every column in your data frame 
# represents a variable and every row represents an observation. 
# In other words...

# Each variable is in a column.
# Each observation is a row.
# Each value is a cell.

# This is also referred to as long format (as opposed to wide format).

# Let's look at an example
# How many variables are there in this dataset?

my_data <- tribble(
  ~'id',    ~'spec', ~'test1', ~'test2', 
   'span01', 'g1_lo',  64.31,   69.2,
   'span02', 'g1_lo',  59.81,   63.7,
   'span03', 'g1_hi',  66.08,   70.9,
   'span04', 'g1_hi',  72.78,   79.2,
   'span05', 'g2_lo',  68.29,   75.4,
   'span06', 'g2_lo',  69.22,   76.7,
   'span07', 'g2_hi',  71.36,   77.2,
   'span08', 'g2_hi',  80.37,   88.9,
   'cata01', 'g1_lo',  75.63,   83.6,
   'cata02', 'g1_lo',  71.25,   78.8,
   'cata03', 'g1_hi',  69.09,   74.6,
   'cata04', 'g1_hi',  72.35,   80.7,
   'cata05', 'g2_lo',  71.66,   77.9,
   'cata06', 'g2_lo',  69.01,   75.0,
   'cata07', 'g2_hi',  69.86,   76.0,
   'cata08', 'g2_hi',  77.34,   85.6
) 

# We cannot do much with the data in this 
# format

my_data %>% 
  ggplot(., aes(x = test1, y = test2)) + 
    geom_point()

cor(my_data$test1, my_data$test2)
t.test(my_data$test1, my_data$test2, paired = TRUE)


# Here are the key verbs: 

# gather
#  - takes columns, and gathers them into key-value pairs
#  - this is one of the most important verbs for tidying data
#  - you need to tell R what names you will give to the new 
#    categorical factor and the new continuous variable

my_data_long <- my_data %>%
  gather(., key = test, val = score, -id, -spec)





# separate
#  - This verb is used to separate chars in a column
#  - essentially you create two variables from one column
#  - This is useful if you carefully planned your experimental 
#    outputs when you were collecting data (i.e. with you use the 
#    same number of chars for group names, common separators, etc.)

my_data_long %>% 
  separate(., col = spec, into = c('group', 'prof'), sep = "_") %>% 
  separate(., col = id, into = c('lang', 'id'), sep = 4) %>% 
  ggplot(., aes(x = group, y = score, fill = prof)) + 
    facet_grid(lang ~ test) + 
    geom_boxplot()












# spread
#  - This verb does the oposite of gather
#  - essentially you spread the data from long to wide
#  - this is useful if you already have tidy data and you need to 
#    make a scatter plot

my_data_long %>% 
  separate(., col = spec, into = c('group', 'prof'), sep = "_") %>% 
  separate(., col = id, into = c('lang', 'id'), sep = 4) %>% 
  spread(., key = lang, value = score) %>% 
  ggplot(., aes(x = cata, y = span, shape = test)) + 
    facet_grid(. ~ prof) + 
    geom_point()


  
  
  





# unite 
#  - You will rarely use this verb, I think
#  - Use it to create a single column from two columns
#  - I don't know if I have ever needed to use it

my_data_long %>% 
  separate(., col = spec, into = c('group', 'prof'), sep = "_") %>% 
  separate(., col = id, into = c('lang', 'id'), sep = 4) %>% 
  unite(., col = 'ident', lang, id, sep = "_", remove = F) %>% 
  select(-id) %>% View




# gather/separate 
# spread/unite































# We cannot do much with the data in this format

ggplot(my_data, aes(x = test1, y = test2)) + 
  geom_point()

t.test(my_data$test1, my_data$test2, paired = TRUE)


# Here are the key verbs: 

# gather
#  - takes columns, and gathers them into key-value pairs
#  - this is one of the most important verbs for tidying data
#  - you need to tell R what names you will give to the new 
#    categorical factor and the new continuous variable

my_data_long <- gather(my_data, test, score, -id, -spec) 

my_data_long %>% 
  ggplot(., aes(x = test, y = score)) + 
    geom_boxplot()

# separate
#  - This verb is used to separate chars in a column
#  - essentially you create two variables from one column
#  - This is useful if you carefully planned your experimental 
#    outputs when you were collecting data (i.e. with you use the 
#    same number of chars for group names, common separators, etc.)

my_data_tidy <- my_data_long %>% 
  separate(., col = spec, into = c("group", "level"), sep = "_") %>%
  separate(., col = id, into = c("lang", "id"), sep = 4)


# spread
#  - This verb does the oposite of gather
#  - essentially you spread the data from long to wide
#  - this is useful if you already have tidy data and you need to 
#    make a scatter plot

my_data_tidy %>% 
  spread(., lang, score) %>% 
  ggplot(., aes(x = cata, y = span, color = group, shape = level)) + 
    geom_point()


# unite 
#  - You will rarely use this verb, I think
#  - Use it to create a single column from two columns
#  - I don't know if I have ever needed to use it

my_data_tidy %>% 
  unite(., col = participant, lang, id, sep = "_", remove = FALSE) %>% 
  select(., -id)

# gather/separate 
# spread/unite













# Practice
# 1. Using the mtcars data set, select 'hp' and 'disp', and gather them 
#    into a tidy dataframe. You will need to think of names for the new 
#    grouping variable and the continuous variable. Next, pipe into ggplot 
#    and create a boxplot. 

mtcars %>% 
  select(., hp, disp) %>% 
  gather(., key = measurement, val = amount) %>% 
  ggplot(., aes(x = measurement, y = amount)) + 
    geom_boxplot()





# 2. Using the 'iris' dataset, select all columns except 'Sepal.Length' and 
#    'Sepal.Width'. Create a tidy dataset with the remaining variables and 
#    plot the continuous variable as a funtion of all grouping variables. 
#    HINT: think about the grouping variables first. How many are there? What 
#    should they be called? What is the most appropriate plot?

iris %>% 
  select(., -Sepal.Length, -Sepal.Width) %>% 
  gather(., key = measurement, val = numbers, -Species) %>% 
  ggplot(., aes(x = Species, y = numbers, fill = measurement, dodge = measurement)) + 
    geom_boxplot()


# 3. Using the 'iris' dataset again, select the 'Species', 'Petal.Length', and 
#    'Petal.Width' columns, filter out all Species except 'setosa', and create a 
#    scatterplot. Fit a line through the data. Is there a relationship? After 
#    eyeballing the plot calculate the Pearson's correlation coefficient. Is 
#    there a correlation? In what direction? Conduct a test of independence 
#    (test the null hypothesis that r is equal to 0). Bonus points if you can 
#    do it by hand.

z_score <- function(x) {
  (x - mean(x)) / sd(x)
}

iris_stand <- iris %>% 
  select(., Species, Petal.Length, Petal.Width) %>% 
  filter(., Species == 'setosa') %>% 
  mutate(., z_length = z_score(Petal.Length), 
            z_width = z_score(Petal.Width), 
            z_lw = z_length * z_width)

iris_stand %>% 
  ggplot(., aes(x = z_length, y = z_width)) + 
    geom_point() + 
    geom_smooth(method = 'lm')

cor(iris_stand$Petal.Length, iris_stand$Petal.Width)
cor.test(iris_stand$Petal.Length, iris_stand$Petal.Width)

sum(iris_stand$z_lw) / (nrow(iris_stand) - 1)















# Practice
# 1. Using the mtcars data set, select 'hp' and 'disp', and gather them 
#    into a tidy dataframe. You will need to think of names for the new 
#    grouping variable and the continuous variable. Next, pipe into ggplot 
#    and create a boxplot. 

mtcars %>% 
  select(., hp, disp) %>% 
  gather(., key = metric, val = val) %>% 
  ggplot(., aes(x = metric, y = val)) + 
    geom_boxplot()

# 2. Using the 'iris' dataset, select all columns except 'Sepal.Length' and 
#    'Sepal.Width'. Create a tidy dataset with the remaining variables and 
#    plot the continuous variable as a funtion of all grouping variables. 
#    HINT: think about the grouping variables first. How many are there? What 
#    should they be called? What is the most appropriate plot?

iris %>% 
  select(., -Sepal.Length, -Sepal.Width) %>% 
  gather(., measurement, val, -Species) %>% 
  ggplot(., aes(x = Species, y = val, color = measurement)) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange")

# 3. Using the 'iris' dataset again, select the 'Species', 'Petal.Length', and 
#    'Petal.Width' columns, filter out all Species except 'setosa', and create a 
#    scatterplot. Fit a 
#    line through the data. Is there a relationship? After eyeballing the plot 
#    calculate the Pearson's correlation coefficient. Is there a correlation? 
#    In what direction? Conduct a test of independence (test the null hypothesis 
#    that r is equal to 0). Bonus points if you can do it by hand.

iris_subset <- iris %>% 
  select(., Species, Petal.Length, Petal.Width) %>% 
  filter(., Species == 'setosa') %>% 
  mutate(., z_length = (Petal.Length - mean(Petal.Length)) / sd(Petal.Length), 
            z_width = (Petal.Width - mean(Petal.Width)) / sd(Petal.Width), 
            zl_zw = z_length * z_width)

iris_subset %>%
  ggplot(., aes(x = z_length, y = z_width)) + 
    geom_point() + 
    geom_smooth(method = 'lm')

sum(iris_subset$zl_zw) / (nrow(iris_subset) - 1)
cor(iris_subset$Petal.Length, iris_subset$Petal.Width)
cor.test(iris_subset$Petal.Length, iris_subset$Petal.Width)









