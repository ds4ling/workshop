library("tidyverse")
library("ds4ling")
library("sjPlot")

# 1. load data
ice_cream_poisson_data


# 2. check structure
glimpse()
summary()


# 3. fit the following models and test for main effects:
# - intercept-only model
# - temperature (temp) model
# - temperature/city additive model

mod0 <- glm(
  units ~ 1,
  data = ice_cream_poisson_data,
  family = poisson(link = "log")
  )

summary(mod0)


# 4. Which is the best model?


# 5. Make a plot
ggplot(ice_cream_poisson_data, aes(x = XXX, y = XXX, color = XXX)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    method.args = list(family = "poisson"),
    formula = "y ~ x"
  )

tab_model(MODEL)




#
# Mixed effects models
#

# Load LME4 package
library("lme4")

# Get to know the dataset
?sleepstudy

# Look at structure of data frame, get descriptives

# No-pooling model(s) (1 model for each participant)
no_pooling <- lmList(Reaction ~ Days | Subject, sleepstudy) %>%
  coef() %>%
  tibble::rownames_to_column("Subject") %>%
  rename(Intercept = `(Intercept)`, Slope_Days = Days)

# Lets fit a model using lm to one subject and compare
sleep_1 <- lm(Reaction ~ Days, data = filter(sleepstudy, Subject == 308))

# Let's fit a complete pooling model
complete_pooling <- lm(Reaction ~ Days, data = sleepstudy)

# Random intercept model
rand_int <- lmer()
summary(rand_int)

# Maximal model
max_mod <- lmer()






#
# With a different dataset
#
path <- url("https://www.ds4ling.jvcasillas.com/workshops/scripts/polite.csv")
polite <- read_csv(path)


polite %>%
  select(articulation_rate, attitude, task, subject) %>%
  write_csv(here("scripts", "polite.csv"))


# Get to know the dataset
?pre_post

# Look at structure of data frame, tidy, get descriptives
hold <- pre_post %>%
  separate(spec, into = c("group", "level"), sep = "_") %>%
  separate(id, into = c("lang", "id"), sep = 4) %>%
  pivot_longer(cols = c("test1", "test2"),
               names_to = "time",
               values_to = "score")

hold %>%
  filter(lang == "span") %>%
  group_by(group, time) %>%
  summarize(avg = mean(score))


# No-pooling model(s) (1 model for each participant)
