library("tidyverse")
library("ds4ling")
library("sjPlot")
library("MuMIn")
library("lmerTest")

# 1. load data
ice_cream_poisson_data


# 2. check structure
glimpse(ice_cream_poisson_data)
str(ice_cream_poisson_data)
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

mod1 <- glm(
  units ~ 1 + temp,
  data = ice_cream_poisson_data,
  family = poisson(link = "log")
)
summary(mod1)

mod2 <- glm(
  units ~ 1 + temp + city,
  data = ice_cream_poisson_data,
  family = poisson(link = "log")
)
summary(mod2)

ice_cream_poisson_data$city %>% unique
anova(mod0, mod1, mod2, test = "Chisq")

# 4. Which is the best model?


# 5. Make a plot
p1 <- ggplot(ice_cream_poisson_data,
       aes(x = temp, y = units, color = city)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    method.args = list(family = "poisson"),
    formula = "y ~ x"
  )

ggsave(filename = "myplot.pdf", plot = p1)

tab_model(mod2)




#
# Mixed effects models
#

# Load LME4 package
library("lme4")

# Get to know the dataset
?sleepstudy

# Look at structure of data frame, get descriptives
glimpse(sleepstudy)
summary(sleepstudy)

sleepstudy %>%
  group_by(Days) %>%
  summarize(avg = mean(Reaction), sd = sd(Reaction),
            min = min(Reaction), max = max(Reaction))

# No-pooling model(s) (1 model for each participant)
no_pooling <- lmList(Reaction ~ Days | Subject, sleepstudy) %>%
  coef() %>%
  tibble::rownames_to_column("Subject") %>%
  rename(Intercept = `(Intercept)`, Slope_Days = Days)

no_pooling %>%
  ggplot(., aes(x = Slope_Days, y = Intercept)) +
    geom_point(color = "black")

# Lets fit a model using lm to one subject and compare
sleep_1 <- lm(Reaction ~ Days, data = filter(sleepstudy, Subject == 308))

# Let's fit a complete pooling model
complete_pooling <- lm(Reaction ~ Days, data = sleepstudy)
summary(complete_pooling)

# Random intercept model
rand_int <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(rand_int)

# Maximal model

max_mod <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
summary(max_mod)

summary(complete_pooling)$coef
summary(rand_int)$coef
summary(max_mod)$coef
r.squaredGLMM(max_mod)



#
# With a different dataset
#

# Get url and load data
path <- url("https://www.ds4ling.jvcasillas.com/workshops/scripts/polite.csv")
polite <- read_csv(path)

# Get to know the dataset: Look at structure of dataframe, get descriptives
glimpse(polite)
head(polite)

polite_tidy <- polite %>%
  separate(subject, into = c("gender", "id"), sep = 1, remove = F) %>%
  select(-id)

polite_tidy$gender %>% unique

polite_tidy %>%
  summarize(avg = mean(articulation_rate))

polite_tidy %>%
  group_by(gender, attitude) %>%
  summarize(avg = mean(articulation_rate))

# Exploratory plots
polite_tidy %>%
  ggplot() +
  aes(x = attitude, y = articulation_rate, fill = gender) +
  geom_boxplot(color = "black") +
  geom_point(aes(color = gender), position = position_dodge(0.75))

polite_tidy %>%
  ggplot() +
  aes(x = articulation_rate, fill = gender) +
  facet_grid(. ~ attitude) +
  geom_density(alpha = 0.5)

# Complete pooling model
mod_cp <- lm(
  articulation_rate ~ gender * attitude,
  data = polite_tidy
  )

summary(mod_cp)
plot_model(mod_cp, show.intercept = T)

# Random intercepts
mod_int <- lmer(
  articulation_rate ~ gender * attitude +
    (1 | subject),
  data = polite_tidy
  )

summary(mod_int)
plot_model(mod_int, show.intercept = T)


# Maximal model
mod_max <- lmer(
  articulation_rate ~ gender * attitude +
    (1 + attitude | subject),
  data = polite_tidy
)

summary(mod_max)
plot_model(mod_max, show.intercept = T)
tab_model(mod_max)



# Assess main effects, interactions

# Write up

# Individual differences
