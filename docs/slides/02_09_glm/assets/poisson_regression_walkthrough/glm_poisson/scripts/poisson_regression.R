library("tidyverse")
library("ds4ling")
library("sjPlot")

# 1. load data
ice_cream_poisson_data


# 2. check structure
glimpse()
summary()


# 3. fit inclusive and nested models
#    test for interactions/main effects
glm(units ~ temp, 
    data = ice_cream_poisson_data, 
    family = poisson(link = "log"))



# 4. summary of best model




# 5. write up of output



# 6. generate plots




