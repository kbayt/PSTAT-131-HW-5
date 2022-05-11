library(ggplot2)
library(tidyverse)
library(tidymodels)
library(corrplot)
library(ggthemes)
tidymodels_prefer()
library(ISLR)
library(yardstick)
library(corrr)
library(discrim)
library(poissonreg)
library(klaR)
pokemon <- read.csv("C:\\Pokemon.csv")
view(pokemon)
library(janitor)
set.seed(4857)

# QUESTION 1
# does not seem to do anything?
pokemon_clean <- pokemon %>%
  clean_names()

# QUESTION 2
pokemon_clean %>%
  ggplot(aes(x=forcats::fct_infreq(type_1))) +
  geom_bar() + 
  coord_flip()
# 18 outcomes 
# smallest: flying, fairy
# filter type_1 
pokemon_clean <- pokemon_clean %>%
  filter(type_1 == "Bug" | type_1 == "Fire" |
           type_1 == "Grass" | type_1 == "Normal" |
           type_1 == "Water" | type_1 == "Psychic")

# convert type_1 and legendary to factors
pokemon_clean$type_1 <- factor(pokemon_clean$type_1)
pokemon_clean$legendary <- factor(pokemon_clean$legendary)

# QUESTION 3
## initial split of data
## strtify on outcome variable type_1
pokemon_split <- initial_split(pokemon_clean, prop = 0.7,
                               strata = type_1)
pokemon_train <- training(pokemon_split)
pokemon_test <- testing(pokemon_split)
# 318
dim(pokemon_train)
# 140
dim(pokemon_test)

## use v-fold cross validation with 5 folds
## stratify by type_1
pokemon_folds <- vfold_cv(pokemon_train, v=5, 
                          strata = type_1)
pokemon_folds

# QUESTION 4
## set up recipe 
pokemon_recipe <- recipe(type_1 ~ legendary +
                           generation + sp_atk +
                           attack + speed + defense +
                           hp + sp_def, data = pokemon_train) %>%
  step_dummy(legendary, generation) %>%
  step_normalize(all_predictors())
summary(pokemon_recipe)  

# QUESTION 5