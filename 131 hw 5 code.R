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
## stratify on outcome variable type_1
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
## use mutlinom_reg with glment engine to
## tune and fit an elastic net

# set up model
multi_reg <- multinom_reg(penalty = tune(),
                          mixture = tune()) %>%
  set_engine("glmnet")
# set up workflow
multi_wkflow <- workflow() %>%
  add_model(multi_reg) %>%
  add_recipe(pokemon_recipe)
# regular grid for penalty and mixture 
# 10 levels each 
pokemon_grid <- grid_regular(penalty(range=c(-5,5)),
                             mixture(range(c(0,1))),
                             levels = 10)
pokemon_grid
?grid_regular

# QUESTION 6
## fit the models to your folded data via tune_grid()
tune_res <- tune_grid(
  multi_wkflow,
  resamples = pokemon_folds,
  grid = pokemon_grid)
# plot the results via autoplot()
autoplot(tune_res)

# QUESTION 7
# choose model with optimal roc_auc via select_best()
best_model <- select_best(tune_res,
                          metric = "roc_auc")
best_model

# fit the model to the training set 
# finalize_workflow(), fit(), and augment()
final_model <- finalize_workflow(multi_wkflow, best_model)
final_model
final_model_fit <- fit(final_model, data=pokemon_train)

## QUESTION 8
# get overall roc_auc value
augment(final_model_fit, new_data = pokemon_test) %>%
  roc_auc(truth = type_1, estimate = c(.pred_Bug, 
                                       .pred_Fire, .pred_Grass, .pred_Normal,
                                       .pred_Psychic, .pred_Water))
# plots of different roc curves per level
augment(final_model_fit, new_data = pokemon_test) %>%
  roc_curve(type_1, estimate = c(.pred_Bug, 
            .pred_Fire, .pred_Grass, .pred_Normal,
            .pred_Psychic, .pred_Water)) %>%
  autoplot()
# confusion matrix
augment(final_model_fit, new_data = pokemon_test) %>%
  conf_mat(type_1, estimate = .pred_class) %>%
  autoplot(type="heatmap")


