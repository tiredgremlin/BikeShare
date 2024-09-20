library(tidymodels)
library(poissonreg)
library(glmnet)
library(vroom)



# Workflows ---------------------------------------------------------------

bike_train_HW7 <- bike_train %>%
  select(-casual, -registered) %>%
  mutate(count = log(count))

bike_test_HW7 <- bike_test

## Define recipe
bike_recipe <- recipe(count ~ ., data = bike_train_HW7) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather, 
                               levels = c(1,2,3), 
                               labels = c('clear', 'mist', 'rain'))) %>%
  step_time(datetime, features = "hour") %>%
  step_date(datetime, features =  c("month")) %>%
  step_rm(datetime) %>%
  step_rm(holiday) %>%
  step_mutate(season = factor(season,
                              levels = c(1,2,3,4),
                              labels = c('spring','summer','fall','winter'))) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, bike_train_HW7) 

## Define model
preg_model1 <- linear_reg(penalty = .001, mixture = 0.24) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

preg_model2 <- linear_reg(penalty = .01, mixture = 0.24) %>% #Set model and tuning
  set_engine("glmnet")

preg_model3 <- linear_reg(penalty = .1, mixture = 0.24) %>% #Set model and tuning
  set_engine("glmnet")

preg_model4 <- linear_reg(penalty = .001, mixture = 0.76) %>% #Set model and tuning
  set_engine("glmnet")

preg_model5 <- linear_reg(penalty = .01, mixture = 0.81) %>% #Set model and tuning
  set_engine("glmnet")

preg_model6 <- linear_reg(penalty = .1, mixture = 0.76) %>% #Set model and tuning
  set_engine("glmnet")

?linear_reg
## Combine into workflow and fit
preg_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=bike_train_HW7)

## Run all the steps on the test data
preg_preds_workflow <- predict(preg_workflow, new_data = bike_test_HW7)

# Kaggle Format -----------------------------------------------------------

kaggle_submission_elastic <- exp(preg_preds_workflow) %>%
  bind_cols(., bike_test) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(kaggle_submission_elastic, "kaggle_sub_elastic.csv", delim = ",")
head(kaggle_submission_elastic)





# ChatGPT's idea ----------------------------------------------------------

library(tidymodels)
library(vroom)

# Define your models in a list for easier iteration
models <- list(
  model1 = linear_reg(penalty = .001, mixture = 0.24) %>% set_engine("glmnet"),
  model2 = linear_reg(penalty = .01, mixture = 0.24) %>% set_engine("glmnet"),
  model3 = linear_reg(penalty = .1, mixture = 0.24) %>% set_engine("glmnet"),
  model4 = linear_reg(penalty = .001, mixture = 0.76) %>% set_engine("glmnet"),
  model5 = linear_reg(penalty = .01, mixture = 0.76) %>% set_engine("glmnet"),
  model6 = linear_reg(penalty = .1, mixture = 0.76) %>% set_engine("glmnet")
)

# Iterate over models
for (model_name in names(models)) {
  preg_workflow <- workflow() %>%
    add_recipe(bike_recipe) %>%
    add_model(models[[model_name]])
  
  # Fit the model
  preg_fit <- fit(preg_workflow, data = bike_train_HW7)
  
  # Make predictions
  preg_preds_workflow <- predict(preg_fit, new_data = bike_test_HW7)
  
  # Create Kaggle submission
  kaggle_submission_elastic <- exp(preg_preds_workflow) %>%
    bind_cols(bike_test) %>%
    select(datetime, .pred) %>%
    rename(count = .pred) %>%
    mutate(count = pmax(0, count)) %>%
    mutate(datetime = as.character(format(datetime)))
  
  # Save the submission file
  vroom_write(kaggle_submission_elastic, paste0("kaggle_sub_", model_name, ".csv"), delim = ",")
}



# ---------

#  Iterate over models
for (model_name in names(models)) {
  preg_workflow <- workflow() %>%
    add_recipe(bike_recipe) %>%
    add_model(models[[model_name]])
  
  # Fit the model
  preg_fit <- fit(preg_workflow, data = bike_train_HW7)
  
  # Make predictions
  preg_preds_workflow <- predict(preg_fit, new_data = bike_test_HW7)
  
  # Check predictions
  print(paste("Predictions for", model_name))
  print(head(preg_preds_workflow))
  
}

