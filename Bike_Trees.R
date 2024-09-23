library(tidymodels)
library(poissonreg)
library(vroom)

bike_train_HW7 <- bike_train %>%
  select(-casual, -registered) %>%
  mutate(count = log(count))

bike_test_HW7 <- bike_test

## Tree model
my_tree <- decision_tree(tree_depth = tune(),
                         cost_complexity = tune(),
                         min_n = tune()) %>% #Type of model
  set_engine("rpart") %>%
  set_mode("regression")


# Define recipe
bike_recipe <- recipe(count ~ ., data = bike_train_HW7) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather, 
                               levels = c(1,2,3), 
                               labels = c('clear', 'mist', 'rain'))) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(datetime_hour = factor(datetime_hour)) %>%
  step_date(datetime, features =  c("month")) %>%
  step_mutate(datetime_month = factor(datetime_month)) %>%
  step_rm(datetime) %>%
  step_rm(holiday) %>%
  step_mutate(season = factor(season,
                              levels = c(1,2,3,4),
                              labels = c('spring','summer','fall','winter'))) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, bike_train_HW7) 

tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_tree)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 4) ## 5 each, L^3 total tuning parameters
View(grid_of_tuning_params)

## Split data for CV
folds <- vfold_cv(bike_train_HW7, v = 4, repeats = 1) # v is number of groups

## Run the CV
CV_results <- tree_wf %>%
  tune_grid(resamples = folds, 
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse)) # mae, rsq, or leave metrics NULL

View(CV_results)
## Plot Results
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=='rmse') %>%
  ggplot(data = ., aes(x = cost_complexity, y = mean, color = factor(mixture))) +
  geom_line()

## Find best tuning parameters

bestTune <- CV_results %>%
  select_best(metric = 'rmse')

## Finalize the workflow & fit it
final_wf <-
  tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = bike_train_HW7)

## Predict
tree_preds <- predict(final_wf, new_data = bike_test_HW7)



## kaggle
kaggle_sub_tree <- exp(tree_preds) %>%
  bind_cols(., bike_test) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(kaggle_sub_tree, "kaggle_sub_tree.csv", delim = ",")
head(kaggle_sub_tree)
