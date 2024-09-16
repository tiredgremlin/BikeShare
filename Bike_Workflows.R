
# Workflows ---------------------------------------------------------------

bike_train_HW6 <- bike_train %>%
  select(-casual, -registered) %>%
  mutate(count = log(count))

bike_test_HW6 <- bike_test

## Define recipe
bike_recipe <- recipe(count ~ ., data = bike_train_HW6) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather, 
                               levels = c(1,2,3), 
                               labels = c('clear', 'mist', 'rain'))) %>%
  step_time(datetime, features = "hour") %>%
  step_date(datetime, features =  c("month")) %>%
  step_rm(datetime) %>%
  step_rm(holiday) %>%
  # step_mutate(windspeed = ifelse(windspeed > 25, 1, 0)) %>%
  # step_mutate(windspeed = factor(windspeed,
  #                                levels = c(0,1),
  #                                labels = c('low','high'))) %>%
  step_mutate(season = factor(season,
                              levels = c(1,2,3,4),
                              labels = c('spring','summer','fall','winter')))

View(bike_train_HW6)
prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, bike_train_HW6) 

## Define model
pois_model_HW6 <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression")

lin_model_HW6 <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


## Combine into workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model_HW6) %>%
  fit(data=bike_train_HW6)

## Run all the steps on the test data
lin_preds_workflow <- predict(bike_workflow, new_data = bike_test_HW6)

# Kaggle Format -----------------------------------------------------------

kaggle_submission_workflow <- exp(lin_preds_workflow) %>%
  bind_cols(., bike_test) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(kaggle_submission_workflow, "kaggle_submission_workflow.csv", delim = ",")
head(kaggle_submission_workflow)

