library(tidyverse); library(tidymodels)
library(vroom); library(skimr); library(DataExplorer); library(patchwork)
install.packages()

bike_train <- vroom('bike_train.csv') %>%
  mutate(season = as.factor(season),
         weather = as.factor(weather),
         holiday = as.factor(holiday),
         workingday = as.factor(workingday)) %>%
  mutate(time = #??? 
  )

bike_test <- vroom('bike_test.csv') %>%
  mutate(season = as.factor(season),
       weather = as.factor(weather),
       holiday = as.factor(holiday),
       workingday = as.factor(workingday))

DataExplorer::plot_intro(bike_train)
dplyr::glimpse(bike_train1)


# EDA ---------------------------------------------------------------------

bar_weather <- ggplot(bike_train, aes(x = weather)) +
  geom_bar()

bar_work <- ggplot(bike_train, aes(x = workingday)) +
  geom_bar()

point_temp <- ggplot(bike_train, aes(x = temp, y = count)) +
  geom_point()

point_humidity <- ggplot(bike_train, aes(x = humidity, y = count)) +
  geom_point()

(bar_weather + bar_work)/(point_temp + point_humidity)

# what do we do with the 1 observation in weather category 4?
bike_train2 <- bike_train1 %>%
  mutate()

# Linear Regression -------------------------------------------------------


linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = what R fn to use
  set_mode("regression") %>% # Regression just means quantitative response (as opposed to classification)
  fit(formula = log(count) ~ temp + weather + workingday, data = bike_train)
  
linear_model$fit

bike_predictions <- predict(linear_model,
                       new_data=bike_test) # Use fit to predict


# Poisson Regression ------------------------------------------------------

library(poissonreg)
pois_model <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression") %>%
  fit(data = bike_train, formula = count ~ temp + weather + workingday)

bike_predictions_pois <- predict(pois_model,
                            new_data=bike_test)

bike_predictions_pois


# Kaggle Format -----------------------------------------------------------

awful_kaggle_submission_pois <- bike_predictions_pois %>%
  bind_cols(., bike_test) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(awful_kaggle_submission_pois, "awful_kaggle_submission_pois.csv", delim = ",")
head(awful_kaggle_submission)

