library(tidyverse)
library(tidymodels)
library(vroom); library(skimr); library(DataExplorer); library(patchwork)
install.packages()

bike_train <- vroom('bike_train.csv')
# bike_test <- vroom('bike_test.csv')

DataExplorer::plot_intro(bike_train)
dplyr::glimpse(bike_train)

bike_train1 <- bike_train %>%
  mutate(season = as.factor(season),
         weather = as.factor(weather),
         holiday = as.factor(holiday),
         workingday = as.factor(workingday))


bar_weather <- ggplot(bike_train1, aes(x = weather)) +
  geom_bar()

bar_season <- ggplot(bike_train1, aes(x = season)) +
  geom_bar()

point_temp <- ggplot(bike_train1, aes(x = temp, y = count)) +
  geom_point()

point_humidity <- ggplot(bike_train1, aes(x = humidity, y = count)) +
  geom_point()

(bar_weather + bar_season)/(point_temp + point_humidity)
