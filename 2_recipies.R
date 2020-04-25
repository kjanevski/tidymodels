library(tidymodels)
library(nycflights13)
library(skimr)
library(timeDate)

set.seed(123)

flight_data <-
  flights %>% 
  mutate(
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = as.Date(time_hour)
  ) %>% 
  inner_join(weather, by = c("origin","time_hour")) %>% 
  select(dep_time, flight, origin, dest, air_time, distance, carrier, date, arr_delay, time_hour) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)

flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

glimpse(flight_data)

flight_data %>% 
  skim(dest,carrier)

#data splitting
set.seed(555)
data_split <- initial_split(flight_data)

train_data = training(data_split)
test_data = testing(data_split)

#create a recipe
flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID")

#create features
flight_data %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date))

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow","month")) %>% 
  step_holiday(date, holidays = listHolidays("US")) %>% 
  step_rm(date) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors())

test_data %>% 
  distinct(dest) %>% 
  anti_join(train_data)

#fit model with recipe
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

#create workflow
flights_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)

flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)

flights_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

#use trained model to predict
flights_pred <- 
  predict(flights_fit, test_data, type = "prob") %>% 
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred %>% 
  roc_curve(truth = arr_delay, .pred_late) %>% 
  autoplot()

flights_pred %>% 
  roc_auc(truth = arr_delay, .pred_late)

#test against without the recipie
flights_wflow_formula <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_formula(arr_delay ~ .)

flights_fit2 <- 
  flights_wflow_formula %>% 
  fit(data = train_data)

flights_pred2 <- 
  predict(flights_fit2, test_data, type = "prob") %>% 
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred2 %>% 
  roc_curve(truth = arr_delay, .pred_late) %>% 
  autoplot()

flights_pred2 %>% 
  roc_auc(truth = arr_delay, .pred_late)
