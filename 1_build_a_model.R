library(tidymodels)
library(readr)
library(tidyverse)

urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  setNames(c("food_regime","initial_volume","width")) %>% 
  mutate(food_regime = factor(food_regime, levels = c("Initial","Low","High")))

ggplot(urchins, aes(x = initial_volume,
                    y = width,
                    group = food_regime,
                    color = food_regime))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  scale_color_viridis_d(option = "plasma", end = .7)


lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

lm_fit <- 
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

tidy(lm_fit)

#Prediction
new_points <- expand.grid(initial_volume = 20,
                          food_regime = c("Initial","Low","High"))


mean_pred <- predict(lm_fit, new_data = new_points)

conf_int_pred <- predict(lm_fit, new_data = new_points, type = "conf_int")


plot_data <-
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

ggplot(plot_data, aes(x = food_regime)) +
  geom_point(aes(y=.pred)) +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = .2)+
  labs(y = "urchin size")


