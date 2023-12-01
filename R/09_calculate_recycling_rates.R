library(dplyr)
library(ggplot2)
library(scales)
library(lemon)

length_class_mods <- 
  readRDS("output/length_class_mods.rds")


# Make population estimate predictions from linear models of population estiamtes 
# Create a list to hold everything
output = list()

for (i in 1:nrow(length_class_mods)){
  
  # how far out should we make these predictions
  years <- 7
  
  # create the dataframe from which to make predcitions
  future <- 
    data.frame(location = rep(length_class_mods$location[[i]], years+1), 
               sp = rep(length_class_mods$sp[[i]], years+1), 
               year = seq(2023, 2023+years, by = 1))
  
  # Predict and add predictions to dataframe
  vals <- cbind(future, predict(length_class_mods$mod[[i]], newdata = future, interval = "prediction"))
  
  # Attach the predicted values to a data structure
  output[[i]] <- vals
  
}

# Collapse the predicted values into a dataframe from a list of dataframes
pop_est_preds <- 
  output %>%
  bind_rows()

pop_est_preds


pred_catch <- read.csv('output/08_predicted_annual_catch_mortality.csv', header = T)



pop_est_preds %>%
  filter(sp == 'Brown') %>%
  left_join(
    pred_catch %>%
      select(year, bnt_catch_lwr, bnt_catch_fit, bnt_catch_upr) %>%
      mutate(catch_lwr = bnt_catch_lwr/86.4, 
             catch_fit = bnt_catch_fit/86.4, 
             catch_upr = bnt_catch_upr/86.4) %>%
      select(year, catch_lwr, catch_fit, catch_upr) 
) %>%
  mutate(re_lower = catch_lwr/fit, 
         re_fit = catch_fit/fit, 
         re_upr = catch_upr/fit)