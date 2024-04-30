#
# Calculates catch by river km and recycling rates, BUT NOT from tagged populations
#

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
  
  # create the dataframe from which to make predictions
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

pop_est_preds_img <- 
  pop_est_preds %>%
  ggplot() +
  geom_line(aes(x = year, y = fit), linetype = 2) +
  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr), alpha = 0.5) +#,
  # geom_smooth(aes(x = year, y = n_hat_km), linetype = 1, color = 'black', se = FALSE, level = 0.95, method = 'lm', formula = y~x) +
  facet_rep_grid(location~sp) +
  # lims(y = c(0, 30000)) +
  xlab('Year') +
  ylab(expression("Predicted population estimate ("~hat("N")~")/km")) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom', 
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )

pop_est_preds_img

ggsave(paste0("output/images/09_pop_est_preds.png"), plot = pop_est_preds_img, 
       width = 16, height = 9, bg = "white")


pred_catch <- 
  read.csv('output/08_predicted_annual_catch_mortality.csv', header = T)


# Calculates catch by rkm and recycling rate by species, by location, by year
brown_recycling_rate <- 
  pop_est_preds %>%
  filter(sp == 'Brown') %>%
  left_join(
    pred_catch %>%
      select(year, bnt_catch_lwr, bnt_catch_fit, bnt_catch_upr) %>%
      mutate(catch_lwr = bnt_catch_lwr/86.4, #If total predicted catch were spread across all 
             catch_fit = bnt_catch_fit/86.4, # 86.4 total rkms of the upper Madison, this is by rkm
             catch_upr = bnt_catch_upr/86.4) %>% # catch
      select(year, catch_lwr, catch_fit, catch_upr) 
) %>%
  mutate(re_lower = catch_lwr/fit, 
         re_fit = catch_fit/fit, 
         re_upr = catch_upr/fit)

brown_recycling_rate

rainbow_recycling_rate <- 
  pop_est_preds %>%
  filter(sp == 'Rainbow') %>%
  left_join(
    pred_catch %>%
      select(year, rbt_catch_lwr, rbt_catch_fit, rbt_catch_upr) %>%
      mutate(catch_lwr = rbt_catch_lwr/86.4, 
             catch_fit = rbt_catch_fit/86.4, 
             catch_upr = rbt_catch_upr/86.4) %>%
      select(year, catch_lwr, catch_fit, catch_upr) 
  ) %>%
  mutate(re_lower = catch_lwr/fit, 
         re_fit = catch_fit/fit, 
         re_upr = catch_upr/fit)

rainbow_recycling_rate
