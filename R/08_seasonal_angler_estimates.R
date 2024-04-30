#
# Simulates every summer of fishing activity from 2023 through 2040
#

library(AnglerCreelSurveySimulation) # Simulates creel surveys
library(ggplot2)
library(dplyr)
library(tidyr)

summer = as.numeric(as.Date('2023-08-31') - as.Date("2023-06-01"))
season_length = summer # days
summer_prop = 0.67 # proportion of all upper-river users

# From madison river pressure survey -- upper river proportion of surveys from Skaar == 58%
# angler_days = 58716 * .58
# angler_days = 400000 * .58 # predicted in 2030
angling_est <-
  readRDS('./data/07_predicted_angler_pressure.rds') %>%
  filter(year >= 2023 & year <= 2030) %>% 
  select(year, fit, lwr, upr) %>% 
  pivot_longer(!year, names_to = 'type', values_to = 'n_anglers') %>%
  mutate(n_upper_anglers = n_anglers * summer_prop, 
         bnt_catch = NA, 
         rbt_catch = NA)

trip_length <- 5.6
day_length <- 11

###############################################################################
# For each lwr, fit, upr of predicted anglers, calculate mortality of BNT and RBT separately
# FOR BROWN TROUT
# Catch rate = 0.38
for (i in 1:nrow(angling_est)){
  
  sim <- conduct_multiple_surveys(
    n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
    start_time = 0, # start time of fishing day
    wait_time = 11, #end time of fishing day
    n_sites = 1, #number of sites sampled, all of upper Madison River
    n_anglers = (angling_est$n_upper_anglers[i]*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
    sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
    mean_catch_rate = 0.38, # catch rate of trout -- 0.38
    fishing_day_length = 11, # total length of fishing day
    mean_trip_length = trip_length) # average trip length
  
  angling_est$bnt_catch[i] <- sum(sim$true_catch)
}

# FOR RAINBOW TROUT
# Catch rate = 0.64
for (i in 1:nrow(angling_est)){
  
  sim <- conduct_multiple_surveys(
    n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
    start_time = 0, # start time of fishing day
    wait_time = 11, #end time of fishing day
    n_sites = 1, #number of sites sampled, all of upper Madison River
    n_anglers = (angling_est$n_upper_anglers[i]*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
    sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
    mean_catch_rate = 0.64, # catch rate of trout -- 0.64
    fishing_day_length = 11, # total length of fishing day
    mean_trip_length = trip_length) # average trip length
  
  angling_est$rbt_catch[i] <- sum(sim$true_catch)
}


# Produces table of estimated catch and along with total mortality estimates
angling_est %>%
  mutate(Brown = bnt_catch * 0.02, 
         Rainbow = rbt_catch * 0.08) %>%
  select(year, type, bnt_catch, rbt_catch, Brown, Rainbow) %>% 
  mutate(bnt_catch = round(bnt_catch, 0), 
         rbt_catch = round(rbt_catch, 0), 
         Brown = round(Brown, 0), 
         Rainbow = round(Rainbow, 0), 
  ) %>%
  pivot_wider(
    names_from = type, 
    values_from = c(bnt_catch, rbt_catch, Brown, Rainbow)) %>% 
  select(year, bnt_catch_lwr, bnt_catch_fit, bnt_catch_upr, 
         rbt_catch_lwr, rbt_catch_fit, rbt_catch_upr, 
         Brown_lwr, Brown_fit, Brown_upr, 
         Rainbow_lwr, Rainbow_fit, Rainbow_upr) %>%
  write.csv(paste0("output/08_predicted_annual_catch_mortality.csv"))














