library(AnglerCreelSurveySimulation)
library(ggplot2)
library(dplyr)

summer = as.numeric(as.Date('2023-08-31') - as.Date("2023-06-01"))

angling_est <-
  readRDS('./data/predicted_angler_pressure.rds')

angler_days <- angling_est %>% 
  filter(year > 2022 & year <= 2030) %>%
  select(year, fit, lwr, upr)# estiamted from predicted_angler_pressure.rds

angler_days

summer_prop <-.67 # proportion of all madison pressure that happens in summer, 2020 fishing pressure survey, 

# 
# 
# 
# sim <- conduct_multiple_surveys(
#   n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
#   start_time = 0, # start time of fishing day
#   wait_time = 11, #end time of fishing day
#   n_sites = 1, #number of sites sampled, all of upper Madison River
#   n_anglers = (angler_days %>% filter(year == 2023) %>% pull(fit)*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
#   sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
#   mean_catch_rate = 0.88, # mean catch rate of trout -- RBT = 0.68
#   fishing_day_length = 11, # total length of fishing day
#   mean_trip_length = 5.6) # average trip length
# 
# 
# (sum(sim$true_catch)/2) * seq(0.02, 0.1, by = 0.005)
# 
# angler_days %>% filter(year <= 2030) %>% nrow()


# Create an empty dataframe to store mortality results from simualtions
empty <- data.frame(year = rep(NA, 8), bnt = rep(NA, 8), bnt_1 = rep(NA, 8), rbt = rep(NA, 8), rbt_1 = rep(NA, 8))

# Add to an empty list so I can store mortality values from fitted, lower, and upper values
mortality_list <- list(fit = empty, lwr = empty, upr = empty)

# For fitted values
for (i in 1:length(angler_days$year)){
  
  sim <- conduct_multiple_surveys(
    n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
    start_time = 0, # start time of fishing day
    wait_time = 11, #end time of fishing day
    n_sites = 1, #number of sites sampled, all of upper Madison River
    n_anglers = (angler_days %>% filter(year == angler_days$year[i]) %>% pull(fit)*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
    sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
    mean_catch_rate = 0.88, # mean catch rate of trout -- RBT = 0.68
    fishing_day_length = 11, # total length of fishing day
    mean_trip_length = 5.6) # average trip length
  
  
  mortality_list$fit[i, ] <- c(angler_days$year[i], (sum(sim$true_catch)/2) * c(0.02, 0.03, 0.08, 0.09))
}

# For lower values
for (i in 1:length(angler_days$year)){
  
  sim <- conduct_multiple_surveys(
    n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
    start_time = 0, # start time of fishing day
    wait_time = 11, #end time of fishing day
    n_sites = 1, #number of sites sampled, all of upper Madison River
    n_anglers = (angler_days %>% filter(year == angler_days$year[i]) %>% pull(lwr)*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
    sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
    mean_catch_rate = 0.88, # mean catch rate of trout -- RBT = 0.68
    fishing_day_length = 11, # total length of fishing day
    mean_trip_length = 5.6) # average trip length
  
  
  mortality_list$lwr[i, ] <- c(angler_days$year[i], (sum(sim$true_catch)/2) * c(0.02, 0.03, 0.08, 0.09))
}

#for upr values
for (i in 1:length(angler_days$year)){
  
  sim <- conduct_multiple_surveys(
    n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
    start_time = 0, # start time of fishing day
    wait_time = 11, #end time of fishing day
    n_sites = 1, #number of sites sampled, all of upper Madison River
    n_anglers = (angler_days %>% filter(year == angler_days$year[i]) %>% pull(upr)*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
    sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
    mean_catch_rate = 0.88, # mean catch rate of trout -- RBT = 0.68
    fishing_day_length = 11, # total length of fishing day
    mean_trip_length = 5.6) # average trip length
  
  
  mortality_list$upr[i, ] <- c(angler_days$year[i], (sum(sim$true_catch)/2) * c(0.02, 0.03, 0.08, 0.09))
}


lapply(
  mortality_list,
  function(df) {
    foo <- df %>% 
      pivot_longer(cols = c('bnt', 'bnt_1', 'rbt', 'rbt_1'))
    names <- names(foo)
    foo %>% 
      ggplot() +
      aes_string(x=names[1], y=names[3], colour=names[2]) + 
      geom_line()
  }
)



