library(AnglerCreelSurveySimulation)
library(ggplot2)
library(dplyr)

summer = as.numeric(as.Date('2023-08-31') - as.Date("2023-06-01"))
season_length = summer # days

# From madison river pressure survey -- upper river proportion of surveys from Skaar == 58%
angler_days = 208542 * .58
trips = 1597 * .58

angler_days/season_length
summer

# increase angler days by 36% SW mt population and 20% of those as anglers
new_angler_days = angler_days+(angler_days*.24*c(.1, .2, .5))
new_angler_days[3]


#multiple simulations = 100 days? - season == 90 days, 

sim <- conduct_multiple_surveys(n_sims = season_length, start_time = 0, wait_time = 12, n_sites = 1,
                                n_anglers = new_angler_days[3]/season_length, sampling_prob = 1, 
                                mean_catch_rate = 0.88, fishing_day_length = 12, mean_trip_length = 5.43)

sim %>%
  ggplot() +
  aes(x = true_effort, y = true_catch) +
  geom_point() + 
  geom_smooth()

sum(sim$true_catch)*c(0.02, .04, 0.05, 0.08, .1, .16)
sum(sim$true_catch)/c((3700*54), (6500*54))


# catch from new residents
new_catch = sum(sim$true_catch)*(angler_days*.36*c(.1, .2, .5))/angler_days

old_catch = sum(sim$true_catch)*(1-(angler_days*.36*c(.1, .2, .5))/angler_days)

for(i in 1:length(new_catch)){print(new_catch[i]*c(.1, .13, .16))}

for(i in 1:length(old_catch)){print(old_catch[i]*c(.02, .05, .08))}
