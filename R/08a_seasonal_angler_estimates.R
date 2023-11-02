library(AnglerCreelSurveySimulation)
library(ggplot2)
library(dplyr)

summer = as.numeric(as.Date('2023-08-31') - as.Date("2023-06-01"))
season_length = summer # days

# From madison river pressure survey -- upper river proportion of surveys from Skaar == 58%
# angler_days = 58716 * .58
# angler_days = 400000 * .58 # predicted in 2030
angling_est <-
  readRDS('./data/predicted_angler_pressure.rds') %>%
  filter(year >= 2023 & year <= 2030) %>% 
  select(year, fit, lwr, upr) %>% 
  pivot_longer(!year, names_to = 'type', values_to = 'n_anglers') %>%
  mutate(n_upper_anglers = n_anglers * .67)












# increase angler days by 36% SW mt population and 20% of those as anglers
# Don't recall where these numbers come from
# 1.4% is increase of BZN pop, c(.1, .2, .5) is potential increase of the BZN pop as anglers?
new_angler_days = angler_days+(angler_days*.014*c(.1, .2, .5))
new_angler_days[3]

# Single season long
# 11 hour day length (horton 2017, pg 24)
# discounting whitefish caught; catch rate for RBT and BNT only
sim <- conduct_multiple_surveys(
  n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
  start_time = 0, # start time of fishing day
  wait_time = 11, #end time of fishing day
  n_sites = 1, #number of sites sampled, all of upper Madison River
  n_anglers = (angler_days*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
  sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
  mean_catch_rate = 0.88, # mean catch rate of trout -- RBT = 0.68
  fishing_day_length = 11, # total length of fishing day
  mean_trip_length = 5.6) # average trip length

sim %>%
  ggplot() +
  aes(x = true_effort, y = true_catch) +
  geom_point() + 
  geom_smooth(method = 'lm')

# Estimate mortality, 8% assumed in Horton 2017, pg 23
# Sum expected catch of anglers from simulated season and estimate low to high total mortality based
# on several different rates
# range of mortality estimates
sum(sim$true_catch)*seq(.00, 0.24, by = 0.005)

mortality <- 
  data.frame(mort_rate = seq(.01, 0.24, by = 0.005)) %>%
  mutate(n_mort = mort_rate * (sum(sim$true_catch)*.5)) #multiplied by 50% because there's a pretty straight 50/50 split between rbt and bnt now


mortality %>%
  ggplot() +
  aes(x = mort_rate, y = n_mort) +
  geom_line() +
  geom_hline(yintercept = 30000, linetype = 2)

# #recyling rates
# # total trout catch divided by 
(sum(sim$true_catch)/2)/c((2500*54), (2400*54)) #population estimates of rbt and bnt from horton 2017?


# portion of catch from new residents
new_catch = sum(sim$true_catch)*(angler_days*.014*c(.1, .2, .5))/angler_days

# portion of catch from "old" residents
old_catch = sum(sim$true_catch)*(1-(angler_days*.014*c(.1, .2, .5))/angler_days)

new_catch_mortality <- 
  data.frame(
    catch_by_new = new_catch
  )

new_catch_mortality <-
  new_catch_mortality %>%
  mutate(mort_10 = catch_by_new * .1, 
         mort_13 = catch_by_new * .13, 
         mort_16 = catch_by_new * .16)

new_catch_mortality


old_catch_mortality <- 
  data.frame(
    catch_by_old = old_catch
  )

old_catch_mortality <-
  old_catch_mortality %>%
  mutate(mort_02 = catch_by_old * .02, 
         mort_05 = catch_by_old * .05, 
         mort_08 = catch_by_old * .08)

old_catch_mortality
