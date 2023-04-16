

library(AnglerCreelSurveySimulation)
library(ggplot2)
library(dplyr)


season_length = 90 # days
angler_days = 20000

angler_days/season_length

#multiple simulations = 100 seasons?
sim <- conduct_multiple_surveys(n_sims = 90*10, start_time = 0, wait_time = 12, n_sites = 1,
                                n_anglers = angler_days/season_length, sampling_prob = 1, 
                                mean_catch_rate = 2.5, fishing_day_length = 12)

sim %>% 
  ggplot() + 
  aes(y = true_catch, x = true_effort) + 
  geom_point() + 
  geom_smooth()

nrow(sim)


foo <- 
  data.frame(
  vals = seq(0, 50, 0.5)
  ) %>%
  mutate(pois_probs = dpois(x = 10, lambda = seq(0, 50, 0.5))
  )

foo %>%
  ggplot() + 
  aes(x = vals, y = pois_probs) +
  geom_line()

sum(foo$pois_probs)

      