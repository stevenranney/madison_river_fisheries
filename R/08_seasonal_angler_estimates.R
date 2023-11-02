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
  mutate(n_upper_anglers = n_anglers * .67, 
         bnt_catch = NA, 
         rbt_catch = NA)


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
    mean_trip_length = 5.6) # average trip length
  
  angling_est$bnt_catch[i] <- sum(sim$true_catch)
}

# FOR RAINBOW TROUT
# Catch rate = 0.62
for (i in 1:nrow(angling_est)){
  
  sim <- conduct_multiple_surveys(
    n_sims = season_length, # number of days to estimate catch and effort, 91 days in summer
    start_time = 0, # start time of fishing day
    wait_time = 11, #end time of fishing day
    n_sites = 1, #number of sites sampled, all of upper Madison River
    n_anglers = (angling_est$n_upper_anglers[i]*summer_prop)/summer, # n anglers total upper summer anglers/n days in summer = angler_days per day
    sampling_prob = 1, # likelihood of angler being interviewed/sampled by surveyor, measuring every angler
    mean_catch_rate = 0.62, # catch rate of trout -- 0.38
    fishing_day_length = 11, # total length of fishing day
    mean_trip_length = 5.6) # average trip length
  
  angling_est$rbt_catch[i] <- sum(sim$true_catch)
}

p <- 
  angling_est %>%
  mutate(Brown = bnt_catch * 0.02, 
         Rainbow = rbt_catch * 0.08) %>%
  select(year, type, Brown, Rainbow) %>%
  pivot_longer(c(Brown, Rainbow), names_to = 'species', values_to = 'mortality') %>%
  ggplot() +
  geom_line(
    data = . %>% filter(type == 'fit'), 
    aes(x = year, y = mortality, linetype = type), 
    size = 1) +
  geom_ribbon(
    data = . %>% pivot_wider(names_from = type, values_from = mortality), 
    aes(x = year, ymin = lwr, ymax = upr, fill = 'band'), 
    # fill = 'grey', 
    alpha = 0.5) + 
  facet_grid(~species, scales = "free") +
  xlab("Year") +
  ylab('Predicted annual mortality') +
  scale_linetype_manual(name = "",
                        values = 'solid', 
                        labels = "Fit"
  ) + 
  scale_fill_manual("", values = "grey", labels = 'Prediction interval') +
  labs(linetype = '', fill = '') +
  # scale_y_continuous(labels = label_comma()) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

p

ggsave(paste0("output/images/", Sys.Date(), "_predicted_annual_mortality.png"), plot = p, 
       width = 16, height = 9, bg = "white")














