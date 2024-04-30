###
# 02_population_estimates
# Makes to different population estimates, Chapman modificaation of Lincoln Peterson and
# a maximum likelihood estimator


library(dplyr)
library(ggplot2)
library(scales)
library(lemon)

source('./R/00_helper_functions.R')

d <- 
  readRDS('./data/01_upper_madison.rds') %>%
  filter(species %in% c("Brown", "Rainbow"))

section_length <-
  d %>% 
  select(location, Section.Length) %>% 
  unique() %>%
  mutate(length = as.numeric(gsub("m", "", Section.Length)), 
         length_km = length/1000, 
         length_mi = length/1609)

section_length

foo <- 
  d %>% 
  filter(Year < 2023)

########################################################################
# Calculates Chapman modification of Lincoln-Peterson estimator by 10mm length class
# and length category (Gablehouse 198?)

# Calculates Chapman estimator Nhat
calc_chapman_nhat <- function(n1, n2, m2){
  
  n <- (((n1 + 1)*(n2+1))/(m2+1)) -1
  return(n)
}

# calculates Chapman estimator variance
calc_chapman_var <- function(n1, n2, m2){
  var <- 
    ((
      (n1+1)*(n2+1)*(n1-m2)*(n2-m2)
    )/
      (
        ((m2+1)^2)*(m2+2)
      )) - 1
  
  return(var)
}

# Bootstraps Chapman estimator confidence intervals
calc_chapman_bootstrap_ci <- function(n1, n2, m2, boots = 10000, conf = 0.95){
  cis <- unname(
    quantile(calc_chapman_nhat(n1, n2, rbinom(boots, n2, m2/n2)), 
             probs = c((1-conf)/2, 1-(1-conf)/2), na.rm = TRUE
    ))
  return(cis)
}

###
# Function to estimate MLE values
get_mle_vals <- function(n1, n2, q){

  poss_n <- seq(max(c(n1, n2)), max(c(n1, n2))*1000, by = 1)

  lh <- dbinom(n2, poss_n, q)

  df <- data.frame(n = poss_n, L = lh)

  n_hat <- 
    df %>%
    filter(L == max(L)) %>%
    pull(n) %>%
    max()
  
  ci <-
    df %>% 
    mutate(log_l = log(L)) %>%
    filter(log_l > max(log_l)-3.841) %>%
    slice(c(1, n())) %>% 
    pull(n)
  
  return(list(n_hat = n_hat, lower_ci = min(ci), upper_ci = max(ci)))
}


########################################################################
# Some data handling
counts <- 
  foo %>%
  filter(species %in% c("Brown", "Rainbow"), 
         length_class >= 150) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(species, Year, location, Date, Trip.Type, M.C, length_class) %>%
  summarize(n = n())

pop_ests_length_class <- 
  counts %>%
  group_by(species, Year, location, length_class) %>%
  filter(Trip.Type == 'M') %>%
  summarize(n1 = sum(n, na.rm = T)) %>%
  full_join(
    counts %>% 
      group_by(species, Year, location, length_class) %>%
      filter(Trip.Type == 'R') %>%
      summarize(n2 = sum(n, na.rm = T))
  ) %>% 
  full_join(
    counts %>% 
      group_by(species, Year, location, length_class) %>%
      filter(Trip.Type == 'R' & M.C == 1) %>%
      summarize(m2 = sum(n, na.rm = T))
  ) %>%
  full_join(
    counts %>% 
      group_by(species, Year, location) %>%
      filter(Trip.Type == 'R') %>%
      summarize(t = n_distinct(Date))
  ) %>%
  rename(sp = species, 
         year = Year) %>%
  ungroup() %>%
  mutate(n1 = ifelse(is.na(n1), 0, n1),
         n2 = ifelse(is.na(n2), 0, n2),
         m2 = ifelse(is.na(m2), 0, m2)) %>%
  rowwise() %>%
  # Make some estimates--ended up not using these
  mutate(n_hat = calc_chapman_nhat(n1, n2, m2),
         ci = 1.96*sqrt(calc_chapman_var(n1, n2, m2)), 
         boot_ci_low = calc_chapman_bootstrap_ci(n1, n2, m2)[[1]], 
         boot_ci_upr = calc_chapman_bootstrap_ci(n1, n2, m2)[[2]], 
         psd = ifelse(sp == 'Brown', assign_bnt_psd(length_class), 
                      ifelse(sp == 'Rainbow', assign_rbt_psd(length_class), NA)), 
         psd = factor(psd, levels = c('SS', 'S-Q', 'Q-P', 'P-M', 'M-T', '>T')), 
         obs_prop_caught = m2/n2
  )

########################
# modeling catchability from Anderson 1995
# Spline modeling of catchability by 10mm length class by year by population by species

q_mods <- 
  pop_ests_length_class %>%
  arrange(year, sp, location, length_class) %>%
  # Following removed because there was no sampling at these times/locations/species
  filter(!is.na(obs_prop_caught)) %>%
  group_by(year, sp, location) %>%
  do(model = lm(obs_prop_caught ~ splines::bs(length_class), data = .)) 

# Get unique year, species, location values for predictions
uniques <- 
  pop_ests_length_class %>%
  arrange(year, sp, location, length_class) %>% 
  filter(!is.na(obs_prop_caught)) %>%
  select(year, sp, location) %>%
  unique()


# Predict probability of recapture (catchability, q_hat) by 10mm for each year, species, location
fits <- list()

for (i in 1:nrow(uniques)){
  
  fits[[i]] <-
    data.frame(
      predict(
        (q_mods %>% 
           filter(year == uniques$year[i] &
                    sp == uniques$sp[i] &
                    location == uniques$location[i]) %>%
           pull(model))[[1]],
        se = TRUE))
  
}

# reattach fitted probability of capture values to mark/recap dataset
pop_ests_length_class <-
  pop_ests_length_class %>%
  arrange(year, sp, location, length_class) %>% 
  filter(!is.na(obs_prop_caught)) %>%
  bind_cols(bind_rows(fits))

# Calculate MLE n_hat and 95% confidence intervals
pop_ests_length_class <- 
  pop_ests_length_class %>%
  # below is calculating MLE values  
  mutate(mle_nhat = get_mle_vals(n1, n2, fit)$n_hat,
         mle_lwrci = get_mle_vals(n1, n2, fit)$lower_ci,
         mle_uprci = get_mle_vals(n1, n2, fit)$upper_ci
  )

#######################
# LMs of nhat as a function of year

# Shows which length classes end up getting removed from consideration
# Mostly the extreme smalls and largers for both species
# pop_ests_length_class %>% 
#   filter(fit <= 0.01) %>% 
#   select(year, sp, length_class) %>% 
#   group_by(year, sp, length_class) %>% 
#   summarize(n()) %>% 
#   arrange(sp, year, length_class) %>% 
#   as.data.frame()

est_per_km <- 
  pop_ests_length_class %>%
  filter(fit >= 0.01) %>% 
  group_by(year, location, sp) %>%
  mutate(mle_nhat = ifelse(is.infinite(mle_nhat), NA, mle_nhat), 
         mle_lwrci = ifelse(is.infinite(mle_lwrci), NA, mle_lwrci),
         mle_uprci = ifelse(is.infinite(mle_uprci), NA, mle_uprci)
  ) %>%
  summarize(n_hat = sum(mle_nhat, na.rm = T), 
            lower = sum(mle_lwrci, na.rm = T), 
            upper = sum(mle_uprci, na.rm = T)) %>%
  full_join(
    section_length %>%
      select(location, length_km, length_mi)
  ) %>%
  mutate(n_hat_km = n_hat/length_km, 
         lower_km = lower/length_km, 
         upper_km = upper/length_km, 
         n_hat_mi = n_hat/length_mi, 
         lower_mi = lower/length_mi, 
         upper_mi = upper/length_mi, 
  )



length_class_mods <- 
  est_per_km %>%
  ungroup() %>%
  group_by(location, sp) %>%
  do(mod = lm(n_hat_km ~ year, data = .))

length_class_mods 

saveRDS(length_class_mods, "output/length_class_mods.rds")

for (i in 1:nrow(length_class_mods)){
  print(c(length_class_mods$sp[[i]], length_class_mods$location[[i]]))

  print(summary(length_class_mods$mod[[i]]))
}

# for (i in 1:nrow(length_class_mods)){
#   print(c(length_class_mods$sp[[i]], length_class_mods$location[[i]]))
#   
#   print(length_class_mods$mod[[i]])
# }

for (i in 1:nrow(length_class_mods)){
  print(c(length_class_mods$sp[[i]], length_class_mods$location[[i]]))
  
  print(length_class_mods$mod[[i]]$coefficients)
}




###################
# plot of population estimates/km

per_km <- 
  est_per_km %>%
  ggplot() +
  geom_point(aes(x = year, y = n_hat_km), size = 3) +
  geom_errorbar(aes(x = year, ymin = lower_km, ymax = upper_km),
                linetype = 2, linewidth = 0.5) +#,
  geom_smooth(aes(x = year, y = n_hat_km), linetype = 1, color = 'black', se = FALSE, level = 0.95, method = 'lm', formula = y~x) +
  facet_rep_grid(location~sp) +
  # lims(y = c(0, 30000)) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")/km")) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )

per_km

ggsave(paste0("output/images/02_population_estimates_per_km.png"), plot = per_km, 
       width = 16, height = 9, bg = "white")

###
# Length category

# plotted population estimates by year, sp, location, AND length category
# q <-
#   pop_ests_length_class %>%
#   filter(fit >= 0.01) %>%
#   group_by(year, location, sp, psd) %>%
#   mutate(mle_nhat = ifelse(is.infinite(mle_nhat), NA, mle_nhat),
#          mle_lwrci = ifelse(is.infinite(mle_lwrci), NA, mle_lwrci),
#          mle_uprci = ifelse(is.infinite(mle_uprci), NA, mle_uprci)
#   ) %>%
#   summarize(n_hat = sum(mle_nhat, na.rm = T),
#             lower = sum(mle_lwrci, na.rm = T),
#             upper = sum(mle_uprci, na.rm = T)) %>%
#   ggplot() +
#   aes(colour = psd) +
#   geom_point(aes(x = year, y = n_hat), size = 3) +
#   # geom_errorbar(aes(x = year, ymin = lower, ymax = upper),
#   #               linetype = 2, linewidth = 0.5) +#,
#   geom_smooth(aes(x = year, y = n_hat, linetype = psd), se = FALSE, method = 'lm', formula = y~x) +
#   # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
#   facet_rep_grid(location~sp) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   # labs(title = expression(hat("N")~ "by 10mm length class")) +
#   # labs(shape = "", linetype = '', point = "") +
#   # scale_y_continuous(labels = label_comma()) +
#   theme_minimal(base_size = 30) +
#   theme(legend.position = 'bottom',
#         legend.title=element_blank(),
#         panel.grid.minor = element_blank(),
#         # panel.border = element_rect(colour = "black", fill=NA, size=1)
#         # panel.border = element_blank(),
#         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
#         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
#   )
# 
# q
# 
# ggsave(paste0("output/images/02_population_estimates_length_cagtegory.png"), plot = q, 
#        width = 16, height = 9, bg = "white")

##########################
# time difference between mark/recapture runs

d %>% 
  group_by(Year, location, Trip.Type) %>% 
  summarize(n_dates = n_distinct(Date)) %>% 
  ggplot() + 
  aes(x = Year, y = n_dates) + 
  geom_point() + 
  geom_line() + 
  facet_rep_grid(Trip.Type~location)

d %>% 
  group_by(Year, location, Trip.Type) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  summarize(n_dates = n_distinct(Date), 
            start = min(Date), 
            end = max(Date)
  ) %>%
  mutate(delta_days = as.numeric(start - lag(end))) %>%
  ungroup() %>%
  summarize(mean_delta = mean(delta_days, na.rm = T), 
            sd_delta = sd(delta_days, na.rm = T))

d %>% 
  group_by(Year, location, Trip.Type) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  summarize(n_dates = n_distinct(Date), 
            start = min(Date), 
            end = max(Date)
  ) %>%
  mutate(delta_days = as.numeric(start - lag(end))) %>%
  ggplot() +
  aes(x = delta_days) +
  geom_histogram(binwidth = 1)



