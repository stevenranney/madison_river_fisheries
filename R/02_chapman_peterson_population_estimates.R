########################################################################
# Calculates Chapman modification of Lincoln-Peterson estimator by 10mm length class
# and length category (Gablehouse 198?)

library(dplyr)
library(ggplot2)
library(scales)
library(lemon)

source('./R/00_helper_functions.R')

d <- 
  readRDS('./data/01_upper_madison.rds') %>%
  filter(species %in% c("Brown", "Rainbow"))

foo <- 
  d %>% 
  filter(Year < 2023)


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


########################################################################
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
  # group_by(sp, year, location, length_class) %>%
  rowwise() %>%
  mutate(n_hat = calc_chapman_nhat(n1, n2, m2), 
         ci = 1.96*sqrt(calc_chapman_var(n1, n2, m2)), 
         boot_ci_low = calc_chapman_bootstrap_ci(n1, n2, m2)[[1]], 
         boot_ci_upr = calc_chapman_bootstrap_ci(n1, n2, m2)[[2]], 
         psd = ifelse(sp == 'Brown', assign_bnt_psd(length_class), 
                      ifelse(sp == 'Rainbow', assign_rbt_psd(length_class), NA)), 
         psd = factor(psd, levels = c('SS', 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))
  )

length_class_mods <- 
  pop_ests_length_class %>%
  group_by(year, location, sp) %>%
  summarize(n_hat = sum(n_hat, na.rm = T), 
            lower = sum(boot_ci_low, na.rm = T), 
            upper = sum(boot_ci_upr, na.rm = T)) %>%
  ungroup() %>%
  group_by(location, sp) %>%
  do(mod = lm(year ~ n_hat, data = .))

length_class_mods 

for (i in 1:nrow(length_class_mods)){
  print(c(length_class_mods$sp[[i]], length_class_mods$location[[i]]))

  print(summary(length_class_mods$mod[[i]]))
}


###################################################
# Function to return N_hat by maximum likelihood
# uses the Nhat from Chapman and the observed catchability 
# to find MLE of Nhat

get_mle_catchability <- function(n, N){
  q <- seq(0, 1, 0.01)
  
  d <- data.frame(q) %>%
    mutate(L = dbinom(n, N, q))
  
  return(d %>% filter(L == max(L)) %>% pull(q))
  
}

get_mle_nhat <- function(n, prop){
  
  size <- seq(n, 100*(n+1), by = 1)
  
  d <- data.frame(size = size) %>%
    mutate(L = dbinom(n, size, prop))
  
  return(d %>% filter(L == max(L)) %>% pull(size) %>% max())
}

pop_ests_length_class <- 
  pop_ests_length_class %>%
  rowwise() %>%
  mutate(mle_nhat = get_mle_nhat(m2, m2/n2))

pop_ests_length_class %>% 
  filter(mle_nhat > 0) %>%
  group_by(year, location, sp) %>%
  summarize(mle_nhat = sum(mle_nhat, na.rm = T), 
            lower = sum(boot_ci_low, na.rm = T), 
            upper = sum(boot_ci_upr, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = year, y = mle_nhat), size = 3) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper),
                linetype = 2, linewidth = 0.5) +#,
  # linetype = "1", colour = 'black', linewidth = 0.75)) +
  # geom_smooth(aes(x = year, y = n_hat), se = FALSE, method = 'loess') +
  geom_smooth(aes(x = year, y = mle_nhat), linetype = 1, color = 'black', se = FALSE, method = 'lm', formula = y~x) +
  # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
  facet_rep_grid(location~sp) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  # labs(title = expression(hat("N")~ "by 10mm length class")) +
  # labs(shape = "", linetype = '', point = "") +
  # scale_y_continuous(labels = label_comma()) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )












# plotted population estimates
p <- 
  pop_ests_length_class %>%
  # filter(n_hat > 0) %>%
  group_by(year, location, sp) %>%
  summarize(n_hat = sum(n_hat, na.rm = T), 
            lower = sum(boot_ci_low, na.rm = T), 
            upper = sum(boot_ci_upr, na.rm = T)) %>%
  ggplot() +
  geom_point(aes(x = year, y = n_hat), size = 3) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper),
                linetype = 2, linewidth = 0.5) +#,
  geom_smooth(aes(x = year, y = n_hat), linetype = 1, color = 'black', se = FALSE, method = 'lm', formula = y~x) +
  facet_rep_grid(location~sp) +
  lims(y = c(0, 30000)) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )

p

ggsave(paste0("output/images/02_population_estimates.png"), plot = p, 
       width = 16, height = 9, bg = "white")

#### 
# All river poulation estimates?

# pop_ests_length_class %>%
#   group_by(year, sp) %>%
#   summarize(n_hat = sum(n_hat, na.rm = T)/2, 
#             lower = sum(boot_ci_low, na.rm = T)/2, 
#             upper = sum(boot_ci_upr, na.rm = T)/2, 
#             total_river = n_hat * 54, 
#             lower_ci = lower*54, 
#             upper_ci = upper*54) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = total_river), size = 3) +
#   geom_errorbar(aes(x = year, ymin = lower_ci, ymax = upper_ci),
#                 linetype = 2, linewidth = 0.5) +#,
#   geom_smooth(aes(x = year, y = total_river), linetype = 1, color = 'black', se = TRUE, method = 'lm', formula = y~x) +
#   facet_rep_grid(~sp) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   theme_minimal(base_size = 30) +
#   theme(legend.position = 'bottom',
#         legend.title=element_blank(),
#         panel.grid.minor = element_blank(),
#         # panel.border = element_rect(colour = "black", fill=NA, size=1)
#         # panel.border = element_blank(),
#         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
#         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
#   )



# Length category

# plotted population estimates
q <- 
  pop_ests_length_class %>%
  # filter(sp == 'Brown') %>%
  filter(n_hat > 0) %>%
  group_by(year, location, sp, psd) %>%
  summarize(n_hat = sum(n_hat, na.rm = T), 
            lower = sum(boot_ci_low, na.rm = T), 
            upper = sum(boot_ci_upr, na.rm = T)) %>%
  ggplot() +
  aes(colour = psd) +
  geom_point(aes(x = year, y = n_hat), size = 3) +
  # geom_errorbar(aes(x = year, ymin = lower, ymax = upper), 
  #               linetype = 2, linewidth = 0.5) +#,
  geom_smooth(aes(x = year, y = n_hat, linetype = psd), se = FALSE, method = 'lm', formula = y~x) +
  # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
  facet_rep_grid(location~sp) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  # labs(title = expression(hat("N")~ "by 10mm length class")) +
  # labs(shape = "", linetype = '', point = "") +
  # scale_y_continuous(labels = label_comma()) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )

q

ggsave(paste0("output/images/02_population_estimates_length_cagtegory.png"), plot = q, 
       width = 16, height = 9, bg = "white")

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




# ########################################################################
# ########################################################################
# # NOW WITH A FOR LOOP
# # Rainbow trout, both locations, all years, by 10mm length class
# 
# rbt <- foo %>%
#   filter(
#     # Length >= 160 &
#       species == 'Rainbow') %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   group_by(Year, location, Date, Trip.Type, M.C, length_class) %>%
#   summarize(n = n())
# 
# rbt
# 
# times <- rbt$length_class %>% unique() %>% length()
# 
# rbt_est <- data.frame(year = rep(bnt$Year %>% unique(), times * 2) %>% sort(),
#                       location = c(rep("Pine Butte", times), rep("Varney", times)),
#                       sp = 'Rainbow',
#                       length_class = rep(rbt$length_class %>% unique(), 20),
#                       t = NA,
#                       n_marked = NA,
#                       n_caught = NA,
#                       n_recap = NA,
#                       n_hat = NA,
#                       ci = NA)
# 
# for (y in rbt$Year %>% unique()){
#   for (l in rbt$location %>% unique()){
#     for (lc in rbt$length_class %>% unique()){
# 
#       t  <- # number caught in first sampling (i.e., marking) period
#         rbt %>% filter(Trip.Type == 'R' & Year == y & location == l) %>%
#         pull(Date) %>%
#         unique() %>%
#         length()
# 
#       n1 <- # number caught in first sampling (i.e., marking) period
#         rbt %>% filter(Trip.Type == 'M' & Year == y & location == l & length_class == lc) %>%
#         pull(n) %>%
#         sum()
# 
#       n2 <- # number caught in second sampling (i.e., recapture) period
#         rbt %>% filter(Trip.Type == 'R' & Year == y & location == l & length_class == lc) %>%
#         pull(n) %>%
#         sum()
# 
#       m2 <- # number of marked animals caught in second sampling period
#         rbt %>%
#         filter(Trip.Type == 'R' & M.C == 1 & Year == y & location == l & length_class == lc) %>%
#         pull(n) %>%
#         sum()
# 
#       estimate <- calc_chapman_nhat(n1, n2, m2)
# 
#       variance <- calc_chapman_var(n1, n2, m2)
# 
#       ci <- 1.96*sqrt(variance)
# 
#       boot_ci <- calc_chapman_bootstrap_ci(n1, n2, m2)
# 
#       rbt_est$t[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- t
#       rbt_est$n_marked[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- n1
#       rbt_est$n_caught[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- n2
#       rbt_est$n_recap[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- m2
#       #Chapman modification of Lincoln-Peterson
#       rbt_est$n_hat[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- estimate
#       rbt_est$ci[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- ci
#       rbt_est$boot_ci_low[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- boot_ci[[1]]
#       rbt_est$boot_ci_upr[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- boot_ci[[2]]
# 
#     }
#   }
# }
# # ########################################################################
# # # Browns 
# # 
# bnt <- foo %>%
#   filter(
#     # Length >= 160 &
#       species == 'Brown') %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   group_by(Year, location, Date, Trip.Type, M.C, length_class) %>%
#   summarize(n = n())
# 
# bnt
# 
# times <- bnt$length_class %>% unique() %>% length()
# 
# bnt_est <- data.frame(year = rep(bnt$Year %>% unique(), times * 2) %>% sort(),
#                       location = c(rep("Pine Butte", times), rep("Varney", times)),
#                       sp = 'Brown',
#                       length_class = rep(bnt$length_class %>% unique(), 20),
#                       t = NA,
#                       n_marked = NA,
#                       n_caught = NA,
#                       n_recap = NA,
#                       n_hat = NA,
#                       ci = NA
# 
# )
# 
# 
# for (y in bnt$Year %>% unique()){
#   for (l in bnt$location %>% unique()){
#     for (lc in bnt$length_class %>% unique()){
# 
#       t  <- # number caught in first sampling (i.e., marking) period
#         bnt %>% filter(Trip.Type == 'R' & Year == y & location == l) %>%
#         pull(Date) %>%
#         unique() %>%
#         length()
# 
# 
#       n1 <- # number caught in first sampling (i.e., marking) period
#         bnt %>% filter(Trip.Type == 'M' & Year == y & location == l & length_class == lc) %>%
#         pull(n) %>%
#         sum()
# 
#       n2 <- # number caught in second sampling (i.e., recapture) period
#         bnt %>% filter(Trip.Type == 'R' & Year == y & location == l & length_class == lc) %>%
#         pull(n) %>%
#         sum()
# 
#       m2 <- # number of marked animals caught in second sampling period
#         bnt %>%
#         filter(Trip.Type == 'R' & M.C == 1 & Year == y & location == l & length_class == lc) %>%
#         pull(n) %>%
#         sum()
# 
#       estimate <- calc_chapman_nhat(n1, n2, m2)
# 
#       variance <- calc_chapman_var(n1, n2, m2)
# 
#       ci <- 1.96*sqrt(variance)
# 
#       boot_ci <- calc_chapman_bootstrap_ci(n1, n2, m2)
# 
#       bnt_est$t[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- t
#       bnt_est$n_marked[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- n1
#       bnt_est$n_caught[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- n2
#       bnt_est$n_recap[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- m2
#       #Chapman modification of Lincoln-Peterson
#       bnt_est$n_hat[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- estimate
#       bnt_est$ci[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- ci
#       bnt_est$boot_ci_low[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- boot_ci[[1]]
#       bnt_est$boot_ci_upr[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- boot_ci[[2]]
# 
#     }
#   }
# }
# # 
# #plotted population estimates
# from_for_loop <- 
#   bnt_est %>%
#   bind_rows(rbt_est) 
# 
# from_for_loop %>%
#   group_by(year, location, sp) %>%
#   summarize(
#     yearly_est = sum(n_hat, na.rm = T)) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = yearly_est), size = 3) +
#   # geom_errorbar(aes(x = year, ymin = est-ci, ymax = n_hat))
#   facet_grid(location~sp) +
#   geom_smooth(aes(x = year, y = yearly_est), method = 'lm', formula = y ~ x, se = FALSE) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   lims(y = c(0, 30000)) +
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
#   
#   pop_ests_length_class %>%
#     # filter(sp == 'Brown') %>%
#     filter(n_hat > 0) %>%
#     group_by(year, location, sp) %>%
#     summarize(n_hat = sum(n_hat, na.rm = T), 
#               lower = sum(boot_ci_low, na.rm = T), 
#               upper = sum(boot_ci_upr, na.rm = T)) %>%
#     ggplot() +
#     aes(colour = psd) +
#     geom_point(aes(x = year, y = n_hat), size = 3) +
#     # geom_errorbar(aes(x = year, ymin = lower, ymax = upper), 
#     #               linetype = 2, linewidth = 0.5) +#,
#     geom_smooth(aes(x = year, y = n_hat, linetype = psd), se = FALSE, method = 'lm', formula = y~x) +
#     # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
#     facet_rep_grid(location~sp) +
#     xlab('Year') +
#     ylab(expression("Population estimate ("~hat("N")~")")) +
#     # labs(title = expression(hat("N")~ "by 10mm length class")) +
#     # labs(shape = "", linetype = '', point = "") +
#     # scale_y_continuous(labels = label_comma()) +
#     theme_minimal(base_size = 30) +
#     theme(legend.position = 'bottom',
#           legend.title=element_blank(),
#           panel.grid.minor = element_blank(),
#           # panel.border = element_rect(colour = "black", fill=NA, size=1)
#           # panel.border = element_blank(),
#           axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
#           axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
#     )
# 
# 
# 
# ###############
# # rbind rbt_est and bnt_est
# 
# pop_est_length_cat <- 
#   rbt_est %>%
#   rbind(bnt_est)
# 
# length_cat_mods <- 
#   pop_est_length_cat %>%
#   group_by(year, location, sp) %>%
#   summarize(n_hat = sum(n_hat, na.rm = T), 
#             lower = sum(boot_ci_low, na.rm = T), 
#             upper = sum(boot_ci_upr, na.rm = T)) %>%
#   ungroup() %>%
#   group_by(location, sp) %>%
#   do(mod = lm(year ~ n_hat, data = .))
# 
# for (i in 1:nrow(length_cat_mods)){
#   print(summary(length_cat_mods$mod[[i]]))
# }
# 
# 
# # plotted population estimates
# pop_est_length_cat %>%
#   filter(n_hat > 0) %>%
#   group_by(year, location, sp) %>%
#   summarize(n_hat = sum(n_hat, na.rm = T)) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = n_hat), size = 3) +
#   # geom_errorbar(aes(x = year, ymin = est - ci, ymax = est + ci),
#   #               linetype = "1", colour = 'black', linewidth = 0.75)) +
#   geom_smooth(aes(x = year, y = n_hat), se = FALSE, method = 'loess') +
#   # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
#   facet_rep_grid(location~sp) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   labs(title = expression(hat("N")~ "by length category (Gablehouse 1984)")) +
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
# 
# 
# # plotted population estimates
# pop_est_length_cat %>%
#   filter(n_hat > 0) %>%
#   group_by(year, location, sp, psd) %>%
#   summarize(n_hat = sum(n_hat, na.rm = T)) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = n_hat, shape = psd), size = 3) +
#   # geom_errorbar(aes(x = year, ymin = est - ci, ymax = est + ci),
#   #               linetype = "1", colour = 'black', linewidth = 0.75)) +
#   geom_smooth(aes(x = year, y = n_hat, linetype = psd), se = FALSE, method = 'loess') +
#   # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
#   facet_rep_grid(location~sp) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   labs(title = expression(hat("N")~ "by length category (Gablehouse 1084)")) +
#   # labs(shape = "", linetype = '', point = "") +
#   # scale_y_continuous(labels = label_comma()) +
#   theme_minimal(base_size = 30) +
#   theme(legend.position = 'bottom',
#         legend.title=element_blank(),
#         # legend.title = element_text(size = 15), 
#         legend.text = element_text(size = 15),
#         panel.grid.minor = element_blank(),
#         # panel.border = element_rect(colour = "black", fill=NA, size=1)
#         # panel.border = element_blank(),
#         axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
#         axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
#   ) +
#   guides(shape = guide_legend(override.aes = list(size = 5)), 
#          color = guide_legend(override.aes = list(size = 5)), 
#          linetype = guide_legend(override.aes = list(size = 5))
#   )



