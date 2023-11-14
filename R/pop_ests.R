########################################################################
# Calculates Chapman modification of Lincoln-Peterson estimator by 10mm length class
# Includes:
# * likelihood function population estimates (from pg 337 in AIFFD) by 10mm length class with q_hat
# from capture probability from smoothed spline estimate (Anderson 1995)
# * likelihood function from Otis et al (1978) model M0 (explained on AIFFD 335 eq 8.8 and in the box 
# startin on pg342)

library(dplyr)
library(ggplot2)
library(scales)
library(lemon)


d <- readRDS('./data/01_upper_madison.rds')

foo <- 
  d %>% 
  filter(Year < 2023)


########################################################################
########################################################################
# Rainbow trout, both locations, all years, by 10mm length class

rbt <- foo %>%
  filter(Length >= 160 & species == 'Rainbow') %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Year, location, Date, Trip.Type, M.C, length_class) %>%
  summarize(n = n())

rbt

rbt_est <- data.frame(year = rep(rbt$Year %>% unique(), 41) %>% sort(), 
                      location = rep(c("Pine Butte", "Varney"), 410), 
                      sp = 'Rainbow', 
                      length_class = rep(rbt$length_class %>% unique(), 20),
                      t = NA,
                      n_marked = NA,
                      n_caught = NA, 
                      n_recap = NA,
                      n_hat = NA, 
                      ci = NA)



for (y in rbt$Year %>% unique()){
  for (l in rbt$location %>% unique()){
    for (lc in rbt$length_class %>% unique()){
      
      t  <- # number caught in first sampling (i.e., marking) period
        rbt %>% filter(Trip.Type == 'R' & Year == y & location == l) %>%
        pull(Date) %>% 
        unique() %>%
        length()
      
    n1 <- # number caught in first sampling (i.e., marking) period
      rbt %>% filter(Trip.Type == 'M' & Year == y & location == l & length_class == lc) %>%
      pull(n) %>% 
      sum()
    
    n2 <- # number caught in second sampling (i.e., recapture) period
      rbt %>% filter(Trip.Type == 'R' & Year == y & location == l & length_class == lc) %>% 
      pull(n) %>% 
      sum()
    
    m2 <- # number of marked animals caught in second sampling period
      rbt %>% 
      filter(Trip.Type == 'R' & M.C == 1 & Year == y & location == l & length_class == lc) %>% 
      pull(n) %>% 
      sum()
    
    estimate <- ((n1 + 1)*(n2+1))/(m2+1) -1
    
    variance <- 
      ((
        (n1+1)*(n2+1)*(n1-m2)*(n2-m2)
      )/
        (
          ((m2+1)^2)*(m2+2)
        )) - 1
    
    ci <- 1.96*sqrt(variance)

    rbt_est$t[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- t
    rbt_est$n_marked[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- n1
    rbt_est$n_caught[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- n2
    rbt_est$n_recap[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- m2
    #Chapman modification of Lincoln-Peterson
    rbt_est$n_hat[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- estimate
    rbt_est$ci[which(rbt_est$year == y & rbt_est$location == l & rbt_est$length_class == lc)] <- ci
    
    }
  }
}
########################################################################
# Browns 

bnt <- foo %>%
  filter(Length >= 160 & species == 'Brown') %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Year, location, Date, Trip.Type, M.C, length_class) %>%
  summarize(n = n())

bnt


bnt_est <- data.frame(year = rep(bnt$Year %>% unique(), 54) %>% sort(), 
                      location = rep(c("Pine Butte", "Varney"), 540), 
                      sp = 'Brown', 
                      length_class = rep(bnt$length_class %>% unique(), 20), 
                      t = NA,
                      n_marked = NA,
                      n_caught = NA, 
                      n_recap = NA,
                      n_hat = NA, 
                      ci = NA
                      
)


for (y in bnt$Year %>% unique()){
  for (l in bnt$location %>% unique()){
    for (lc in bnt$length_class %>% unique()){
      
    t  <- # number caught in first sampling (i.e., marking) period
      bnt %>% filter(Trip.Type == 'R' & Year == y & location == l) %>%
      pull(Date) %>% 
      unique() %>%
      length()
    
    
    n1 <- # number caught in first sampling (i.e., marking) period
      bnt %>% filter(Trip.Type == 'M' & Year == y & location == l & length_class == lc) %>%
      pull(n) %>% 
      sum()
    
    n2 <- # number caught in second sampling (i.e., recapture) period
      bnt %>% filter(Trip.Type == 'R' & Year == y & location == l & length_class == lc) %>% 
      pull(n) %>% 
      sum()
    
    m2 <- # number of marked animals caught in second sampling period
      bnt %>% 
      filter(Trip.Type == 'R' & M.C == 1 & Year == y & location == l & length_class == lc) %>% 
      pull(n) %>% 
      sum()
    
    estimate <- ((n1 + 1)*(n2+1))/(m2+1) -1
    
    variance <- 
      ((
        (n1+1)*(n2+1)*(n1-m2)*(n2-m2)
      )/
        (
          ((m2+1)^2)*(m2+2)
        )) - 1
    
    ci <- 1.96*sqrt(variance)
    
    bnt_est$t[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- t
    bnt_est$n_marked[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- n1
    bnt_est$n_caught[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- n2
    bnt_est$n_recap[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- m2
    #Chapman modification of Lincoln-Peterson
    bnt_est$n_hat[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- estimate
    bnt_est$ci[which(bnt_est$year == y & bnt_est$location == l & bnt_est$length_class == lc)] <- ci
    
    }
  }
}

# plotted population estimates
# bnt_est %>%
#   group_by(year, location) %>%
#   summarize(yearly_est = sum(est, na.rm = T)) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = yearly_est), size = 3) +
#   geom_errorbar(aes(x = year, ymin = est-ci))
#   facet_grid(~location) +
#   geom_smooth(method = 'lm', formula = y ~ splines::bs(x), se = FALSE) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   labs(title = "Not stratified by 10mm length class") +
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



###############
# rbind rbt_est and bnt_est

pop_ests <- 
  rbt_est %>%
  rbind(bnt_est)

# plotted population estimates
# pop_ests %>%
#   filter(est > 0) %>% 
#   group_by(year, location, sp) %>%
#   summarize(est = sum(est, na.rm = T)) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = est), size = 3) +
#   # geom_errorbar(aes(x = year, ymin = est - ci, ymax = est + ci),
#   #               linetype = "1", colour = 'black', linewidth = 0.75)) +
#   geom_smooth(aes(x = year, y = est), se = FALSE, method = 'loess') +
#   # geom_smooth(aes(x = year, y = est), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
#   facet_rep_grid(location~sp) +
#   xlab('Year') +
#   ylab(expression("Population estimate ("~hat("N")~")")) +
#   # labs(title = expression(hat("N")~ "±95% CI from Chapman modification of Lincoln-Peterson")) +
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

###################################################
###################################################
###################################################
# Estimate probability of capture within length class with spline smoothing
# Anderson 1995

pop_ests <- 
  pop_ests %>%
  mutate(obs_prop_caught = n_recap/n_caught)

pop_ests %>%
  ggplot() +
  geom_point(aes(x = year, y = obs_prop_caught), size = 3) +
  # geom_smooth(aes(x = year, y = est), se = FALSE, method = 'loess') +
  geom_smooth(aes(x = year, y = obs_prop_caught), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
  facet_rep_grid(location~sp) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  # labs(title = expression(hat("N")~ "±95% CI from Chapman modification of Lincoln-Peterson")) +
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
  
  
mods <- pop_ests %>%
  arrange(year, sp, location, length_class) %>%
  # Following removed because there was no sampling at these times/locations/species
  filter((!(year == 2008 & location == 'Varney') &
           !(year == 2016 & location == 'Varney')), 
         !is.na(obs_prop_caught)) %>%
  group_by(year, sp, location) %>%
  do(model = lm(obs_prop_caught ~ splines::bs(length_class), data = .)) 


# Get unique year, species, location values for predictions
uniques <- 
  pop_ests %>%
  arrange(year, sp, location, length_class) %>% 
  filter((!(year == 2008 & location == 'Varney') &
            !(year == 2016 & location == 'Varney')), 
         !is.na(obs_prop_caught)) %>%
  select(year, sp, location) %>%
  unique()


# Predict probability of recapture (catchability, q_hat) by 10mm for each year, species, location
fits <- list()

for (i in 1:nrow(uniques)){

  fits[[i]] <-
    data.frame(
      predict(
        (mods %>% 
          filter(year == uniques$year[i] &
                          sp == uniques$sp[i] &
                          location == uniques$location[i]) %>%
          pull(model))[[1]],
        se = TRUE))

}

# reattach fitted probability of capture values to mark/recap dataset
filtered <-
  pop_ests %>%
  arrange(year, sp, location, length_class) %>% 
  filter((!(year == 2008 & location == 'Varney') &
            !(year == 2016 & location == 'Varney')), 
         !is.na(obs_prop_caught)) %>%
  bind_cols(bind_rows(fits))


# Function to return N_hat by maximum likelihood
get_mle_nhat <- function(n, prop){
  
  size <- seq(0, 100*(n+1), by = 1)
  
  d <- data.frame(size = size) %>%
    mutate(L = dbinom(n, size, prop))
  
  return(d %>% filter(L == max(L)) %>% pull(size) %>% max())
}

# adds MLE N_hat estimator to dataset
filtered <- 
  filtered %>%
  rowwise() %>%
  mutate(obs_prop_caught = ifelse(obs_prop_caught < 0, 0.001, obs_prop_caught), 
         fit = ifelse(fit < 0, 0.001, fit), 
         mle_obs_nhat = get_mle_nhat(n_recap, obs_prop_caught), 
         mle_fit_nhat = get_mle_nhat(n_recap, fit))


filtered %>% 
  select(year, location, sp, n_caught, n_recap, n_hat, obs_prop_caught, fit, mle_obs_nhat, mle_fit_nhat) %>%
  group_by(year, location, sp) %>%
  summarize(chapman_nhat = sum(n_hat), 
            mle_obs_nhat = sum(mle_obs_nhat), 
            mle_fit_nhat = sum(mle_fit_nhat)) %>%
  pivot_longer(c(chapman_nhat, mle_obs_nhat, mle_fit_nhat), names_to = 'method', values_to = 'n_hat') %>%
  ggplot() +
  geom_point(aes(x = year, y = n_hat, colour = method)) +
  geom_smooth(aes(x = year, y = n_hat, colour = method), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
  facet_rep_grid(location~sp) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  # labs(title = expression(hat("N")~ "±95% CI from Chapman modification of Lincoln-Peterson")) +
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



get_mle_nhat_m0 <- function(n, M, t){
  
  N <- seq(0, 170)
  
  L <- 
    log(factorial(N)/factorial((N-M)))+
    (n*log(n)) +
    ((t*N-n)*log(t*N-n)) -
    (t*N*log(t*N))
    
  d <- data.frame(N = N, 
                  L = L)
  
  
  return(d %>% filter(L == max(L, na.rm = T)) %>% pull(N) %>% max())
  
}

filtered <- 
  filtered %>%
  rowwise() %>%
  mutate(mle_nhat_m0 = get_mle_nhat_m0(n = n_recap, M = n_marked, t = t))




filtered %>% 
  select(year, location, sp, n_marked, n_caught, n_recap, n_hat, obs_prop_caught, fit, mle_obs_nhat, mle_fit_nhat, mle_nhat_m0) %>%
  group_by(year, location, sp) %>%
  summarize(chapman_nhat = sum(n_hat), 
            mle_obs_nhat = sum(mle_obs_nhat), 
            mle_fit_nhat = sum(mle_fit_nhat), 
            mle_nhat_m0 = sum(mle_nhat_m0)) %>%
  pivot_longer(c(chapman_nhat, mle_obs_nhat, mle_fit_nhat, mle_nhat_m0), names_to = 'method', values_to = 'n_hat') %>%
  ggplot() +
  geom_point(aes(x = year, y = n_hat, colour = method)) +
  geom_smooth(aes(x = year, y = n_hat, colour = method), method = 'lm', formula = y ~ splines::bs(x), se = FALSE)+#, se = TRUE) +
  facet_rep_grid(location~sp) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  # labs(title = expression(hat("N")~ "±95% CI from Chapman modification of Lincoln-Peterson")) +
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


