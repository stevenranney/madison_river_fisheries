
library(dplyr)
library(ggplot2)
library(scales)


d <- readRDS('./data/01_upper_madison.rds')

# M.C as mark/recapture?

foo <- 
  d %>% 
  filter(Year < 2023)

foo %>% 
  mutate(Year = as.factor(Year)) %>%
  group_by(Year, location, Date) %>% 
  summarize(
    catch = n(), 
    f_hours = unique(Shock.Time..min.)/60, 
    c_f = catch/f_hours) %>% 
  arrange(Year, location) %>% 
  ggplot() +
  aes(x = Year, y = c_f, fill = location) +
  geom_boxplot()


########################################################################
# Rainbows 

rbt <- foo %>%
  filter(Length >= 155 & species == 'Rainbow') %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Year, location, Date, Trip.Type, M.C) %>%
  summarize(n = n())

rbt


rbt_est <- data.frame(year = rep(dat$Year %>% unique(), 2) %>% sort(), 
                      location = rep(c("Pine Butte", "Varney"), 10), 
                      sp = 'Rainbow', 
                      est = rep(NA,  dat$Year %>% n_distinct(), 2), 
                      ci = rep(NA,  dat$Year %>% n_distinct(), 2)
                      
)


for (y in rbt$Year %>% unique()){
  for (l in rbt$location %>% unique()){
    
    n1 <- # number caught in first sampling (i.e., marking) period
      rbt %>% filter(Trip.Type == 'M' & Year == y & location == l) %>%
      pull(n) %>% 
      sum()
    
    n2 <- # number caught in second sampling (i.e., recapture) period
      rbt %>% filter(Trip.Type == 'R' & Year == y & location == l) %>% 
      pull(n) %>% 
      sum()
    
    m2 <- # number of marked animals caught in second sampling period
      rbt %>% 
      filter(Trip.Type == 'R' & M.C == 1 & Year == y & location == l) %>% 
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

    rbt_est$est[which(rbt_est$year == y & rbt_est$location == l)] <- estimate
    rbt_est$ci[which(rbt_est$year == y & rbt_est$location == l)] <- ci
    
  }
}

########################################################################
# Browns 

bnt <- foo %>%
  filter(Length >= 155 & species == 'Brown') %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Year, location, Date, Trip.Type, M.C) %>%
  summarize(n = n())

bnt


bnt_est <- data.frame(year = rep(dat$Year %>% unique(), 2) %>% sort(), 
                      location = rep(c("Pine Butte", "Varney"), 10), 
                      sp = 'Brown', 
                      est = rep(NA,  dat$Year %>% n_distinct(), 2), 
                      ci = rep(NA,  dat$Year %>% n_distinct(), 2)
                      
)


for (y in bnt$Year %>% unique()){
  for (l in bnt$location %>% unique()){
    
    n1 <- # number caught in first sampling (i.e., marking) period
      bnt %>% filter(Trip.Type == 'M' & Year == y & location == l) %>%
      pull(n) %>% 
      sum()
    
    n2 <- # number caught in second sampling (i.e., recapture) period
      bnt %>% filter(Trip.Type == 'R' & Year == y & location == l) %>% 
      pull(n) %>% 
      sum()
    
    m2 <- # number of marked animals caught in second sampling period
      bnt %>% 
      filter(Trip.Type == 'R' & M.C == 1 & Year == y & location == l) %>% 
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
    
    bnt_est$est[which(bnt_est$year == y & bnt_est$location == l)] <- estimate
    bnt_est$ci[which(bnt_est$year == y & bnt_est$location == l)] <- ci
    
  }
}


###############
# rbind rbt_est and bnt_est

pop_ests <- 
  rbt_est %>%
  rbind(bnt_est)

pop_ests %>%
  filter(est > 0) %>% 
  ggplot() +
  geom_errorbar(aes(x = year, ymin = est - ci, ymax = est + ci), 
                linetype = 1, colour = 'black', linewidth = 0.25) +
  geom_point(aes(x = year, y = est), size = 3) +
  geom_smooth(aes(x = year, y = est), se = FALSE, method = 'loess') +
  facet_rep_grid(location~sp) +
  xlab('Year') +
  ylab(expression("Population estimate ("~hat("N")~")")) +
  labs(title = expression(hat("N")~ "±95% CI from Chapman modification of Lincoln-Peterson")) +
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






