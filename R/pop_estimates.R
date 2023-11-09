
library(dplyr)
library(ggplot2)


d <- readRDS('./data/01_upper_madison.rds')

# M.C as mark/recapture?

foo <- 
  d %>% 
  filter(species == 'Brown', 
         Year < 2023)

dat <- foo %>%
  filter(location == 'Varney') %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Year, Date, M.C) %>%
  summarize(n = n())

# top <- rep(NA, dat$Date %>% unique() %>% length)
# bottom  <- rep(NA, dat$Date %>% unique() %>% length)

pop_estimates <- 
  data.frame(year = dat$Year %>% unique()) %>%
  arrange(year) %>%
  mutate(est = NA, 
         var = NA, 
         ci = NA)

years <- dat$Year %>% unique()



for (y in years){
  tmp_dat <- 
    foo %>%
    filter(Year == y & location == 'Varney') %>% #filter for a given year
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    group_by(Year, Date, M.C) %>%
    summarize(n = n())
  # stop()
  
  top <- rep(NA, tmp_dat$Date %>% unique() %>% length())
  bottom  <- rep(NA, tmp_dat$Date %>% unique() %>% length())
  
  for (d in 1:(tmp_dat$Date %>% unique() %>% length())){

    nd <- #number of fish caught in the dth sample, for a single day
      tmp_dat %>%
      filter(Date == (tmp_dat$Date %>% unique())[d]) %>% pull(n) %>% sum()

    
    Md <- #number of marked fish present in the population for the dth sample, essentially all of the previous fish captured
      tmp_dat %>%
      filter(Date <= (tmp_dat$Date %>% unique())[d]) %>% pull(n) %>% sum()
    
    top[d] <- nd*Md
    
    
    md <- # number of fish caught in the dth sample
      tmp_dat %>%
      filter(
        Date == (tmp_dat$Date %>% unique())[d], 
        M.C == 1) %>% pull(n) %>% sum()
    
    bottom[d] <- md + 1

  }
  
  estimate <- sum(top)/sum(bottom)
  
  variance <- estimate^2*((estimate/sum(top)) + (2* ((estimate^2)/(sum(top)^2))) + (6 * (estimate^3)/(sum(top)^3)))
  
  
  pop_estimates$est[which(years == y)] <- estimate
  pop_estimates$var[which(years == y)] <- variance
  pop_estimates$ci[which(years == y)] <- 1.96*sqrt(variance)
  
  
  
}

mod <- lm(est~year, data = pop_estimates)


pop_estimates %>%
  ggplot() +
  geom_point(aes(x = year, y = est), size = 3) +
  geom_smooth(aes(x = year, y = est), method = 'lm') +
  xlab("Year") +
  ylab('Population estimate') +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(), 
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )#,

summary(mod)

