
library(dplyr)
library(ggplot2)


d <- readRDS('./data/01_upper_madison.rds')

# M.C as mark/recapture?

foo <- 
  d %>% 
  filter(Year < 2023)

foo %>% 
  filter(Year == 2020 & species == "Rainbow") %>%
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

dat <- foo %>%
  filter(Length >= 155 &
           Year == 2020 &
    location == 'Varney' & 
      species == "Rainbow") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  group_by(Year, Date, Trip.Type, M.C) %>%
  summarize(n = n())

dat

########
#Chapman modification of the Lincoln-Peterson estimator

n1 <- # number caught in first sampling (i.e., marking) period
  dat %>% filter(Trip.Type == 'M') %>%
  pull(n) %>% 
  sum()

n2 <- # number caught in second sampling (i.e., recapture) period
  dat %>% filter(Trip.Type == 'R') %>% 
  pull(n) %>% 
  sum()

m2 <- # number of marked animals caught in second sampling period
  dat %>% 
  filter(Trip.Type == 'R' & M.C == 1) %>% 
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

c(estimate-ci, estimate, estimate+ci)

#############


# tmp <- foo %>%
#   filter(Year == 2022) %>%
#   select(Year, Date, Length, Weight, Trip.Type, M.C) %>%
#   arrange(Length, Date)

# From anderson 1995, pg 666, mark recpature designs
tmp <- 
  foo %>%
  filter(Length >= 155 & Year == 2020 & Trip.Type == 'R' & location == 'Varney' & species == 'Rainbow') %>%
  select(length_class, M.C) %>% 
  rename(Ri = M.C)


# tmp %>% 
#   mutate(l = Ri*log(pzi) + (1-Ri)*log(1-pzi))

a <- tmp %>%
  group_by(length_class, Ri) %>%
  summarize(n = n()) %>%
  arrange(length_class, Ri) %>%
  mutate(Ri = as.factor(Ri))
a

b <- a %>% 
  cbind(
    a %>%
    ungroup() %>%
    group_by(length_class) %>%
    summarize(prop = n/sum(n)) %>% 
    select(prop)
  )
b

p <- b %>%
  filter(Ri == 1) %>%
  ggplot() +
  aes(x = length_class...1, y = prop) +
  geom_point(size = 3) +
  geom_smooth() +
  # facet_grid(~Ri) +
  xlab("10mm length class") +
  ylab('Probability of Capture') +
  labs(title = 'Mz: capture probability function in which capture probability\nis heterogenous among individuals and based on size')
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

smoother <- 
  ggplot_build(p)$data[[2]] %>%
  select(x, y, ymin,  ymax, se) %>%
  mutate(logit = log(y/(1-y)))

smoother %>%
  ggplot() +
  aes(x = x, y = logit) + 
  geom_line()
  

a %>%
  group_by(length_class) %>%
  summarize(n = sum(n))

# estimates
model <- loess(prop ~ length_class...1, data = b %>% filter(Ri == 1))
model_fit <- data.frame(predict(model, se = TRUE))

estimating_df <- 
  b %>% 
  filter(Ri == 1) %>%
  cbind(model_fit) %>%
  select(length_class...1, prop, fit, se.fit) %>%
  rename(length_class = length_class...1) %>%
  full_join(
    a %>% 
      group_by(length_class) %>% 
      summarize(sampled_n = sum(n)), 
    by = c('length_class'))

estimating_df %>%
  mutate(n_hat = sampled_n/prop) %>%
  summarize(pop_est = sum(n_hat, na.rm = T))
  

  
