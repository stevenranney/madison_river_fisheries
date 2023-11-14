
library(dplyr)
library(ggplot2)


d <- readRDS('./data/01_upper_madison.rds')

# M.C as mark/recapture?

foo <- 
  d %>% 
  filter(Year < 2023)

# foo %>% 
#   filter(Year == 2020 & species == "Brown") %>%
#   mutate(Year = as.factor(Year)) %>%
#   group_by(Year, location, Date) %>% 
#   summarize(
#     catch = n(), 
#     f_hours = unique(Shock.Time..min.)/60, 
#     c_f = catch/f_hours) %>% 
#   arrange(Year, location) %>% 
#   ggplot() +
#   aes(x = Year, y = c_f, fill = location) +
#   geom_boxplot()
# 
# dat <- foo %>%
#   filter(Length >= 155 &
#            Year == 2020 &
#     location == 'Varney' & 
#       species == "Brown") %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
#   group_by(Year, Date, Trip.Type, M.C) %>%
#   summarize(n = n())
# 
# dat

foo %>%
  filter(Trip.Type == 'R' & length_class >= 160) %>%
  group_by(Year, location, species, Date, length_class, M.C) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(Year, location, Date, species) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(Year, location, species, Date, length_class, M.C)









# ########
# #Chapman modification of the Lincoln-Peterson estimator
# 
# n1 <- # number caught in first sampling (i.e., marking) period
#   dat %>% filter(Trip.Type == 'M') %>%
#   pull(n) %>% 
#   sum()
# 
# n2 <- # number caught in second sampling (i.e., recapture) period
#   dat %>% filter(Trip.Type == 'R') %>% 
#   pull(n) %>% 
#   sum()
# 
# m2 <- # number of marked animals caught in second sampling period
#   dat %>% 
#   filter(Trip.Type == 'R' & M.C == 1) %>% 
#   pull(n) %>% 
#   sum()
# 
# estimate <- ((n1 + 1)*(n2+1))/(m2+1) -1
# 
# variance <- 
#   ((
#     (n1+1)*(n2+1)*(n1-m2)*(n2-m2)
#   )/
#   (
#     ((m2+1)^2)*(m2+2)
#   )) - 1
# 
# ci <- 1.96*sqrt(variance)
# 
# c(estimate-ci, estimate, estimate+ci)

#############


# tmp <- foo %>%
#   filter(Year == 2022) %>%
#   select(Year, Date, Length, Weight, Trip.Type, M.C) %>%
#   arrange(Length, Date)

# From anderson 1995, pg 666, mark recpature designs
tmp <- 
  foo %>%
  filter(length_class >= 160 & Trip.Type == 'R') %>%
  select(Year, location, species, length_class, M.C) %>% 
  rename(Ri = M.C)


# tmp %>% 
#   mutate(l = Ri*log(pzi) + (1-Ri)*log(1-pzi))

a <- tmp %>%
  group_by(Year, location, species, length_class, Ri) %>%
  summarize(n = n()) %>%
  arrange(Year, location, species, length_class, Ri) %>%
  mutate(Ri = as.factor(Ri))
a

b <- a %>% 
  bind_cols(
    a %>%
    ungroup() %>%
    group_by(Year, location, species, length_class) %>%
    summarize(prop = n/sum(n)) %>% 
      ungroup() %>%
    select(prop)
  )

b %>% filter(Ri == 1)

p <- b %>%
  filter(Ri == 1) %>%
  group_by(species, location, length_class) %>%
  summarize(mean_prop = mean(prop)) %>%
  ggplot() +
  aes(x = length_class, y = mean_prop) +
  geom_point(size = 3) +
  geom_smooth(method = 'loess', col = 'red', se = FALSE) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x), se = FALSE) +
  # geom_spline() +
  # facet_grid(~Ri) +
  facet_rep_grid(location~species) +
  xlab("10mm length class") +
  ylab('Probability of Capture') +
  labs(title = 'Mz: capture probability function in which capture probability\nis heterogenous among individuals and based on size') +
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


get_mle_nhat <- function(n, prop){
  
  size <- seq(0, 100*n)
  
  d <- data.frame(size = size) %>%
    mutate(L = dbinom(n, size, prop))
  
  return(d %>% filter(L == max(L)) %>% pull(size) %>% max())
}





b <- b %>% 
  filter(Ri == 1) %>%
  rowwise() %>%
  mutate(mle_nhat = get_mle_nhat(n, prop))


b %>%
  group_by(Year, location, species) %>%
  summarize(N_hat = sum(mle_nhat, na.rm = T)) %>%
  ggplot() +
  aes(x = Year, y = N_hat) +
  geom_point(size = 3) +
  geom_smooth(col = 'blue', se = FALSE) +
  # geom_smooth(method = 'lm', formula = y ~ splines::bs(x), se = FALSE) +
  facet_rep_grid(location~species) +
  xlab("Year") +
  ylab(expression("Population estimate ("~hat(N)~")")) +
  # labs(title = 'Mz: capture probability function in which capture probability\nis heterogenous among individuals and based on size') +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_rect(colour = "black", fill=NA, size=1)
        # panel.border = element_blank(), 
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )









# smoother <- 
#   ggplot_build(p)$data[[2]] %>%
#   select(x, y, ymin,  ymax, se) %>%
#   mutate(logit = log(y/(1-y)))
# 
# smoother %>%
#   ggplot() +
#   aes(x = x, y = logit) + 
#   geom_line()
#   
# 
# a %>%
#   group_by(length_class) %>%
#   summarize(n = sum(n))
# 
# # estimates
# model <- lm(prop ~ splines::bs(length_class...1), data = b %>% filter(Ri == 1))
# model_fit <- data.frame(predict(model, se = TRUE))
# 
# estimating_df <- 
#   b %>% 
#   filter(Ri == 1) %>%
#   cbind(model_fit) %>%
#   select(length_class...1, prop, fit, se.fit) %>%
#   rename(length_class = length_class...1) %>%
#   full_join(
#     a %>% 
#       group_by(length_class) %>% 
#       summarize(sampled_n = sum(n)), 
#     by = c('length_class'))
# 
# est_df <- estimating_df %>%
#   mutate(n_hat = sampled_n/fit)
# 
# est_df %>%
#   summarize(pop_est = sum(n_hat, na.rm = T))
# 
# 
# 
# # n = 20
# # N = seq(20, 150, by = 1)
# # q = 0.4#seq(0.01, 0.99, by = 0.01)
# # 
# # n_hat <- 
# #   (
# #     factorial(N)
# #     /(
# #       factorial(n)*factorial(N-n)
# #     )
# #   )*(
# #     q^(n)*(1-q)^(N-n)
# #   )
# # 
# # boo <- data.frame(N = N, n_hat = n_hat)
# # 
# # boo %>% 
# #   ggplot() +
# #   aes(x = N, y = n_hat) + 
# #   geom_line()
# # 
# 
# 
# library(reticulate)
# 
# math <- import('math', convert = FALSE)
# 
# py_run_string("x = 170; y = 1000")
# 
# math$factorial(py$x)
# math$factorial(py$y)
# 
# 
# nhat_mle <- function(n, q){
#     
#   N <- seq(1, 10*n, by = 1)
#   
#   n_hat <-
#     (
#       lfactorial(N)
#       /(
#         lfactorial(n)*lfactorial(N-n)
#       )
#     )*(
#       log(q^(n)*(1-q)^(N-n))
#     )
#   
#   # print(n_hat)
# 
#   df <- data.frame(N_hat = N, likelihood = n_hat)
# 
#   max_n_hat <-
#     df %>%
#     filter(!is.infinite(n_hat)) %>%
#     filter(n_hat == max(n_hat, na.rm = T)) %>%
#     unique()
#   print(max_n_hat)
#   
#   return(max_n_hat)
# }
#   
# 
# # est_df <- 
#   est_df %>%
#   filter(!is.na(fit)) %>%
#   ungroup() %>%
#   # tail(2) %>%
#     rowwise() %>%
#   mutate(mle_nhat = nhat_mle(sampled_n, fit))
#   
#   
#   
# log_nhat <- function(M, n., t){
#   seq(M, )
# }
#   
# 
# library(reticulate)
# 
# math <- import('math', convert = FALSE)
# 
# py_run_string("x = 170; y = 1000")
# 
# math$factorial(py$x)
# math$factorial(py$y)
# 
# 
# 
# 
#   
