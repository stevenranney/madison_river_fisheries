
library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# source("R/helper_functions.R")

length_cats = c(20, 33, 46, 59, 72)

# For repeatability
set.seed(256)

# Data handling. Read in the reference and state "independent" datasets, manipulate, 
# and combine
varney <- 
  read.csv('/Users/sranney/desktop/Varney20032022_fish.csv', header = T, skip = 8) %>%
  mutate(Year = as.factor(Year),
         species = ifelse(Species == 'LL', "Brown", 
                          ifelse(Species == 'RB', "Rainbow", Species))
  )

varney %>%
  filter(Length > 0 && Weight > 0) %>%
  ggplot(aes(x = log10(Length), y = log10(Weight))) +
  geom_point(alpha = 0.25) + 
  facet_wrap(~species)

varney_ref <-
  varney %>%
  filter(Year == 2004)

#################################################################################
# Table of quantiles and predictions of weight-at-length

# Five quantiles at once with their predictions by 10mm increments
varney_brown_ref_10to90 <-
  varney_ref %>%
  filter(species == 'Brown') %>%
  rq(log10(Weight)~log10(Length), data = ., tau = c(0.10, 0.25, 0.50, 0.75, 0.90))

by10mm <- 
  data.frame(Length = seq(50, 720, by = 10))

predict_by_10mm <- 
  predict(varney_brown_ref_10to90, newdata = by10mm, confidence = none)

# Create, as much as possible, the prediction table in R so not as much 
# Excel or Word formatting needs to be done.
predict_by_10mm <- 
  10^(predict_by_10mm) %>% #Exponentiate all values
  round(1) %>% #Round to 1 decimal place
  comma() %>% #Add comma
  cbind(by10mm, .) %>%
  # rename(`Total length (mm)` = Length, 
  #       "tau= 0.10" = `0.10`, 
  #       "tau= 0.25" = `0.25`, 
  #       "tau= 0.50" = `0.50`, 
  #       "tau= 0.75" = `0.75`, 
  #       "tau= 0.90" = `0.90`)

# predict_by_10mm %>%
#   write.csv(paste0("output/", Sys.Date(), "_predicted_values.csv"), 
#             row.names = FALSE)

#-----------------------------------------------------------------------------
# One approach for obtaining estimates of the differences among populations
# se="xy",R=1000, mofn=5000 is bootstrap of xy-pairs 5000 of n samples 
# made 1000 times.

varney <-
  varney %>%
  filter(Length > 0 && Weight > 0) %>%
  filter(Year %in% c('2003', '2004', '2009', '2011', '2014', '2016', '2019', '2021'))

# Make the ref data the base level in this estimate of 0.75 quantile.
varney <-
  varney %>%
  mutate(Year = Year %>% relevel(ref = "2003"))

varney_75 <- 
  varney %>% 
  rq(log10(Weight)~log10(Length) + Year + log10(Length):Year, data = ., 
     contrasts = list(Year="contr.treatment"), tau = 0.75)

varney_75_diff <- summary(varney_75, se = "boot", bsmethod = "xy", R = 1000, mofn = 5000)

wae_75_diff <- 
  data.frame(wae_75_diff$coef) %>%
  mutate(name = row.names(.)) %>%
  select(name, Value, Std..Error, t.value, Pr...t..) %>%
  rename(Estimate = Value, 
         SE = `Std..Error`,
         `t value` = t.value, 
         `p value` = `Pr...t..`)

# Calculate 95% confidence intervals around the estimate of the differences in 
# slope/int among populationsusing bootstrap estimates of SE.
resid_df <- nrow(wae_75$x) - ncol(wae_75$x)

wae_75_diff <- 
  wae_75_diff %>%
  mutate(Lwr95CI = Estimate + SE * qt(0.025,resid_df), 
         Upr95CI = Estimate + SE * qt(0.975,resid_df)) %>%
  select(name, Lwr95CI, Estimate, Upr95CI, `t value`, `p value`)

wae_75_diff %>%
  write.csv(paste0("output/", Sys.Date(), "_differences_in_slope_int.csv"), 
            row.names = FALSE)

# Retrieve slope and intercept for each population
# Same model as above but removing the intercept term so that I can find slope/int
# estimates for each population, including ref
wae_75_slope_int <- 
  wae %>% 
  rq(log10(weight)~State + log10(length):State - 1, data = ., 
     contrasts = list(State = "contr.treatment"), tau = 0.75)

wae_75_slope_int_est <- summary(wae_75_slope_int, se = "boot", bsmethod = "xy", R = 1000, mofn = 5000)

wae_75_slope_int_est <- 
  data.frame(wae_75_slope_int_est$coefficients) %>%
  mutate(name = row.names(.)) %>%
  select(name, Value, Std..Error, t.value, Pr...t..) %>%
  rename(`Point estimate` = Value, 
         SE = `Std..Error`,
         `t value` = t.value, 
         `p value` = `Pr...t..`)


###Calculate 95% confidence intervals using bootstrap estimates of SE.
resid_df <- nrow(wae_75_slope_int$x) - ncol(wae_75_slope_int$x)

wae_75_slope_int_est <- 
  wae_75_slope_int_est %>%
  mutate(Lwr95CI = `Point estimate` + SE * qt(0.025, resid_df), 
         Upr95CI = `Point estimate` + SE * qt(0.975, resid_df)) %>%
  select(name, Lwr95CI, `Point estimate`, Upr95CI)

wae_75_slope_int_est %>%
  write.csv(paste0("output/", Sys.Date(), "_slope_int_estimates.csv"), 
            row.names = FALSE)



