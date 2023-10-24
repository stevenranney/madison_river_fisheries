
library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# source("R/helper_functions.R")

# length_cats = c(200, 330, 460, 590, 720)

# For repeatability
set.seed(256)

# Data handling. Read in the reference and state "independent" datasets, manipulate, 
# and combine
all <- readRDS('./data/upper_madison.rds') %>%
  filter(Year %in% c(2003, 2004, 2007, 2010, 2013, 2016, 2019, 2022)) %>%
  mutate(Year = as.factor(Year))



#################################################################################
# BROWN TROUT
# Table of quantiles and predictions of weight-at-length

# Five quantiles at once with their predictions by 10mm increments
brown_ref_10to90 <-
  all %>%
  filter(species == 'Brown', 
         Length > 0 & Weight > 0) %>%
  rq(log10(Weight)~log10(Length), data = ., tau = c(0.10, 0.25, 0.50, 0.75, 0.90))

by10mm <- 
  data.frame(Length = seq(50, 720, by = 10))

predict_by_10mm <- 
  predict(brown_ref_10to90, newdata = by10mm, confidence = none)

# Create, as much as possible, the prediction table in R so not as much 
# Excel or Word formatting needs to be done.
predict_by_10mm <- 
  10^(predict_by_10mm) %>% #Exponentiate all values
  round(1) %>% #Round to 1 decimal place
  data.frame() %>%
  # comma() %>% #Add comma
  cbind(by10mm) %>%
  rename(`Total length (mm)` = Length,
         `0.10` = "tau..0.10",
         `0.25` = "tau..0.25",
         `0.50` = "tau..0.50",
         `0.75` = "tau..0.75",
         `0.90` = "tau..0.90")

predict_by_10mm %>%
  write.csv(paste0("output/", Sys.Date(), "_brown_predicted_values.csv"),
            row.names = FALSE)

#-----------------------------------------------------------------------------
# One approach for obtaining estimates of the differences among years
# se="xy",R=1000, mofn=5000 is bootstrap of xy-pairs 5000 of n samples 
# made 1000 times.

all <-
  all %>%
  filter(species == 'Brown') %>%
  filter(Length > 0 & Weight > 0)

# Make the ref data the base level in this estimate of 0.75 quantile.
all <-
  all %>%
  mutate(Year = Year %>% relevel(ref = "2003"))

all_75 <- 
  all %>% 
  rq(log10(Weight)~log10(Length) + Year + log10(Length):Year, data = .,
     contrasts = list(Year="contr.treatment"), tau = 0.75)

all_75_diff <- summary(all_75, se = "boot", bsmethod = "xy", R = 1000, mofn = 5000)

all_75_diff <- 
  data.frame(all_75_diff$coef) %>%
  mutate(name = row.names(.)) %>%
  select(name, Value, Std..Error, t.value, Pr...t..) %>%
  rename(Estimate = Value, 
         SE = `Std..Error`,
         `t value` = t.value, 
         `p value` = `Pr...t..`)

# Calculate 95% confidence intervals around the estimate of the differences in 
# slope/int among populationsusing bootstrap estimates of SE.
resid_df <- nrow(all_75$x) - ncol(all_75$x)

all_75_diff <- 
  all_75_diff %>%
  mutate(Lwr95CI = Estimate + SE * qt(0.025,resid_df), 
         Upr95CI = Estimate + SE * qt(0.975,resid_df)) %>%
  mutate_if(is.numeric, round, 4) %>%
  select(name, Lwr95CI, Estimate, Upr95CI, `t value`, `p value`)

all_75_diff %>%
  write.csv(paste0("output/", Sys.Date(), "_brown_differences_in_slope_int.csv"), 
            row.names = FALSE)

# Retrieve slope and intercept for each population
# Same model as above but removing the intercept term so that I can find slope/int
# estimates for each population, including ref
all_75_slope_int <- 
  all %>% 
  rq(log10(Weight)~Year + log10(Length):Year - 1, data = ., 
     contrasts = list(Year = "contr.treatment"), tau = 0.75)

all_75_slope_int_est <- summary(all_75_slope_int, se = "boot", bsmethod = "xy", R = 1000, mofn = 5000)

all_75_slope_int_est <- 
  data.frame(all_75_slope_int_est$coefficients) %>%
  mutate(name = row.names(.)) %>%
  select(name, Value, Std..Error, t.value, Pr...t..) %>%
  rename(`Point estimate` = Value, 
         SE = `Std..Error`,
         `t value` = t.value, 
         `p value` = `Pr...t..`)

###Calculate 95% confidence intervals using bootstrap estimates of SE.
resid_df <- nrow(all_75_slope_int$x) - ncol(all_75_slope_int$x)

all_75_slope_int_est <- 
  all_75_slope_int_est %>%
  mutate(Lwr95CI = `Point estimate` + SE * qt(0.025, resid_df), 
         Upr95CI = `Point estimate` + SE * qt(0.975, resid_df)) %>%
  mutate_if(is.numeric, round, 4) %>%
  select(name, Lwr95CI, `Point estimate`, Upr95CI)

all_75_slope_int_est %>%
  write.csv(paste0("output/", Sys.Date(), "_brown_slope_int_estimates.csv"), 
            row.names = FALSE)


#-----------------------------------------------------------------------------
# For every population from every quantile seq(0.05, 0.95, by = 0.05), predict 
# weight at length at the midpoints of the Gabelhouse length categories

var_new <-
  data.frame(Year = rep(all$Year %>% unique(), 5), 
             Length = rep(c(100, 200+(330-200)/2, 330+(460-330)/2, 460+(590-460)/2, 590+(720-590)/2), each = 7), 
             Weight = rep(NA, 35))

#Empty list to store values
predicted_output <- list()

taus <- seq(0.05, 0.95, by = 0.05)

# The below for loop will takes ~9 minutes to run
for(i in 1:length(taus)){
  
  #Create model for each tau
  var_mod <- 
    varney %>% 
    rq(log10(Weight)~log10(Length) + Year + log10(Length):Year, data = ., 
       contrasts = list(Year="contr.treatment"), tau = taus[i])
  
  #predict weights at length in wae_new for each tau 
  var_pred <- predict(var_mod, newdata = var_new,
                      type = "percentile", se = "boot", bsmethod = "xy", R = 1000,
                      mofn = 5000, interval = "confidence", level = 0.95)
  
  #exponentiate weights into g
  var_pred_midpoints <- 10^var_pred
  var_pred_midpoints <- data.frame(var_pred_midpoints)
  var_pred_midpoints <-
    cbind(var_new$Year,var_new$Length,var_pred_midpoints) %>%
    mutate(tau = taus[i])
  
  #Store output in the empty list
  predicted_output[[i]] <- var_pred_midpoints
  
}

predicted_output <- 
  do.call("rbind", predicted_output) %>%
  as.data.frame() %>%
  rename(Year = `var_new$Year`, 
         Length = `var_new$Length`) %>%
  mutate(
    # year = ifelse(state == "GA2", "GA1", 
    #                       ifelse(state == "GA3", "GA2", 
    #                              ifelse(state == "GA4", "GA3", 
    #                                     ifelse(state == "SD4", "SD1", 
    #                                            ifelse(state == "SD13", "SD2", 
    #                                                   ifelse(state == "SD25", "SD3", "Reference")))))), 
    Year = Year %>% as.factor(), 
    Year = Year %>% relevel(ref = "2003"))

predicted_output %>%
  saveRDS(paste0("data/", Sys.Date(), "_brown_predicted_weight_at_length.rds"))

