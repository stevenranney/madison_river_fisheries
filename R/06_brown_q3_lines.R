# Generates predictions of weight-at-length for all quantils 0.05 through 0.95 for 
# Brown Trout
# Generates plots of thsese data


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
all <- readRDS('./data/01_upper_madison.rds') %>%
  # filter(Year %in% c(2003, 2004, 2007, 2010, 2013, 2016, 2019, 2022)) %>%
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
  write.csv(paste0("output/06_brown_predicted_values.csv"),
            row.names = FALSE)

#-----------------------------------------------------------------------------
# One approach for obtaining estimates of the differences among years
# se="xy",R=1000, mofn=5000 is bootstrap of xy-pairs 5000 of n samples 
# made 1000 times.

all_
all_75_diff %>%
  write.csv(paste0("output/06_brown_differences_in_slope_int.csv"), 
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
  write.csv(paste0("output/06_brown_slope_int_estimates.csv"), 
            row.names = FALSE)


#-----------------------------------------------------------------------------
# For every population from every quantile seq(0.05, 0.95, by = 0.05), predict 
# weight at length at the midpoints of the Gabelhouse length categories

var_new <-
  data.frame(Year = rep(all$Year %>% unique(), 5), 
             Length = rep(c(75, 190, 265, 340, 420), each = 21), 
             Weight = rep(NA, 105))

#Empty list to store values
predicted_output <- list()

taus <- seq(0.05, 0.95, by = 0.05)

# The below for loop will takes ~9 minutes to run
start.time <- Sys.time()
for(i in 1:length(taus)){
  
  #Create model for each tau
  all_mod <- 
    all %>% 
    rq(log10(Weight)~log10(Length) + Year + log10(Length):Year, data = ., 
       contrasts = list(Year="contr.treatment"), tau = taus[i])
  
  #predict weights at length in wae_new for each tau 
  all_pred <- predict(all_mod, newdata = var_new,
                      type = "percentile", se = "boot", bsmethod = "xy", R = 1000,
                      mofn = 5000, interval = "confidence", level = 0.95)
  
  #exponentiate weights into g
  var_pred_midpoints <- 10^all_pred
  var_pred_midpoints <- data.frame(var_pred_midpoints)
  var_pred_midpoints <-
    cbind(var_new$Year,var_new$Length,var_pred_midpoints) %>%
    mutate(tau = taus[i])
  
  #Store output in the empty list
  predicted_output[[i]] <- var_pred_midpoints
  
}
end.time <- Sys.time()

time.taken <- end.time - start.time
time.taken


predicted_output <- 
  do.call("rbind", predicted_output) %>%
  as.data.frame() %>%
  rename(Year = `var_new$Year`, 
         Length = `var_new$Length`) %>%
  mutate(
    Year = Year %>% as.factor(), 
    Year = Year %>% relevel(ref = "2003"))

predicted_output %>%
  saveRDS(paste0("data/06_brown_predicted_weight_at_length.rds"))

predicted_output <- readRDS(paste0("data/06_brown_predicted_weight_at_length.rds"))
###########################################################
p <- 
  predicted_output %>%
  rename(weight = fit) %>%
  mutate(length = paste0("TL = ", Length), 
         length = factor(length, levels = c("TL = 75", "TL = 190", "TL = 265", "TL = 340", "TL = 420"))) %>% 
  filter(Year %in% c(2003, 2004, 2007, 2010, 2013, 2016, 2019, 2022)) %>%
  ggplot(aes(x = tau, y = weight, fill = Year)) +
  geom_line(aes(linetype = Year), lwd = 0.65) +
  geom_ribbon(aes(x = tau, ymin = lower, ymax = higher, fill = Year, alpha = 0.05)) +
  facet_wrap(~length, scales = "free_y") +
  labs(x= "Quantile", y = "Weight (g)") +
  scale_fill_manual(name = "Year", 
                    labels = c(2003, 2004, 2007, 2010, 2013, 2016, 2019, 2022), 
                    values = alpha(hue_pal()(8), alpha = 0.5)) +
  scale_linetype_manual(name = "Year", 
                        labels = c(2003, 2004, 2007, 2010, 2013, 2016, 2019, 2022), 
                        values = c(1,3,4,6, 7, 9, 10, 11)) +
  scale_alpha(guide = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(0.05, 0.95, by = .15)) +
  labs(tag = '(A)') + 
  theme_bw() +
  theme_minimal(base_size = 30) +
  theme(legend.position = c(.875, -0.2), 
        legend.justification = c(1, 0), 
        legend.key.size = unit(0.35, 'in'),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p


ggsave(paste0("output/images/06_brown_plots_color.png"), plot = p, 
       width = 16, height = 9, bg = "white")



