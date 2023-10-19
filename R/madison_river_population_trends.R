
library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

source("R/helper_functions.R")

# For repeatability
set.seed(256)

# Data handling. Read in the reference and state "independent" datasets, manipulate, 
# and combine
varney <- 
  read.csv('/Users/sranney/desktop/Varney20032022_fish.csv', header = T, skip = 8) %>%
  mutate(log_length = log10(Length), 
         log_weight = log10(Weight), 
         species = ifelse(Species == 'LL', "Brown", 
                          ifelse(Species == 'RB', "Rainbow", Species))
  )

varney


varney %>%
  filter(Length > 0 && Weight > 0) %>%
  ggplot(aes(x = log_length, y = log_weight)) +
  geom_point(alpha = 0.25) + 
  facet_wrap(~species)

