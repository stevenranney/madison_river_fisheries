library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# source("R/helper_functions.R")

length_cats <- c(200, 330, 460, 590, 720)

# For repeatability
set.seed(256)

# Data handling. Read in the reference and state "independent" datasets, manipulate, 
# and combine
varney <- 
  read.csv('./data/raw/Varney20032022_fish.csv', header = T, skip = 8) %>%
  mutate(Year = as.factor(Year),
         species = ifelse(Species == 'LL', "Brown", 
                          ifelse(Species == 'RB', "Rainbow", Species))
  )

###################################################################################

brown <- varney %>%
  filter(species == 'Brown')


brown %>%
  filter(Length > 0 & Weight > 0) %>%
  group_by(Year) %>%
  summarize(
    mean = mean(log10(Length), na.rm = T), 
    n = n())


brown %>%
  filter(Year %in% c('2003', '2004', '2009', '2011', '2014', '2016', '2019', '2021')) %>%
  filter(Length > 0 & Weight > 0) %>%
  ggplot() +
  aes(x = log10(Weight), group = Year, colour = Year) + 
  geom_density() +
  facet_wrap(~Year)

brown %>%   
  # filter(Year %in% c('2003', '2004', '2009', '2011', '2014', '2016', '2019', '2021')) %>%
  filter(Length > 0 & Weight > 0) %>% 
  group_by(Year) %>%
  summarize(
    weight_q3 = quantile(log10(Length), probs = 0.75), 
    mean_length = mean(log10(Length))) %>%
  ggplot() +
  aes(x = Year, y = weight_q3) + 
  geom_point()
