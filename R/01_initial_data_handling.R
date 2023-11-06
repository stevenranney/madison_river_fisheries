

# 01. Initial data handling

# 1. Reads in raw data and handles raw data that was exported from FWP.
# 2. Adds two columns: location, and un-coded species name
# 3. row-binds columns together and writes full dataset to a single file in ./data/upper_madison.rds


library(dplyr) #code orgnization
library(lubridate) # for dates

source('./R/00_helper_functions.R')

# interim data frame that handles translating species' codes to species' names
coded_species <-
  data.frame(
    Species = c('LL', 'RB', 'RBXCT', 'WCT'),
    species = c('Brown', 'Rainbow', 'Rainbow/Cutthroat Hybrid', 'Westslope Cutthroat')
  )

varney <- 
  read.csv('./data/raw/Varney20032022_fish.csv', header = T, skip = 8) %>%
  mutate(
    location = 'Varney') %>%
  left_join(coded_species, by = 'Species')


pb <- 
  read.csv('./data/raw/PineButte20032022_fish.csv', header = T, skip = 8) %>%
  mutate(
    location = 'Pine Butte') %>%
  left_join(coded_species, by = 'Species')

#Only keep fall sampling data

varney %>%
  rbind(pb) %>%
  filter(month(as.Date(Date, format = "%m/%d/%Y")) %in% c(8, 9, 10)) %>%
  mutate(psd = ifelse(species == 'Brown', assign_bnt_psd(Length), 
                      ifelse(species == 'Rainbow', assign_rbt_psd(Length), NA)), 
         psd = factor(psd, levels = c('SS', 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))) %>%
  saveRDS('./data/01_upper_madison.rds')
