

# 01. Initial data handling

# 1. Reads in raw data and handles raw data that was exported from FWP.
# 2. Adds two columns: location, and un-coded species name
# 3. row-binds columns together and writes full dataset to a single file in ./data/upper_madison.rds


library(dplyr) #code orgnization

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


varney %>%
  rbind(pb) %>%
  saveRDS('./data/upper_madison.rds')
