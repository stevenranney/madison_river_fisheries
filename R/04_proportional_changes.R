
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# For repeatability
set.seed(256)

all <- readRDS('./data/upper_madison.rds')

location_prop <-
  all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(location, Year, species) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(Year, location) %>%
  mutate(freq = n/sum(n))

p <- 
  location_prop %>%
  ggplot() +
  aes(x = as.numeric(as.character(Year)), y = freq) + #, colour = species) +
  geom_line(aes(linetype = species), size = 1) +
  # scale_linetype_manual(values=c("solid", "dashed")) +
  scale_colour_grey(aes(linetype = species)) +
  facet_wrap(~location) + 
  xlab('Year') +
  ylab('Proportion') +
  scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_discrete(name = "Species", 
                        labels = c('Brown Trout', 
                                   'Rainbow Trout')
                        ) +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))#, 
        # panel.grid.minor = element_blank())

p

ggsave(paste0("output/images/", Sys.Date(), "_proportional_changes.png"), plot = p, 
       width = 16, height = 9, bg = "white")



# Not split by location
prop <-
  all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(Year, species) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(freq = n/sum(n))

prop %>%
  ggplot() +
  aes(x = as.numeric(as.character(Year)), y = freq) + #, colour = species) +
  geom_line(aes(linetype = species), size = 1) +
  # scale_linetype_manual(values=c("solid", "dashed")) +
  scale_colour_grey(aes(linetype = species)) +
  xlab('Year') +
  ylab('Proportion') +
  scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_discrete(name = "Species", 
                          labels = c('Brown Trout', 
                                     'Rainbow Trout')
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))#, 
# panel.grid.minor = element_blank())
