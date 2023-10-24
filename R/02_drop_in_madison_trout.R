library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# source("R/helper_functions.R")

# length_cats = c(200, 330, 460, 590, 720)

# For repeatability
set.seed(256)

all <- readRDS('./data/upper_madison.rds')

reg <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(species, Year) %>%
  summarize(n = n()) %>%
  do(mod = lm(n ~ as.numeric(as.character(Year)), data = .))


all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(species, Year) %>%
  summarize(n = n()) %>%
  ggplot() +
  aes(x = Year, y = n) +
  geom_point(aes(shape = species)) + 
  scale_shape_manual(name = "Species", 
                     values = c(16, 21), 
                     labels = c(expression(italic('S. trutta')),
                                expression(italic('O. mykiss')))
  )+
  geom_smooth(
    aes(linetype = species),
    method = 'lm', 
    se = FALSE, 
    colour = 'black', 
    size = 1
  ) + 
  # scale_colour_grey(aes(linetype = species)) +
  # facet_wrap(~location) + 
  xlab("Year") +
  ylab('N individuals') +
  scale_linetype_discrete(name = "Species",
                          labels = c(expression(italic('S. trutta')),
                                     expression(italic('O. mykiss')))
  ) +
  labs(shape = "Species", linetype = 'Species') +
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))#,
# panel.grid.minor = element_blank())
