library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# source("R/helper_functions.R")

# length_cats = c(200, 330, 460, 590, 720)

# For repeatability
set.seed(256)

all <- readRDS('./data/upper_madison.rds') %>%
  filter(Year < 2023)

data_desc <-
  all %>% 
  group_by(species, Year) %>%
  summarize(n = n())

data_desc %>% group_by(species) %>% summarize(tot = sum(n), mean = mean(n), sd = sd(n))

data_desc %>% 
  group_by(species) %>% 
  summarize(tot = sum(n)) %>%
  mutate(prop = tot/sum(tot), 
         cum_prop = cumsum(prop))


reg <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow"), 
         Year < 2023) %>%
  group_by(species, Year) %>%
  summarize(n = n()/2) %>%
  do(mod = lm(n ~ as.numeric(as.character(Year)), data = .))

print("Brown")
summary((reg %>% filter(species == 'Brown') %>% pull(mod))[[1]])

print("Rainbow")
summary((reg %>% filter(species == 'Rainbow') %>% pull(mod))[[1]])

# All fish
all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(species, Year) %>%
  summarize(n = n()/2) %>%
  ggplot() +
  aes(x = Year, y = n) +
  geom_point(aes(shape = species), size = 3) + 
  scale_shape_manual(name = "Species",
                     values = c(16, 21),
                     labels = c('Brown trout',
                                'Rainbow trout')
  )+
  geom_smooth(
    aes(linetype = species),
    method = 'lm',
    se = FALSE,
    colour = 'black',
    size = 1
  ) +
  # scale_colour_grey(aes(linetype = species)) +
  # facet_wrap(~species) +
  xlab("Year") +
  ylab('N individuals/river mile') +
  scale_linetype_discrete(name = "Species",
                          labels = c('Brown trout',
                                     'Rainbow trout')
  ) +
  labs(shape = "Species", linetype = 'Species') +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))#,

# All fish, psd based
all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(species, psd, Year) %>%
  summarize(n = n()/2) %>%
  ggplot() +
  aes(x = Year, y = n) +
  geom_line(aes(colour = psd, linetype = psd), size = 1.5) + 
  # scale_shape_manual(name = "Species", 
  #                    values = c(16, 21),
  #                    labels = c(expression(italic('Brown trout')),
  #                               expression(italic('Rainbow trout')))
  # )+
  # geom_smooth(
  #   aes(linetype = species),
  #   method = 'lm', 
  #   se = FALSE, 
  #   colour = 'black', 
  #   size = 1
# ) + 
# scale_colour_grey(aes(linetype = species)) +
facet_wrap(~species) +
  xlab("Year") +
  ylab('N individuals/river mile') +
  scale_linetype_discrete(name = "PSD",
                          labels = c('SS', 'S-Q', 'Q-P', 'P-M', 'M-T', ">T")
  ) +
  labs(shape = "PSD", linetype = 'PSD', colour = 'PSD') +
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))#,
# panel.grid.minor = element_blank())
