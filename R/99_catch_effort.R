library(dplyr)
library(ggplot2)
library(ggthemes)
library(lemon)
library(lubridate)

d <- readRDS('./data/01_upper_madison.rds')


tmp <- d %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  filter(Shock.Time..min. != 0) %>%
  group_by(Year, Date, location, species) %>%
  summarize(catch = n(), 
            total_hr = unique(Shock.Time..min.)/60, 
            c_f = catch/total_hr)

tmp %>%
  filter(Year >= 2009 & Year < 2023) %>%
  ggplot() + 
  aes(x = Year, y = c_f, group = Year) +
  geom_boxplot() +
  geom_point(jitter = 5, alpha = 0.5) +
  facet_rep_grid(location~species) +
  # facet_wrap(species~location) +
  xlab('Year') +
  ylab("Catch/Effort (hr)") +
  # scale_y_continuous(limits = c(0, 1)) +
  # scale_linetype_discrete(name = "PSD", 
  #                         labels = c("SS", 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))+
  # theme_tufte(base_size = 30) +
  # theme(legend.position = 'bottom', 
  #       axis.line = element_line())
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        
  )



tmp %>%
  filter(Year >= 2009 & Year < 2023) %>%
  ggplot() + 
  aes(x = Year, y = c_f) +
  geom_point(size = 3) +
  geom_smooth(method = 'loess', linetype = 2, colour = 'black') + 
  facet_rep_grid(location~species) +
  # facet_wrap(species~location) +
  xlab('Year') +
  ylab("Catch/Effort (hr)") +
  # scale_y_continuous(limits = c(0, 1)) +
  # scale_linetype_discrete(name = "PSD", 
  #                         labels = c("SS", 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))+
  # theme_tufte(base_size = 30) +
  # theme(legend.position = 'bottom', 
  #       axis.line = element_line())
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        
  )



d %>%
  filter(species == 'Brown' & Year == 2020) %>%
  select(Date, Shock.Time..min.) %>%
  unique()