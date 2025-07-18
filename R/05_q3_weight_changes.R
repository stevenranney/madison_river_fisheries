###
# q3 weight changes by length category
# Takeaways:
# no real changes in Q3 or Q2 weights over the years for either brown or rainbow trout


library(ggplot2) #plotting
library(dplyr) #code orgnization/readability
library(scales)

# For repeatability
set.seed(256)


all <- readRDS('./data/01_upper_madison.rds') %>%
  filter(Year <= 2023)

all %>% group_by(species, Year) %>% summarize(total = n())


growth <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(location, Year, species, psd) %>%
  summarize(q3 = quantile(Weight, probs = 0.75, na.rm = TRUE), 
            q2 = quantile(Weight, probs = 0.5, na.rm = TRUE), 
  ) 

p <- growth %>%
  ggplot() +
  aes(x = Year,y = q3) + #, colour = species) +
  geom_line(aes(linetype = psd), size = 1) +
  # scale_linetype_manual(values=c("solid", "dashed")) +
  scale_colour_grey(aes(linetype = psd)) +
  facet_grid(species~location) +
  # facet_wrap(species~location) +
  xlab('Year') +
  ylab(bquote(3^'rd'~" Quartile Weight (g)")) +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_discrete(name = "PSD", 
                          labels = c("SS", 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))+
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )

p

ggsave(paste0("output/images/05_q3_weights.png"), plot = p, 
       width = 16, height = 9, bg = "white")



p <- growth %>%
  ggplot() +
  aes(x = Year,y = q2) + #, colour = species) +
  geom_line(aes(linetype = psd), size = 1) +
  # scale_linetype_manual(values=c("solid", "dashed")) +
  scale_colour_grey(aes(linetype = psd)) +
  facet_grid(species~location) +
  # facet_wrap(species~location) +
  xlab('Year') +
  ylab(bquote(2^'nd'~" Quartile Weight (g)")) +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_discrete(name = "PSD", 
                          labels = c("SS", 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))+
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )

p

ggsave(paste0("output/images/05_q2_weights.png"), plot = p, 
       width = 16, height = 9, bg = "white")

