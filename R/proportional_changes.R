
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# For repeatability
set.seed(256)

all <- readRDS('./data/upper_madison.rds')

prop <-
  all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(location, Year, species) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(Year, location) %>%
  mutate(freq = n/sum(n))

prop %>%
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
                        labels = c(expression(italic('S. trutta')), 
                                   expression(italic('O. mykiss')))
                        ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))#, 
        # panel.grid.minor = element_blank())


# scale_fill_discrete(
#   "Transmission",
#   breaks = c(0, 1),
#   labels = c("Automatic", expression(italic("Manual")))

# theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
#       axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
#       plot.title = element_text(size = 20, face = "bold", color = "darkgreen"))