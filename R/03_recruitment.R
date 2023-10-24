###########
# TAKEAWAYS
#1. Brown trout < 155mm (i.e., "substock") have been decreasing since 2003. 
# The B0 and B1 terms for a linear model describing brown trout < 155m at Varney and Pine Butte 
# are significant and the slope is negative though the R^2 values for both aren't very high (.7 for Varney, 
# -0.5 for Pine Butte)

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

sub_stock <- 
  all %>% 
  filter(
    species %in% c("Brown", "Rainbow"), 
    Length > 0 & Weight > 0) %>% 
  mutate(sub_stock = ifelse(Length < 155, 1, 0)) %>% 
  group_by(
    location,
    species, 
    Year, 
    sub_stock
    ) %>% 
  summarise(n = n()) 

b <-
  sub_stock %>% 
  filter(sub_stock == TRUE) %>% 
  filter(species == "Brown")

nls_mod <- nls(formula = n~Year^a, data = b, start = c(a = 0.5), trace = TRUE)
lm_mod <- lm(n~Year, data = b)

b %>%
  ggplot() + 
  aes(x = Year, y = n, colour = species) + 
  geom_point() + 
  geom_smooth(
    method = 'nls', 
    formula = y ~ x^a, 
    aes(colour = 'Negative Exponential'),
    se = FALSE, 
    method.args = list(start=c(a = -0.1), trace = TRUE)) + 
  geom_smooth(
    method = 'lm', 
    aes(colour = 'Linear model')
    )


rec <- 
  sub_stock %>%
  filter(sub_stock == TRUE) %>%
  group_by(species) %>%
  do(mod = lm(n ~ as.numeric(as.character(Year)), data = .))

sub_stock %>%
  filter(sub_stock == TRUE) %>%
  ggplot() + 
  aes(x = Year, y = n) + 
  geom_point(aes(shape = species), size = 3) + 
  scale_shape_manual(name = "Species", 
                     values = c(16, 21), 
                     labels = c(expression(italic('Brown Trout')),
                                expression(italic('Rainbow trout')))
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
  ylab('N individuals < 155mm') +
  scale_linetype_discrete(name = "Species",
                          labels = c(expression(italic('Brown Trout')),
                                     expression(italic('Rainbow trout')))
  ) +
  labs(shape = "Species", linetype = 'Species') +
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))#,
  # panel.grid.minor = element_blank())
  
  
locations <- c('Varney', 'Pine Butte')

#Brown
print(summary((rec %>% filter(species == 'Brown' ) %>% pull(mod))[[1]]))

# Rainbow
print(summary((rec %>% filter(species == 'Rainbow' ) %>% pull(mod))[[1]]))
