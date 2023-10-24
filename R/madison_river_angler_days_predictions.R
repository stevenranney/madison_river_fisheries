
library(dplyr)
library(ggplot2)
library(mgcv)
library(scales)

# From 2020 survey: totals and summer
# .58 is proportion of users who use "upper" river
total_angler_days <- 77453
summer_angler_days <- 58716

summer_angler_days*.58

# angler days on the upper madison river from Horton paper, 2018
actual <- data_frame(
  year = c(2011, 2013, 2015, 2016), 
  angler_days = c(90000, 135000, 155000, 180000)) %>%
  mutate(type = 'actual')

actual

mod <- lm(angler_days~year, data = actual)
summary(mod)

actual <- cbind(actual, predict(mod, interval = 'confidence'))
actual


future <- data.frame(
  year = seq(2016, 2040, by = 1), 
  angler_days = NA, 
  type = 'predicted')
future

predicted = cbind(future, predict(mod, newdata = future, interval = 'prediction'))

all <- 
  actual %>%
  rbind(predicted)

all %>%
  saveRDS("./data/predicted_angler_pressure.rds")


#Plot the observed/expted angler data
all %>%
  filter(year <= 2030) %>%
  ggplot() +
  aes(x = year, y = fit) +
  geom_point((aes(x = year, y = angler_days))) +
  geom_line(aes(linetype = type), size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  xlab("Year") +
  ylab('N anglers/year\n(upper Madison River)') +
  scale_linetype_discrete(name = "",
                          labels = c(expression(italic('Observed')),
                                     expression(italic('Predicted')))
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom',
        legend.title=element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  labs(shape = "", linetype = '')

