###
# 05a_changes_in_Wr
# Because an initial reviewer requested it, an analysis of how mean Wr has changed
# for Brown and Rainbow Trouts on the Madison River since 2003.
# 
# Ultimately, there has been a significant decrease in mean Wr for both species
# at both locations. However, analyses of Wr data are not as robust as analyzing 
# changes in weight-at-length with Quantile Regression so these data will not be 
# included unless reviewers demand it.
#
# Outputs include:
#   * some figures
#   * a table of individual models of Wr by species, location, and length category, 
#   including B0 and 95% CIs, B1 and 95% CIs, and p-value for overall model fit.
#   

library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)
library(tidyr)
library(parameters) # fancy handling of model parameters

# For repeatability
set.seed(256)

all <- readRDS('./data/01_upper_madison.rds') %>%
  filter(Year < 2023)


growth <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow"), 
         Length >= 140) %>%
  group_by(location, Year, species, psd) %>%
  summarize(q3_wr = quantile(wr, probs = 0.75, na.rm = TRUE), 
            q2_wr = quantile(wr, probs = 0.5, na.rm = TRUE), 
  ) 

# lines for each species, each location, by psd
growth %>%
  ggplot() +
  aes(x = Year, y = q2_wr) + #, colour = species) +
  geom_line(aes(linetype = psd), size = 1) +
  # scale_linetype_manual(values=c("solid", "dashed")) +
  scale_colour_grey(aes(linetype = psd)) +
  facet_grid(species~location) +
  # facet_wrap(species~location) +
  xlab('Year') +
  ylab(bquote("Mean W"['r'])) +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_discrete(name = "PSD", 
                          labels = c("SS", 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))+
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
  )


# mean and CI for each location, species, year, psd
all %>%
  filter(species %in% c("Brown", "Rainbow"), 
         Length >= 140) %>%
  group_by(location, Year, species, psd) %>%
  summarize(mean_wr = mean(wr, na.rm = T),
            n = n(),
            n_new = sum(!is.na(wr)),
            ci = qt(p = 0.025, df = n_new) * (sd(wr, na.rm = T)/sqrt(n_new))
            )
            
# mean and CI for each location, species, year
wr_ci <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow"), 
         Length >= 140) %>%
  group_by(location, Year, species) %>%
  summarize(mean_wr = mean(wr, na.rm = T),
            n = n(),
            n_new = sum(!is.na(wr)),
            ci = qt(p = 0.025, df = n_new) * (sd(wr, na.rm = T)/sqrt(n_new))
  )

wr_ci %>%
  ggplot() +
  aes(x = Year, y = mean_wr) +
  geom_point(size = 2) + 
  geom_errorbar(aes(x = Year, y = mean_wr, ymin = mean_wr - ci, ymax = mean_wr + ci), 
                linetype = 2) + 
  geom_smooth(method = 'lm', color = 'black') +
  facet_grid(species~location) + 
  xlab('Year') +
  ylab(bquote("Mean W"['r'])) +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_linetype_discrete(name = "PSD", 
                          labels = c("SS", 'S-Q', 'Q-P', 'P-M', 'M-T', '>T'))+
  theme_minimal(base_size = 30) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(linewidth = 0.5, linetype = "solid", colour = "black"),
  )

  

### Generate a linear model from each group
lm_mods <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow"), 
         Length >= 140) %>%
  group_by(location, species, psd) %>%
  do(models = lm(wr ~ Year, data = ., na.action = na.omit))

n <- nrow(lm_mods)

lm_mod_p_values <- data.frame(
  'location' = rep(NA, n), 
  'species' = rep(NA, n),
  'psd' = rep(NA, n),
  'p_value' = rep(NA, n)
)

for(i in 1:nrow(lm_mods)){
  lm_mod_p_values$location[i] <- lm_mods$location[i]
  lm_mod_p_values$species[i] <- lm_mods$species[i]
  lm_mod_p_values$psd[i] <- as.character(lm_mods$psd[i])
  lm_mod_p_values$p_value[i] <- anova(lm_mods$models[[i]])$'Pr(>F)'[1]
  
}

mod_coefs <-
  all %>%
  filter(species %in% c("Brown", "Rainbow"), 
         Length >= 140) %>%
  group_by(location, species, psd) %>%
  group_modify(~(model_parameters(lm(wr ~ Year, data = .x, na.action = na.omit), 
                 bootstrap = TRUE, iterations = 1000))) %>%
  pivot_wider(id_cols = c(location, species, psd), 
              values_from = c(Coefficient, CI_low, CI_high, p), 
              names_from = Parameter)

lookup = c('2.5%' = c('CI_low_(Intercept)', 'CI_low_Year'), 
           'Estimate' = c('Coefficient_(Intercept)', 'Coefficient_Year'), 
           '97.5%' = c('CI_high_(Intercept)', 'CI_high_Year'), 
           'P value' = c('p_(Intercept)', 'p_Year'),
           'model p_value' = "p_value")

model_metrics_data_set <- 
  mod_coefs %>%
  left_join(lm_mod_p_values) %>%
  select(location, species, psd, 
         'CI_low_(Intercept)', 
         'Coefficient_(Intercept)', 
         'CI_high_(Intercept)', 
         'p_(Intercept)', 
         'CI_low_Year', 
         'Coefficient_Year', 
         'CI_high_Year',
         'p_Year', 
         p_value
  ) %>%
  rename(all_of(lookup)) %>%
  mutate_if(is.numeric, signif, 4)

# outputs a table of individual models of Wr by species, location, and length category, 
# and includes B0 and 95% CIs, B1 and 95% CIs, and p-value for overall model fit.
model_metrics_data_set %>%
  write.csv("output/05a_changes_in_Wr.csv", row.names = FALSE)
