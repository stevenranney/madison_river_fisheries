
# Takeaways:
# no real changes in Q3 or Q2 weights over the years for either brown or rainbow trout


library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)

# For repeatability
set.seed(256)

# Assign "trout" length categories
assign_bnt_psd <- function(data){
  
  ifelse((data>=150)&(data<230), "S-Q",
         ifelse((data>=230)&(data<300), "Q-P",
                ifelse((data>=300)&(data<380), "P-M",
                       ifelse((data>=380)&(data<460), "M-T",
                              ifelse(data>=460, ">T", "SS")))))
}

assign_rbt_psd <- function(data){
  
  ifelse((data>=250)&(data<400), "S-Q",
         ifelse((data>=400)&(data<500), "Q-P",
                ifelse((data>=500)&(data<650), "P-M",
                       ifelse((data>=650)&(data<800), "M-T",
                              ifelse(data>=800, ">T", "SS")))))
}


all <- readRDS('./data/upper_madison.rds') %>%
  filter(Year < 2023) %>%
  mutate(psd = ifelse(species == 'Brown', assign_bnt_psd(Length), 
                      ifelse(species == 'Rainbow', assign_rbt_psd(Length), NA)), 
         psd = factor(psd, levels = c('SS', 'S-Q', 'Q-P', 'P-M', 'M-T', '>T')))


growth <- 
  all %>%
  filter(species %in% c("Brown", "Rainbow")) %>%
  group_by(location, Year, species, psd) %>%
  summarize(q3 = quantile(Weight, probs = 0.75, na.rm = TRUE), 
            q2 = quantile(Weight, probs = 0.5, na.rm = TRUE), 
  ) 

growth %>%
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
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))


growth %>%
  ggplot() +
  aes(x = Year,y = q2) + #, colour = species) +
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
  theme_minimal(base_size = 20) +
  theme(legend.position = 'bottom', 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1))
