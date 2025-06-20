# used to create and write images for all years for both brown and rainbow trout predictions 
# of weight at length

library(quantreg) 
library(ggplot2) #plotting
library(dplyr) #code orgnization
library(scales)


browns <- readRDS(paste0("data/06_brown_predicted_weight_at_length.rds"))

rainbows <- predicted_output <- readRDS(paste0("data/06_rainbow_predicted_weight_at_length.rds"))

###########################################################

create_yearly_q3_lines <- function(df, year, species){
  
  if (species == 'brown') {
    levels = c("TL = 75", "TL = 190", "TL = 265", "TL = 340", "TL = 420") 
  } else if (species == 'rainbow') {
    levels = c("TL = 125", "TL = 325", "TL = 450", "TL = 575", "TL = 725")  
  } 
  
  p <- df %>%
    rename(weight = fit) %>%
    mutate(length = paste0("TL = ", Length), 
           length = factor(length, levels = levels)) %>% 
    filter(Year %in% c(2003, 2004, year)) %>%
    ggplot(aes(x = tau, y = weight, fill = Year)) +
    geom_line(aes(linetype = Year), lwd = 0.65) +
    geom_ribbon(aes(x = tau, ymin = lower, ymax = higher, fill = Year, alpha = 0.05)) +
    facet_wrap(~length, scales = "free_y") +
    labs(x= "Quantile", y = "Weight (g)") +
    scale_fill_manual(name = "Year", 
                      labels = c(2003, 2004, year), 
                      values = alpha(hue_pal()(8), alpha = 0.5)) +
    scale_linetype_manual(name = "Year", 
                          labels = c(2003, 2004, year), 
                          values = c(1,3,4)) +
    scale_alpha(guide = "none") +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = seq(0.05, 0.95, by = .15)) +
    labs(tag = '(A)') + 
    theme_bw() +
    theme_minimal(base_size = 30) +
    theme(legend.position = c(.875, -0.2), 
          legend.justification = c(1, 0), 
          legend.key.size = unit(0.35, 'in'),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          strip.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  ggsave(paste0("output/images/", year, "_", species, "_plots_color.png"), plot = p, 
         width = 16, height = 9, bg = "white")
  
}

years <- seq(2005, 2023, 1)

for (i in 1:length(years)) {
  create_yearly_q3_lines(browns, years[i], "brown")
  create_yearly_q3_lines(rainbows, years[i], "rainbow")
}