library(maps)
library(sf)
library(ggmap)

library(mapproj)


library(ggplot2)
library(dplyr)
require(maps)
require(viridis)

library(ggspatial)
library(ggrepel)

theme_set(
  theme_void()
)

mt_cities <- data.frame(city = c('Ennis', 'West Yellowstone', 'Helena', 'Bozeman'), 
                        lat = c(45.3491, 44.6621, 46.5891, 45.6793), 
                        long = c(-111.7319, -111.1041, -112.0391, -111.0373), 
                        type = c(rep("city", 4))
)


sampling_locations <- data.frame(
  name = c('Varney', 'Pine Butte'), 
  lat_down = c(45.29483, 44.90004), 
  lat_up = c(45.23264, 44.86503), 
  long_down = c(-111.7561, -111.5921), 
  long_up = c(-111.7515, -111.5550)
)


### 
# Montana shapefile
Montana <- st_read('./data/gis/State_of_Montana__Boundary/State_of_Montana__Boundary.shp')
ggplot() + 
  geom_sf(data = Montana, colour = 'darkgray', fill = 'gray') +
  coord_sf(default_crs = sf::st_crs(4326))

###
# more rivers?

riv <- 
  st_read('./data/gis/nhdhflowline/NHDFlowline.shp') %>%
  filter(grepl('madison', GNIS_Name, ignore.case = TRUE))

st_geometry_type(riv)
st_crs(riv)
st_bbox(riv)

# riv %>% filter(grepl('madison', GNIS_Name, ignore.case = TRUE))

ggplot() + 
  geom_sf(data = st_zm(riv), colour = 'blue')
  coord_sf(default_crs = sf::st_crs(4326))
  
####### 
# Final map
text_size = 5

# CREATE THE MAP FOR THE INSET

inset <- 
  ggplot() +
  # geom_polygon(data = montana_map, aes(x = long, y = lat, group = group), colour = 'darkgray', fill = 'gray') +
  geom_sf(data = rivers %>%
            filter(grepl(
              paste(c('Madison River', 'Ennis Lake', 'Quake Lake', 'Hebgen Lake'),
                    collapse = '|'),
              NAME)
              ),
          colour = 'gray', fill = 'gray'
        ) +
  geom_sf(data = Montana, fill = 'gray') + 
  geom_point(data = mt_cities %>% filter(city == 'Helena'), aes(x = long, y = lat), shape = "*", size = 10, fill = 'black') +
  geom_text(data = mt_cities %>% filter(city == 'Helena'), aes(x = long, y = lat, label = city),
            nudge_y = 0.75,
            # hjust = -0.5
            fontface = 'bold', 
            size = text_size) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  labs(x = NULL, y = NULL) +
  theme_test() +
  theme(legend.position = "none", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, 'pt'), 
        axis.title = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        # panel.background = element_rect(fill = 'wheat')
        ) +
  geom_rect(aes(xmin = -112, xmax = -111, ymin = 44.5, ymax = 45.5), colour = "black", fill = NA)

inset

# CREATE THE DETAIL MAP
detail <- 
  ggplot() +
  # geom_polygon(data = region, aes(x = long, y = lat, group = group), colour = 'darkgray', fill = 'gray') +
  geom_sf(data = Montana, fill = 'gray') + 
  geom_sf(data = rivers %>%
            filter(grepl(
              paste(c('Ennis Lake', 'Quake Lake', 'Hebgen Lake'),
                    collapse = '|'),
              NAME)
            ),
          colour = 'blue', fill = 'blue'
  ) +
  geom_label_repel(
    data = rivers %>%
            filter(grepl(
              paste(c('Ennis Lake', 'Quake Lake', 'Hebgen Lake'),
                    collapse = '|'),
              NAME)),
            aes(label = NAME, geometry = geometry),
    stat = 'sf_coordinates', 
    min.segment.length = 0, 
    nudge_x = c(0.2, 0.1, 0.1),
    nudge_y = c(0, 0.1, 0.15), 
    colour = 'blue', 
    size = text_size
  ) +
  geom_sf(data = st_zm(riv %>% filter(grepl('MADISON RIVER', GNIS_Name, ignore.case = TRUE))),
          colour = 'blue', fill = 'blue') +
  geom_point(data = mt_cities, aes(x = long, y = lat), size = 3) +
  geom_text(data = mt_cities, aes(x = long, y = lat, label = city), 
            nudge_x = -0.05,
            hjust = 0.95, 
            fontface = 'bold', 
            size = text_size) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  labs(x = 'Longitude', y = "Latitude") +
  theme_test() +
  theme(legend.position = "none") +
  xlim(-112, -110.9) +
  ylim(44.5, 45.5) +
  annotation_north_arrow(location = 'topright', style = north_arrow_orienteering()) +
  annotation_scale(height = unit(0.5, 'cm'), text_cex = 1.25) +
  theme(
    panel.background = element_blank(), 
    axis.text = element_text(size = 20), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title = element_text(size= 30)
    ) +
  geom_rect(data = sampling_locations, aes(xmin = long_down, xmax = long_up, 
                                            ymin = lat_up, ymax = lat_down), 
            colour = "red", fill = NA) +
  geom_text(data = sampling_locations, aes(x = long_up, y = lat_down, label = name), 
            nudge_x = -0.05,
            hjust = 1, 
            colour = 'red', 
            fontface = 'bold', 
            size = text_size)

detail

detail +
  annotation_custom(grob = ggplotGrob(inset), 
                    xmin = -111.5, xmax = -110.85, ymin = 45.0, ymax = 45.4)

ggsave(paste0("output/images/detail_inset_map.png"), 
       width = 10, height = 9, bg = "white")






