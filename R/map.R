library(maps)
library(sf)
library(ggmap)

library(mapproj)


library(ggplot2)
library(dplyr)
require(maps)
require(viridis)

library(ggspatial)

theme_set(
  theme_void()
)

montana <- map_data('state', region = c('Montana'))

region <- map_data('state', region = c('Idaho', 'Wyoming'))

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


# montana_map %>% 
#   ggplot() +
#   aes(x = long, y = lat) +
#   geom_polygon(aes( group = group), fill = 'gray') +
#   # scale_fill_viridis_d() +
#   theme_void() +
#   theme(legend.position = "none") + 
#   coord_map(projection = 'azequalarea')

### 
# Montana shapefile
Montana <- st_read('./data/gis/State_of_Montana__Boundary/State_of_Montana__Boundary.shp')
ggplot() + 
  geom_sf(data = Montana, colour = 'blue') +
  coord_sf(default_crs = sf::st_crs(4326))



### 
# Rivers shapefile
rivers <- st_read('./data/gis/hd43p/hd43p.shp')

st_geometry_type(rivers)
st_crs(rivers)
st_bbox(rivers)

ggplot() + 
  geom_sf(data = rivers %>% filter(NAME == 'Madison River'), colour = 'blue')

inset <- 
  ggplot() +
  # geom_polygon(data = montana_map, aes(x = long, y = lat, group = group), colour = 'darkgray', fill = 'gray') +
  geom_point(data = mt_cities %>% filter(city == 'Helena'), aes(x = long, y = lat)) +
  geom_text(data = mt_cities %>% filter(city == 'Helena'), aes(x = long, y = lat, label = city),
            nudge_y = 0.75,
            hjust = 1) +
  geom_sf(data = rivers %>%
            filter(grepl(
              paste(c('Madison River', 'Ennis Lake', 'Quake Lake', 'Hebgen Lake'),
                    collapse = '|'),
              NAME)
              ),
          colour = 'gray', fill = 'gray'
        ) +
  geom_sf(data = Montana, fill = 'gray') + 
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
  geom_rect(aes(xmin = -112, xmax = -111, ymin = 44.5, ymax = 45.5), colour = "red", fill = NA)

inset

detail <- 
  ggplot() +
  # geom_polygon(data = region, aes(x = long, y = lat, group = group), colour = 'darkgray', fill = 'gray') +
  geom_sf(data = Montana, fill = 'gray') + 
  geom_sf(data = rivers %>%
            filter(grepl(
              paste(c('Madison River', 'Ennis Lake', 'Quake Lake', 'Hebgen Lake'),
                    collapse = '|'),
              NAME)
            ),
          colour = 'blue', fill = 'blue'
  ) +
  geom_point(data = mt_cities, aes(x = long, y = lat)) +
  geom_text(data = mt_cities, aes(x = long, y = lat, label = city), 
            nudge_x = -0.05,
            hjust = 1) +
  coord_sf(default_crs = sf::st_crs(4326)) +
  labs(x = 'Longitude', y = "Latitude") +
  theme_test() +
  theme(legend.position = "none") +
  xlim(-112, -110.74) +
  ylim(44.5, 45.5) +
  annotation_north_arrow(location = 'topright', style = north_arrow_orienteering()) +
  annotation_scale() +
  theme(
    panel.background = element_blank()
    ) +
  geom_rect(data = sampling_locations, aes(xmin = long_down, xmax = long_up, 
                                            ymin = lat_up, ymax = lat_down), 
            colour = "red", fill = NA) +
  geom_text(data = sampling_locations, aes(x = long_up, y = lat_down, label = name), 
            nudge_x = -0.05,
            hjust = 1, 
            colour = 'red')

detail

print(inset, vp = viewport(x = 0.634, y = 0.675, width = 0.25, height = 0.25))














  # lims(y = c(45, 46),
  #      x = c(111, 112))
  # coord_sf(
  #   xlim = c(-112, -111), 
  #   ylim = c(NA, 49))
  annotation_north_arrow(location = 'br', style = north_arrow_fancy_orienteering()) +
  # annotation_scale() + 
  scalebar(data = YK, location = "bottomright", dist = 4,
                                dist_unit = "km", transform = TRUE,  model = "WGS84")
  

### 
# Cities shapefile

towns <- st_read('./data/gis/tl_2019_30_concity/tl_2019_30_concity.shp')
ggplot() + 
  geom_sf(data = towns, colour = 'blue')

roads <- st_read('./data/gis/tl_rd22_30055_roads/tl_rd22_30055_roads.shp')


montana_map %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group), fill = 'gray') +
  geom_sf(data = roads, colour = 'blue')
  
  







  

