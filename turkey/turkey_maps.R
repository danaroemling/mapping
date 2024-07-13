# libraries
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)

# load the data
turkey_outline <- ne_countries(country = c("Albania", "Saudi Arabia", "Kazakhstan", "Turkmenistan", "North Macedonia", "Kosovo", "Montenegro", "Bosnia and Herzegovina", "Bulgaria", "Turkey",  "Croatia", "Republic of Serbia", "Romania", "Italy", "Moldova", "Ukraine", "Greece", "Cyprus", "Jordan", "Russia", "Georgia", "Syria", "Lebanon", "Palestine", "Hungary", "Iraq", "Israel", "Armenia", "Azerbaijan", "Iran"), returnclass="sf", scale = "large")
turkey_plot <- turkey_outline %>% dplyr::select(geometry, sov_a3) 

# add points
ankara_point <- st_point(c(32.8597, 39.9334)) %>% 
  st_sfc(crs = st_crs(turkey_plot))
ankara <- st_buffer(ankara_point, dist = 15000)  
ankara <- st_sf(geometry = ankara)

# another way to add points
## not currently used, so old values
cities <- data.frame(
  City = c("Köln", "München", "Wien", "Zürich", "Berlin", "Hamburg"),
  Long = c(6.9578, 11.5755, 16.3731, 8.5417, 13.3833, 10),
  Lat = c(50.9422, 48.1372, 48.2083, 47.3769, 52.5167, 53.55))
crs2 <- CRS("+init=epsg:4326")
cities_sf <- st_as_sf(cities, coords = c("Long", "Lat"), crs = crs2)


# export map
tiff("turkey.tiff", units="in", width=5, height=2.586, res=300)

ggplot() +
  geom_sf(data = turkey_plot, aes(geometry = geometry), color="snow2", fill="grey25", size = 0.5) +
  geom_sf(data = ankara, aes(geometry = geometry), fill="violetred", color="snow3", size = 0.5, show.legend = 'polygon') +
  ## the next two line use the upper unused code
  #geom_sf_text(data = cities_sf, aes(label = City), size = 2.5, nudge_x = 0, nudge_y = -0.15, family = "Optima") +
  #geom_sf(data = cities_sf, aes(geometry = geometry), shape = 4) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "gray10", color = "gray10"),
        plot.background = element_rect(fill = "gray10", color = "gray10")) +
  coord_sf(xlim = c(18, 49),  
           ylim = c(33, 45)) 


dev.off()

