# libraries
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)

# load the data

cities_fin <- data.frame(
  City = c("Helsinki", "Ivalo", "Joensuu", "Tampere", "Oulu"),
  Long = c(24.9384, 27.5397, 29.7636, 23.7526, 25.4651),
  Lat = c(60.1699, 68.6576, 62.6010, 61.4971, 65.0121))
crs2 <- CRS("+init=epsg:4326")
cities_sf <- st_as_sf(cities_fin, coords = c("Long", "Lat"), crs = crs2)

# general country map
suomi_outline <- ne_countries(country = c("Finland", "Sweden", "Norway", "Russia", "Estonia", "Latvia", "Lithuania", "Germany"), returnclass="sf", scale = "large")
suomi_plot <- suomi_outline %>% dplyr::select(geometry, sov_a3) 

# admin level borders
within_suomi <- ne_states(country = "Finland", returnclass="sf")
suomi_sf <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/kunta1000k_2024", layer="kunta1000k_2024Polygon")
#suomi_sf <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/maakunta1000k_2024", layer="maakunta1000k_2024Polygon")
#suomi_sf <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/suuralue1000k_2024", layer="suuralue1000k_2024Polygon")
#suomi_sf <- suomi_sf %>% dplyr::filter(kunta == "FI-13" | kunta == "009") %>% dplyr::select(geometry) 
#suomi_sf <- st_transform(suomi_sf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# kaakkois murteet
kaakkois <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-02") %>% dplyr::select(geometry) 
kaakkois2 <- suomi_sf %>% dplyr::filter(kunta == "935" | kunta == "489") %>% dplyr::select(geometry) 
kaakkois2 <- st_transform(kaakkois2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# savolais murteet
savo <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-13" | iso_3166_2 == "FI-04" | iso_3166_2 == "FI-15" | iso_3166_2 == "FI-08" | iso_3166_2 == "FI-05") %>% dplyr::select(geometry) 
savo2 <- suomi_sf %>% dplyr::filter(kunta == "989" | kunta == "759" | kunta == "403"| kunta == "005"| kunta == "052" | kunta == "781" | kunta == "081" | kunta == "233" | kunta == "934") %>% dplyr::select(geometry) 
savo2 <- st_transform(savo2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))



# export map
tiff("suomi.tiff", units="in", width=3, height=4.5, res=300)

ggplot() +
  geom_sf(data = suomi_plot, aes(geometry = geometry), color="snow2", fill="grey25", size = 0.5) +
  geom_sf(data = kaakkois, aes(geometry = geometry), fill = "slateblue2", color = NA) +
  geom_sf(data = kaakkois2, aes(geometry = geometry), fill = "slateblue2", color = NA) +
  geom_sf(data = savo, aes(geometry = geometry), fill = "mediumpurple2", color = NA) +
  geom_sf(data = savo2, aes(geometry = geometry), fill = "mediumpurple2", color = NA) +
  geom_sf_text(data = cities_sf, aes(label = City), size = 2.5, nudge_x = 0.1, nudge_y = 0.25, family = "Optima", color="snow2") +
  geom_sf(data = cities_sf, aes(geometry = geometry), shape = 4, color="snow2") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "gray10", color = "gray10"),
        plot.background = element_rect(fill = "gray10", color = "gray10")) +
  coord_sf(xlim = c(15, 36),  
           ylim = c(58, 71)) 


dev.off()













--------
  
  # add points
  ankara_point <- st_point(c(32.8597, 39.9334)) %>% 
  st_sfc(crs = st_crs(turkey_plot))
ankara <- st_buffer(ankara_point, dist = 15000)  
ankara <- st_sf(geometry = ankara)


#geom_sf(data = ankara, aes(geometry = geometry), fill="violetred", color="snow3", size = 0.5, show.legend = 'polygon') +
## the next two line use the upper unused code
