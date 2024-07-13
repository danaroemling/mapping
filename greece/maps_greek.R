# libraries
library(sf)
library(sp)
library(tidyverse)
library(gstat)
library(stringr) 
library(dplyr) 
library(scales)
library(classInt)
library(viridis)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggforce)

greek_outline <- ne_countries(country = c("Albania", "Saudi Arabia", "Bulgaria", "Turkey", "Greece", "Bosnia and Herzegovina", "Malta", "Cyprus", "North Macedonia", "Kosovo", "Montenegro", "Jordan", "Croatia", "Republic of Serbia", "Romania", "Italy", "Moldova", "Ukraine", "Russia", "Georgia", "Syria", "Lebanon", "Palestine", "Iraq", "Israel", "Hungary"), returnclass="sf", scale = "large")
greek_plot <- greek_outline %>% dplyr::select(geometry, sov_a3) 
greek_spatial <- as_Spatial(greek_plot) 

cyprus <- greek_plot %>% filter(sov_a3 == "CYP")

crete_bbox <- st_as_sfc(st_bbox(c(xmin = 23.4, xmax = 26.5, ymin = 34.7, ymax = 35.7), crs = st_crs(greek_plot)))
crete <- greek_plot %>%
  filter(sov_a3 == "GRC") %>%
  st_crop(crete_bbox)

kozani_point <- st_point(c(21.7850, 40.3012)) %>% 
  st_sfc(crs = st_crs(greek_plot))
kozani <- st_buffer(kozani_point, dist = 10000)  
kozani <- st_sf(geometry = kozani)

grevena_point <- st_point(c(21.4273, 40.0838)) %>% 
  st_sfc(crs = st_crs(greek_plot))
grevena <- st_buffer(grevena_point, dist = 10000)  
grevena <- st_sf(geometry = grevena)

#europe <- ne_download(scale = "large", type = "admin_1_states_provinces_lines", category = "cultural", returnclass="sf")
#europe_filter <- europe %>%
#  filter(iso_a2 == c("MK", "GR", "BG", "TR"))


#mac1 <- greek_outline %>% filter(sov_a3 == "MKD") %>% dplyr::select(geometry) 
#bulg <- ne_states(country = "bulgaria", returnclass = "sf") 
#mac2 <- bulg %>% dplyr::filter(iso_3166_2 == "BG-01" | iso_3166_2 == "BG-13" | iso_3166_2 =="BG-21" | 
#                                 iso_3166_2 =="BG-16" | iso_3166_2 =="BG-09" | iso_3166_2 =="BG-26" | 
#                                 iso_3166_2 =="BG-24" | iso_3166_2 =="BG-20" | iso_3166_2 =="BG-28" | 
#                                 iso_3166_2 =="BG-02") %>% dplyr::select(geometry) 
gree <- ne_states(country = "greece", returnclass = "sf") 
mac3 <- gree %>% dplyr::filter(iso_3166_2 == "GR-B" | iso_3166_2 == "GR-A" | iso_3166_2 =="GR-C" | iso_3166_2 =="GR-69") %>% dplyr::select(geometry) 
#turk <- ne_states(country = "turkey", returnclass = "sf") 
#mac4 <- turk %>% dplyr::filter(iso_3166_2 == "TR-22" | iso_3166_2 == "TR-39" | iso_3166_2 == "TR-59" | iso_3166_2 == "TR-34") %>% dplyr::select(geometry) 
#shape <- rbind(mac3, mac4)
shape <- mac3



#tiff("schau_guck_comparison_sixth_isogloss.tiff", units="in", width=3.78, height=4.8, res=300)
jpeg("greece_pink_new.jpg", units="in", width=5, height=3.275, res=300)

ggplot() +
  geom_sf(data = greek_plot, aes(geometry = geometry), color="snow2", fill="grey25", size = 0.5) +
  #geom_sf(data = cyprus, aes(geometry = geometry), fill="darkorange3", color=NA, show.legend = 'polygon') +
  #geom_sf(data = crete, aes(geometry = geometry), fill="seagreen", color=NA, show.legend = 'polygon') +
  #geom_sf(data = shape, aes(geometry = geometry), fill="mediumpurple", color=NA, show.legend = 'polygon') +
  geom_sf(data = kozani, aes(geometry = geometry), fill="violetred", color="snow3", size = 0.5, show.legend = 'polygon') +
  geom_sf(data = grevena, aes(geometry = geometry), fill="violetred", color="snow3", size = 0.5, show.legend = 'polygon') +
  #geom_sf(data = greek_plot, aes(geometry = geometry), color="snow2", fill=NA, size = 0.5) +
  theme_minimal() +
#  guides(fill = guide_legend(override.aes = list(size = 4)), position = "left" ) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "gray10", color = "gray10"),
        plot.background = element_rect(fill = "gray10", color = "gray10")) +
  coord_sf(xlim = c(18, 37),  
            ylim = c(43.5, 34)) 
 

dev.off()

















--
  
  
  
  
  macedonia_bbox <- st_as_sfc(st_bbox(c(xmin = 20, xmax = 24, ymin = 39, ymax = 41), crs = st_crs(greek_plot)))

macedonia <- greek_plot %>%
  filter(sov_a3 == "GRC") %>%
  st_crop(macedonia_bbox) %>%
  st_simplify(dTolerance = 10000, preserveTopology = TRUE)



thraki_countries <- ne_countries(country = c("Greece", "Bulgaria", "Turkey"), returnclass="sf", scale = "large")

thraki_bbox <- st_as_sfc(st_bbox(c(xmin = 20, xmax = 30, ymin = 40, ymax = 42), crs = st_crs(thraki_countries)))
thraki_bbox <- st_as_sfc(st_bbox(c(xmin = 20, xmax = 30, ymin = 39, ymax = 42), crs = st_crs(thraki_countries)))
#thraki_bbox <- st_as_sfc(st_bbox(c(xmin = 24, xmax = 27, ymin = 40.5, ymax = 41.5), crs = st_crs(greek_plot)))

thraki <- thraki_countries %>%
  st_crop(thraki_bbox) %>%
  st_simplify(dTolerance = 10000, preserveTopology = TRUE)

#thraki <- greek_plot %>%
#  filter(sov_a3 == "GRC") %>%
#  st_crop(thraki_bbox) %>%
#  st_simplify(dTolerance = 10000, preserveTopology = TRUE)
