# libraries
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(sp)
library(spatstat)

# map basics
cities_fin <- data.frame(
  City = c("Ivalo", "Joensuu", "Tampere", "Rovaniemi"),
  Long = c(27.5397, 29.7636, 23.7526, 25.7294),
  Lat = c(68.6576, 62.6010, 61.4971, 66.5039))
crs2 <- CRS("+init=epsg:4326")
cities_sf <- st_as_sf(cities_fin, coords = c("Long", "Lat"), crs = crs2)

cities_fin_2 <- data.frame(
  City = c("Helsinki"),
  Long = c(24.9384),
  Lat = c(60.1699))
cities_sf_2 <- st_as_sf(cities_fin_2, coords = c("Long", "Lat"), crs = crs2)

# general country map
suomi_outline <- ne_countries(country = c("Finland", "Sweden", "Norway", "Russia", "Estonia", "Latvia", "Lithuania", "Germany"), returnclass="sf", scale = "large")
suomi_plot <- suomi_outline %>% dplyr::select(geometry, sov_a3) 
overlay_outline_all <- ne_countries(country = c("Sweden", "Norway", "Russia", "Estonia", "Latvia", "Lithuania", "Germany"), returnclass="sf", scale = "large")
overlay_outline_suomi <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/suomi/data", layer="FIN_adm0")

# admin level borders
within_suomi <- ne_states(country = "Finland", returnclass="sf")
suomi_sf <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/kunta1000k_2024", layer="kunta1000k_2024Polygon")
#suomi_sf <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/maakunta1000k_2024", layer="maakunta1000k_2024Polygon")
#suomi_sf <- st_read(dsn="/Users/dana/Documents/R/Misc/suomi/suuralue1000k_2024", layer="suuralue1000k_2024Polygon")
#suomi_sf <- suomi_sf %>% dplyr::filter(kunta == "FI-13" | kunta == "009") %>% dplyr::select(geometry) 
#suomi_sf <- st_transform(suomi_sf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ahven <- suomi_sf %>% dplyr::filter(kunta == "035" | kunta == "043" | kunta == "060" | 
                                      kunta == "062" | kunta == "065" | kunta == "076" | 
                                      kunta == "170" | kunta == "295" | kunta == "318" |
                                      kunta == "417" | kunta == "438" | kunta == "478" |
                                      kunta == "736" | kunta == "766" | kunta == "771" | 
                                      kunta == "941" |
                                      # vasrinais
                                      kunta == "445" | kunta == "322") %>% dplyr::select(geometry) 
ahven_sf <- st_transform(ahven, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## ITÄ MURTEET
# kaakkois murteet
kaakkois <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-02") %>% dplyr::select(geometry) 
kaakkois2 <- suomi_sf %>% dplyr::filter(kunta == "935" | kunta == "489") %>% dplyr::select(geometry) 
kaakkois2 <- st_transform(kaakkois2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# savolais murteet
savo <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-13" | iso_3166_2 == "FI-04" | iso_3166_2 == "FI-15" | iso_3166_2 == "FI-08" | iso_3166_2 == "FI-05") %>% dplyr::select(geometry) 
savo2 <- suomi_sf %>% dplyr::filter(kunta == "989" | kunta == "759" | kunta == "403"| kunta == "005"| kunta == "052" | kunta == "781" | kunta == "081" | kunta == "233" | kunta == "934" | kunta == "626" | kunta == "305" | kunta == "614" | kunta == "832") %>% dplyr::select(geometry) 
savo2 <- st_transform(savo2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


## LÄNSI MURTEET
perä <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-10") %>% dplyr::select(geometry) 
hämä <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-06" | iso_3166_2 == "FI-11" | 
                                         iso_3166_2 == "FI-16" | iso_3166_2 == "FI-09" | 
                                         iso_3166_2 == "FI-17") %>% dplyr::select(geometry) 
hämä2 <- suomi_sf %>% dplyr::filter(kunta == "430" | kunta == "434" | kunta == "018" | 
                                      kunta == "504" | kunta == "611" | kunta == "245" | 
                                      kunta == "753" | kunta == "186" | kunta == "858" |
                                      kunta == "091" | kunta == "092" | kunta == "049" |
                                      kunta == "106" | kunta == "543" | kunta == "505" | 
                                      kunta == "407" | kunta == "616" | kunta == "235") %>% dplyr::select(geometry) 
hämä2 <- st_transform(hämä2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
eteläpoh <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-03") %>% dplyr::select(geometry) 
eteläpoh2 <- suomi_sf %>% dplyr::filter(kunta == "399" | kunta == "152") %>% dplyr::select(geometry) 
eteläpoh2 <- st_transform(eteläpoh2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
kesk <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-14" | iso_3166_2 == "FI-07") %>% dplyr::select(geometry) 
kesk2 <- suomi_sf %>% dplyr::filter(kunta == "683" | kunta == "288") %>% dplyr::select(geometry) 
kesk2 <- st_transform(kesk2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lounais <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-19") %>% dplyr::select(geometry) 
lounais2 <- suomi_sf %>% dplyr::filter(kunta == "684" | kunta == "051" | kunta == "050" | 
                                         kunta == "783" | kunta == "444" | kunta == "927" | 
                                         kunta == "224") %>% dplyr::select(geometry) 
lounais2 <- st_transform(lounais2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Isogloss
#itä <- rbind(kaakkois, kaakkois2, savo, savo2)
#länsi <- rbind(hämä, hämä2, perä, eteläpoh2, eteläpoh, svenska, kesk, kesk2, lounais2, lounais)
#border <- st_intersection(st_geometry(itä),
#                          st_geometry(länsi))

## SVENSKA
svenska <- suomi_sf %>% dplyr::filter(kunta == "710" | kunta == "078" | kunta == "149" | kunta == "755" | kunta == "257" | kunta == "638" |
                                        # ahvenanmaa
                                        kunta == "035" | kunta == "043" | kunta == "060" | 
                                        kunta == "062" | kunta == "065" | kunta == "076" | 
                                        kunta == "170" | kunta == "295" | kunta == "318" |
                                        kunta == "417" | kunta == "438" | kunta == "478" |
                                        kunta == "736" | kunta == "766" | kunta == "771" | 
                                        kunta == "941" | 
                                        # varsinais
                                        kunta == "445" | kunta == "322" |
                                        # pohjanmaa
                                        kunta == "440" | kunta == "598" | kunta == "893" |
                                        kunta == "499" | kunta == "905" | kunta == "475" |
                                        kunta == "280" | kunta == "545" | kunta == "287" | 
                                        kunta == "946" | kunta == "599"
                                        ) %>% dplyr::select(geometry) 
svenska <- st_transform(svenska, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
svenska2 <- within_suomi %>% dplyr::filter(iso_3166_2 == "FI-21") %>% dplyr::select(geometry) 




## MAPS
# Eastern Dialects
tiff("suomi_itä.tiff", units="in", width=3, height=4.23, res=300)
ggplot() +
  geom_sf(data = suomi_plot, aes(geometry = geometry), color = NA, fill="grey25", size = 0.5) +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = NA, fill="grey25", size = 0.5) +
  geom_sf(data = kaakkois, aes(geometry = geometry), fill = "slateblue2", color = "slateblue2") +
  geom_sf(data = kaakkois2, aes(geometry = geometry), fill = "slateblue2", color = "slateblue2") +
  geom_sf(data = savo, aes(geometry = geometry), fill = "mediumpurple2", color = "mediumpurple2") +
  geom_sf(data = savo2, aes(geometry = geometry), fill = "mediumpurple2", color = "mediumpurple2") +
  geom_sf(data = overlay_outline_all, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.1) +
  geom_sf(data = overlay_outline_suomi, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.1) +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.03) +
  geom_sf_text(data = cities_sf, aes(label = City), size = 1.2, nudge_x = 0, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf_text(data = cities_sf_2, aes(label = City), size = 1.2, nudge_x = -0.3, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf(data = cities_sf, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2) +
  geom_sf(data = cities_sf_2, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "gray10", color = "gray10"),
        plot.background = element_rect(fill = "gray10", color = "gray10")) +
  annotate(geom="text", x = 30.4, y = 60.9, label= "Kaakkoismurteet", size = 1.2, family = "Optima", color = "slateblue2", fontface = "italic") +
  annotate(geom="text", x = 32.2, y = 64., label= "Savolaismurteet", size = 1.2, family = "Optima", color = "mediumpurple2", fontface = "italic") +
  coord_sf(xlim = c(15, 36),  
           ylim = c(58, 71)) 
dev.off()


# Westtern Dialects
tiff("suomi_länsi.tiff", units="in", width=3, height=4.23, res=300)
ggplot() +
  geom_sf(data = suomi_plot, aes(geometry = geometry), color = "grey25", fill="grey25") +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "grey25", fill="grey25") +
  geom_sf(data = perä, aes(geometry = geometry), fill = "midnightblue", color = "midnightblue") +
  geom_sf(data = hämä, aes(geometry = geometry), fill = "thistle3", color = "thistle3") +
  geom_sf(data = hämä2, aes(geometry = geometry), fill = "thistle3", color = "thistle3") +
  geom_sf(data = lounais, aes(geometry = geometry), fill = "mediumvioletred", color = "mediumvioletred") +
  geom_sf(data = lounais2, aes(geometry = geometry), fill = "mediumvioletred", color = "mediumvioletred") +
  geom_sf(data = kesk, aes(geometry = geometry), fill = "cyan4", color = "cyan4") +
  geom_sf(data = kesk2, aes(geometry = geometry), fill = "cyan4", color = "cyan4") +
  geom_sf(data = eteläpoh, aes(geometry = geometry), fill = "tomato3", color = "tomato3") +
  geom_sf(data = eteläpoh2, aes(geometry = geometry), fill = "tomato3", color = "tomato3") +
  # colour over grey the other areas
  geom_sf(data = savo2, aes(geometry = geometry), fill = "grey25", color = "grey25", lwd = 0.3) +
  geom_sf(data = kaakkois2, aes(geometry = geometry), fill = "grey25", color = "grey25", lwd = 0.3) +
  geom_sf(data = svenska, aes(geometry = geometry), fill = "grey25", color = "grey25") +
  geom_sf(data = svenska2, aes(geometry = geometry), fill = "grey25", color = "grey25") +
  # general map again
  geom_sf(data = overlay_outline_all, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.1) +
  geom_sf(data = overlay_outline_suomi, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.05) +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.03) +
  geom_sf_text(data = cities_sf, aes(label = City), size = 1.2, nudge_x = 0, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf_text(data = cities_sf_2, aes(label = City), size = 1.2, nudge_x = -0.3, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf(data = cities_sf, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2) +
  geom_sf(data = cities_sf_2, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2) +  
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "gray10", color = "gray10"),
        plot.background = element_rect(fill = "gray10", color = "gray10")) +
  annotate(geom="text", x = 31.3, y = 68.6, label= "Peräpohjalaiset murteet", size = 1.2, family = "Optima", color = "midnightblue", fontface = "italic") +
  annotate(geom="text", x = 19.5, y = 61.7, label= "Hämäläismurteet", size = 1.2, family = "Optima", color = "thistle3", fontface = "italic") +
  annotate(geom="text", x = 19.5, y = 60.9, label= "Lounaismurteet", size = 1.2, family = "Optima", color = "mediumvioletred", fontface = "italic") +
  annotate(geom="text", x = 27.5, y = 63.5, label= "Keski- ja \nPohjoispohjalaiset murteet", size = 1.2, family = "Optima", color = "cyan4", fontface = "italic") +
  annotate(geom="text", x = 26.5, y = 62.7, label= "Eteläpohjalaiset murteet", size = 1.2, family = "Optima", color = "tomato3", fontface = "italic") +
  coord_sf(xlim = c(15, 36),  
           ylim = c(58, 71)) 
dev.off()



# Swedish Areas
tiff("suomi_svenska.tiff", units="in", width=3, height=4.23, res=300)
ggplot() +
  geom_sf(data = suomi_plot, aes(geometry = geometry), color = NA, fill="grey25", size = 0.5) +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "grey25", fill="grey25") +
  geom_sf(data = svenska, aes(geometry = geometry), fill = "goldenrod2", color = "goldenrod2") +
  geom_sf(data = svenska2, aes(geometry = geometry), fill = "goldenrod2", color = "goldenrod2") +
  geom_sf(data = overlay_outline_all, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.1) +
  geom_sf(data = overlay_outline_suomi, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.05) +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.03) +
  geom_sf_text(data = cities_sf, aes(label = City), size = 1.2, nudge_x = 0, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf_text(data = cities_sf_2, aes(label = City), size = 1.2, nudge_x = -0.3, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf(data = cities_sf, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2, lwd = 0.2) +
  geom_sf(data = cities_sf_2, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2, lwd = 0.2) +  
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = "gray10", color = "gray10"),
        plot.background = element_rect(fill = "gray10", color = "gray10")) +
  annotate(geom="text", x = 19.5, y = 61.1, label= "Finlandssvenska", size = 1.2, family = "Optima", color = "goldenrod2", fontface = "italic") +
  coord_sf(xlim = c(15, 36),  
           ylim = c(58, 71)) 
dev.off()



## COMBINED MAP
tiff("suomi_kaikki.tiff", units="in", width=3, height=4.23, res=300)
ggplot() +
  geom_sf(data = suomi_plot, aes(geometry = geometry), color = "grey25", fill="grey25") +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "grey25", fill="grey25") +
  # länsi
  geom_sf(data = perä, aes(geometry = geometry), fill = "midnightblue", color = "midnightblue") +
  geom_sf(data = hämä, aes(geometry = geometry), fill = "thistle3", color = "thistle3") +
  geom_sf(data = hämä2, aes(geometry = geometry), fill = "thistle3", color = "thistle3") +
  geom_sf(data = lounais, aes(geometry = geometry), fill = "mediumvioletred", color = "mediumvioletred") +
  geom_sf(data = lounais2, aes(geometry = geometry), fill = "mediumvioletred", color = "mediumvioletred") +
  geom_sf(data = kesk, aes(geometry = geometry), fill = "cyan4", color = "cyan4") +
  geom_sf(data = kesk2, aes(geometry = geometry), fill = "cyan4", color = "cyan4") +
  geom_sf(data = eteläpoh, aes(geometry = geometry), fill = "tomato3", color = "tomato3") +
  geom_sf(data = eteläpoh2, aes(geometry = geometry), fill = "tomato3", color = "tomato3") +
  # itä
  geom_sf(data = kaakkois, aes(geometry = geometry), fill = "slateblue2", color = "slateblue2") +
  geom_sf(data = kaakkois2, aes(geometry = geometry), fill = "slateblue2", color = "slateblue2") +
  geom_sf(data = savo, aes(geometry = geometry), fill = "mediumpurple2", color = "mediumpurple2") +
  geom_sf(data = savo2, aes(geometry = geometry), fill = "mediumpurple2", color = "mediumpurple2") +
  # svenska
  geom_sf(data = svenska, aes(geometry = geometry), fill = "goldenrod2", color = "goldenrod2") +
  geom_sf(data = svenska2, aes(geometry = geometry), fill = "goldenrod2", color = "goldenrod2") +
  # general map again
  geom_sf(data = overlay_outline_all, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.1) +
  geom_sf(data = overlay_outline_suomi, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.05) +
  geom_sf(data = ahven_sf, aes(geometry = geometry), color = "snow2", fill = NA, lwd = 0.03) +
  geom_sf_text(data = cities_sf, aes(label = City), size = 1.2, nudge_x = 0, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf_text(data = cities_sf_2, aes(label = City), size = 1.2, nudge_x = -0.3, nudge_y = 0.15, family = "Optima", color="snow2") +
  geom_sf(data = cities_sf, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2) +
  geom_sf(data = cities_sf_2, aes(geometry = geometry), shape = 1, color="snow2", size = 0.2) +  
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



