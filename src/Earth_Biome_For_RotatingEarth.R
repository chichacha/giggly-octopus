library(tidyverse)
library(sf)
library(skimr)
library(rmapshaper)
library(see)
library(ggforce)

## https://www.sciencebase.gov/catalog/file/get/508fece8e4b0a1b43c29ca22?f=__disk__2a%2F91%2Fd8%2F2a91d8fd78e7896658bf164531b74ac0c08a9253&transform=1&allowOpen=true
biome <- read_sf("~/Downloads/TerrestrialEcos/")

biome_df <-biome %>% st_drop_geometry()
biome_sf <- biome %>% ms_simplify(keep=0.01)

biome_df %>% count(BIOME)

test <-biome %>%
  filter(BIOME==9) %>%
  ms_simplify()

##https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html
?ms_simplify()

world<-map_data("world")

biome_sf %>%
  filter(!is.na(G200_REGIO)) %>%
  ggplot() +
  geom_shape(data=world, aes(x=long,y=lat,group=group), fill="black") +
  geom_sf(aes(fill=factor(BIOME)), color="#ffffff00") +
  scale_fill_material() +
  theme_void()

g200 <-biome_sf %>%
  filter(!is.na(G200_REGIO)) %>%
  arrange(BIOME) %>%
  count(BIOME,G200_REGIO,wt=AREA,sort=T) 

biome_name <- c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves")


g200 %>% 
  ggplot() +
  geom_shape(data=world, aes(x=long,y=lat,group=group), fill="black") +
  geom_sf(aes(fill=factor(BIOME)), color="#ffffff00") +
  geom_sf_text(aes(label=G200_REGIO), data = . %>% slice_head(n=20),
               family="Roboto Condensed", color="white", size=8/.pt) +
  scale_fill_material(labels=biome_name) +
  theme_void(base_family="Roboto Condensed") +
  theme(plot.background=element_rect(fill="grey30"))

ggsave("output/world_biome.pdf", width=21, height=7, device=cairo_pdf)


