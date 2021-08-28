
library(readr)
library(sf)

us_zip <- read_delim("~/Downloads/US/US.txt", 
                 delim = "\t", escape_double = FALSE, 
                 col_names = FALSE, trim_ws = TRUE)

names(us_zip) <- c("country_code","postal_code","place_name","admin_1","admin_cd1","admin_2","admin_cd2","admin_3","admin_cd3","latitude","longitude","accuracy")
us_zip$accuracy_f <- factor(us_zip$accuracy,levels=c(1,4,6),labels=c("estimated","geonameid","centroid"))

us_zip %>% count(accuracy_f,accuracy,sort=T)

# country code      : iso country code, 2 characters
# postal code       : varchar(20)
# place name        : varchar(180)
# admin name1       : 1. order subdivision (state) varchar(100)
# admin code1       : 1. order subdivision (state) varchar(20)
# admin name2       : 2. order subdivision (county/province) varchar(100)
# admin code2       : 2. order subdivision (county/province) varchar(20)
# admin name3       : 3. order subdivision (community) varchar(100)
# admin code3       : 3. order subdivision (community) varchar(20)
# latitude          : estimated latitude (wgs84)
# longitude         : estimated longitude (wgs84)
# accuracy          : accuracy of lat/lng from 1=estimated, 4=geonameid, 6=centroid of addresses or shape

us_zip_50 %>% filter(longitude>0)

us_zip_50 <- us_zip %>% filter(admin_cd1 %in% c(state.abb,"DC")) %>% filter(longitude<=0)
us_zip_50 <-us_zip_50 %>% mutate(admin_1_f = fct_reorder(admin_1, longitude+latitude, min))

us_zip_main <- us_zip_50 %>% filter(longitude>=-128)

us_zip_special <- us_zip %>% filter(!admin_cd1 %in% state.abb)

us_hull_main <-chull(x=us_zip_main$longitude, y=us_zip_main$latitude)
us_zip_main[us_hull_main,] %>% select(longitude,latitude)

res<-deldir::deldir(x=us_zip_main$longitude,y=us_zip_main$latitude)
us_main_aug <- us_zip_main %>% 
  left_join(res$summary %>% select(longitude=x,latitude=y,n.tri:dir.wts))

us_main_aug %>% sample_n(10) %>% DT::datatable()

us_zip_main %>% 
  #filter(between(longitude,-74.93888,-72.35855) & between(latitude,39.53158,41.51889)) %>%
  ggplot(aes(x=longitude,y=latitude)) +
  #geom_point(aes(color=admin_1_f),size=0.1)+
  geom_voronoi_tile(aes(group=1, fill=admin_1_f), color="#ffffff", size=0.3, max.radius=1) +
  geom_voronoi_tile(aes(group=1, alpha=del.area), color="#ffffff00",fill="#ffffff", size=0.3, max.radius=1, data=us_main_aug) +
  # geom_polygon(data=us_zip_main[us_hull_main,] %>% select(longitude,latitude), fill="#ffffff00", color="black") +
  geom_text_repel(aes(x=lon,y=lat,label=city), family="Roboto Condensed",
            data = urban_df2 %>% filter(lon<0&lon>=-128&lat>20) %>% filter(rank<=100), size=3) +
  scale_color_material(guide="none") +
  scale_x_continuous(breaks=scales::pretty_breaks(n=10)) +
  theme_void() +
  scale_fill_material(guide="none") +
  scale_alpha_continuous(range=c(0.5,0), guide="none") +
  coord_map()

ggsave("output/US_Map_Voronoi_Alpha2.pdf", width=16, height=12, device=cairo_pdf)

us_zip_50 %>% filter(is.na(accuracy)) %>% 
  select(place_name,postal_code, latitude,longitude, admin_cd1, admin_2) %>%
  as_tibble()


#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2010.html
## I'm using 2010 so that I can cross reference with wikipedia article. 
## https://en.wikipedia.org/wiki/List_of_United_States_urban_areas#:~:text=An%20urbanized%20area%20(UA)%20is,of%20a%20micropolitan%20statistical%20area.
urban <- sf::st_read("~/Downloads/cb_2012_us_uac10_500k/")

urban %>% as_tibble() %>% select(NAME10, NAMELSAD10, UATYP10)
urban_pt <-urban %>% 
  filter(UATYP10=="U") %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble()

urban_name <- urban %>% 
  filter(UATYP10=="U") %>%
  st_drop_geometry()

urban_df <- bind_cols(urban_name,urban_pt) %>%
  group_by(NAME10) %>%
  summarise(n=n(), lon=mean(X),lat=mean(Y)) %>%
  mutate(NAME10 = str_replace_all(NAME10,"--","-")) %>%
  separate(NAME10, into=c("city","region"),sep=",", remove=F) 

library(ggrepel)
library(rvest)
library(fuzzyjoin)


urban_pop <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_urban_areas#:~:text=An%20urbanized%20area%20(UA)%20is,of%20a%20micropolitan%20statistical%20area.") %>%
  html_table() %>% pluck(2) %>%
  janitor::clean_names() %>% rename(NAME10=name_note_1) %>%
  mutate(pop_2010 = as.numeric(gsub(",","",population_2010_census)))

urban_pop$NAME10[c(1:100)]

urban_df2 <- urban_df %>% stringdist_left_join(urban_pop,method="jw",max_dist=0.5,distance_col="dist") %>%
  group_by(NAME10.x) %>%
  mutate(closest=row_number(dist)) %>%
  filter(closest==1) %>%
  ungroup() %>% arrange(rank)


urban_df %>%
  filter(lon<0&lon>=-128&lat>20) %>%
  filter(n>5) %>%
  ggplot() +
  geom_text(aes(x=lon,y=lat,label=city), family="Roboto Condensed",
            data = urban_df %>% filter(lon<0&lon>=-128&lat>20) ) +
  theme_void() 

ggsave("output/US_Map_UrbanCityName.pdf", width=16, height=12, device=cairo_pdf)

ny_test <-urban %>% filter(NAME=="New York--Newark, NY--NJ--CT") %>%
  st_coordinates()

urban %>% filter(NAME=="New York--Newark, NY--NJ--CT") %>%
  st_bbox()




us_outline %>% 
  filter(!R_STATEFP %in% c("02","72","15")) %>%
  st_union() %>%
  ggplot() +
  geom_sf() +
  theme_void() +
  coord_sf() 

ggsave("output/US_Outline.pdf", width=16, height=12)

simple_test <-tibble(x=rnorm(10),
       y=rbeta(10,1,1)) 



library(deldir)
?deldir
