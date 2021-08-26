

col_pp <- str_c("#",c("006466", "065a60", "0b525b", "144552", "1b3a4b", "212f45", "272640", "312244", "3e1f47", "4d194d"))

col_pp2 <- str_c("#",c("4f000b", "720026", "ce4257", "ff7f51", "ff9b54"))
col_pp2

library(leaflet)

## Phoneix Arizona  33.44717305639869, -112.0738204674498
lon <- -112.0738204674498
lat <-33.44717305639869
method <- "driving"
name = ""

draw_map <- function(lon,lat,method,name){
  
  # lon=-115.1532265507853
  # lat=36.0839224256778
  # method = "cycling"
  # name = "Las Vegas"
  
  name_clean <- janitor::make_clean_names(name)
  
  url1 <- (str_glue("https://api.mapbox.com/isochrone/v1/mapbox/{method}/{lon}%2C{lat}?contours_minutes=5%2C10%2C15%2C20&polygons=true&denoise=1&access_token={Sys.getenv('MAPBOX')}"))
  url2<- (str_glue("https://api.mapbox.com/isochrone/v1/mapbox/{method}/{lon}%2C{lat}?contours_minutes=25%2C30%2C35%2C40&polygons=true&denoise=1&access_token={Sys.getenv('MAPBOX')}"))
  url3<- (str_glue("https://api.mapbox.com/isochrone/v1/mapbox/{method}/{lon}%2C{lat}?contours_minutes=45%2C50%2C55%2C60&polygons=true&denoise=1&access_token={Sys.getenv('MAPBOX')}"))
  
  tmp1 <-st_read(url1)
  tmp2 <- st_read(url2)
  tmp3 <- st_read(url3)
  
  
  iso_sf <- bind_rows(list("drive_1"=tmp1,"drive_2"=tmp2, "drive_3"=tmp3))
  iso_sf <-iso_sf %>% arrange(desc(contour)) #%>%
    #mutate(geometry=st_cast(geometry,"POLYGON"))
  
  iso_sf2 <- st_transform(iso_sf,crs=3857)
  iso_sf2_origin <- tibble(lon=lon,lat=lat) %>%
    st_as_sf(coords=c("lon","lat")) %>%
    st_set_crs(4326) %>%
    st_transform(3857) 
  
  if(method=="walking"){
    iso_sf2_origin$geometry <- st_buffer(iso_sf2_origin$geometry,dist=5000)
    iso_sf2_origin$txt <- "5km"
  }else if(method=="cycling"){
    iso_sf2_origin$geometry <- st_buffer(iso_sf2_origin$geometry,dist=15000)
    iso_sf2_origin$txt <- "15km"
  }else{
    iso_sf2_origin$geometry <- st_buffer(iso_sf2_origin$geometry,dist=50000)
    iso_sf2_origin$txt <- "50km"
  }
  
  
  p <-iso_sf2 %>%
    ggplot() +
    geom_sf(aes(fill=contour), color="#ffffff00") +
    geom_sf(data = iso_sf2_origin, color="black", fill="#ffffff00")+
    scale_fill_gradientn(colors=col_pp2, guide="none") +
    theme_void()
  
  print(p)
  
  p2 <-iso_sf2_origin %>%
    st_transform(crs=4326) %>%
    leaflet() %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolylines() %>%
    addPolygons(data=iso_sf, color=~"#F24E4E",weight=1)

  print(p2)
    
  
  #st_bbox(iso_sf)
  
  # iso_sf %>% ggplot() +
  #   geom_sf(aes(fill=contour), color="#ffffff00") +
  #   scale_fill_gradientn(colors=col_pp) +
  #   theme_void()
  # 
  # bb <- iso_sf$geometry[1]
  # res <- opq(bbox=st_bbox(iso_sf)) %>%
  #    add_osm_feature(key="aeroway") %>%
  #    osmdata_sf() %>%
  #    trim_osmdata(bb)
  
  # res2 <- opq(bbox=st_bbox(iso_sf)) %>%
  #   add_osm_feature(key="boundary", value="administrative") %>%
  #   osmdata_sf() %>%
  #   trim_osmdata(bb)
  
  #  moter_road <- res$osm_lines
  #  st_crs(moter_road) <- 4326
  #  building <- res$osm_polygons
  #  st_crs(building) <- 4326
  # #admin <- res2$osm_polygons
  # #st_crs(admin) <-4326
  # 
  # p <-iso_sf %>% ggplot() +
  #   geom_sf(aes(fill=contour), color="#ffffff00") +
  #   geom_sf(data=building, fill="#00000030", color="#ffffff00") +
  #   geom_sf(data=moter_road, color="#ffffffde") +
  #   scale_fill_gradientn(colors=col_pp2, guide="none") +
  #   theme_void()
  # 
  # print(p)
  
}

#49.28320820636919, -123.12040329343608
draw_map(lon=-123.12040329343608, lat=49.28320820636919, method="walking",name="Vancouver") +
  draw_map(lon=-123.12040329343608, lat=49.28320820636919, method="cycling",name="Vancouver") +
  draw_map(lon=-123.12040329343608, lat=49.28320820636919, method="driving",name="Vancouver")

draw_map(lon=-123.12040329343608, lat=49.28320820636919, method="driving",name="Vancouver")

ggsave("output/Vancouver_3Maps.pdf", width=16*3, height=16)

#Shibuya Hachico 35.65994705262562, 139.70075804749467
draw_map(lon=139.70075804749467, lat=35.65994705262562, method="walking",name="Shibuya") +
  draw_map(lon=139.70075804749467, lat=35.65994705262562, method="cycling",name="Shibuya") +
  draw_map(lon=139.70075804749467, lat=35.65994705262562, method="driving",name="Shibuya")

ggsave("output/Shibuya_3Maps.pdf", width=16*3, height=16)

draw_map(lon=139.70075804749467, lat=35.65994705262562, method="driving",name="Shibuya")



## Phoneix Arizona  33.44717305639869, -112.0738204674498
draw_map(lon=-112.0738204674498,lat=33.44717305639869,method="walking",name="Phoenix Arizona") +
  draw_map(lon=-112.0738204674498,lat=33.44717305639869,method="cycling",name="Phoenix Arizona") +
  draw_map(lon=-112.0738204674498,lat=33.44717305639869,method="driving",name="Phoenix Arizona") 
  
ggsave("output/Phonenix_3Maps.pdf", width=16*3, height=16)


## Rome
## Trevi Mountain 41.902411410673665, 12.483012061986356
draw_map(lon=12.483012061986356,lat=41.902411410673665,method="walking",name="TreviFountain_Rome") +
  draw_map(lon=12.483012061986356,lat=41.902411410673665,method="cycling",name="TreviFountain_Rome") +
  draw_map(lon=12.483012061986356,lat=41.902411410673665,method="driving",name="TreviFountain_Rome")

ggsave("output/Rome_3Maps.pdf", width=16*3, height=16)


## UC Berkely 37.87330969382552, -122.25853565647158
draw_map(lon=-122.25853565647158, lat=37.87330969382552,method="walking", name="UCBerkeley") +
  draw_map(lon=-122.25853565647158, lat=37.87330969382552,method="cycling", name="UCBerkeley") +
  draw_map(lon=-122.25853565647158, lat=37.87330969382552,method="driving", name="UCBerkeley")

ggsave("output/Berkeley_3Maps.pdf", width=16*3, height=16)


#51.50858443139137, -0.08775151979390969 London Bridge
draw_map(lon=-0.08775151979390969,lat=51.50858443139137,method="walking",name="London Bridge") +draw_map(lon=-0.08775151979390969,lat=51.50858443139137,method="cycling",name="London Bridge") +draw_map(lon=-0.08775151979390969,lat=51.50858443139137,method="driving",name="London Bridge") 
ggsave("output/London_3Maps.pdf", width=16*3, height=16)


#40.75864109884177, -73.98545676846578 Times Square
draw_map(lon=-73.98545676846578,lat=40.75864109884177,method="walking",name="TimesSquare_NYC")+draw_map(lon=-73.98545676846578,lat=40.75864109884177,method="cycling",name="TimesSquare_NYC")+draw_map(lon=-73.98545676846578,lat=40.75864109884177,method="driving",name="TimesSquare_NYC")

ggsave("output/NYC_3Maps.pdf", width=16*3, height=16)


## Jakarta Indonesia -6.1679114307661225, 106.82731535718816
draw_map(lon=106.82731535718816,lat=-6.1679114307661225,method="walking",name="Jakarta")+draw_map(lon=106.82731535718816,lat=-6.1679114307661225,method="cycling",name="Jakarta")+draw_map(lon=106.82731535718816,lat=-6.1679114307661225,method="driving",name="Jakarta")
ggsave("output/Jakarta_3Maps.pdf", width=16*3, height=16)


## Denpasar Airport -8.746494503945266, 115.16641148863778
draw_map(lon=115.16641148863778,lat=-8.746494503945266,method="walking",name="Denpasar Airport")+draw_map(lon=115.16641148863778,lat=-8.746494503945266,method="cycling",name="Denpasar Airport")+draw_map(lon=115.16641148863778,lat=-8.746494503945266,method="driving",name="Denpasar Airport")
ggsave("output/Denpasar_Bali_3Maps.pdf", width=16*3, height=16)

draw_map(lon=115.16641148863778,lat=-8.746494503945266,method="cycling",name="Denpasar Airport")


### Las Vegas Airport 36.0839224256778, -115.1532265507853
draw_map(lon=-115.1532265507853,lat=36.0839224256778,method="walking",name="Las Vegas Airport")+draw_map(lon=-115.1532265507853,lat=36.0839224256778,method="cycling",name="Las Vegas Airport")+draw_map(lon=-115.1532265507853,lat=36.0839224256778,method="driving",name="Las Vegas Airport")
ggsave("output/LasVegasAirport_3Maps.pdf", width=16*3, height=16)


### Caltrain Station San Francisco 37.776832181736204, -122.39483236673678
draw_map(lon=-122.39483236673678,lat=37.776832181736204,method="walking",name="SF_Caltrain")+draw_map(lon=-122.39483236673678,lat=37.776832181736204,method="cycling",name="SF_Caltrain")+draw_map(lon=-122.39483236673678,lat=37.776832181736204,method="driving",name="SF_Caltrain")
ggsave("output/SF_CaltrainStation_3Maps.pdf", width=16*3, height=16)


## Lombok Airport -8.756321817579886, 116.27252513714524
draw_map(lon=116.27252513714524,lat=-8.756321817579886,method="walking",name="")+draw_map(lon=116.27252513714524,lat=-8.756321817579886,method="cycling",name="")+draw_map(lon=116.27252513714524,lat=-8.756321817579886,method="driving",name="")
ggsave("output/Lombok_3Maps.pdf", width=16*3, height=16)


### Tofino 49.15303165144173, -125.90669154546724
draw_map(lon=-125.90669154546724,lat=49.15303165144173,method="walking",name="")+
  draw_map(lon=-125.90669154546724,lat=49.15303165144173,method="cycling",name="")+
  draw_map(lon=-125.90669154546724,lat=49.15303165144173,method="driving",name="")
ggsave("output/Tofino_3Maps.pdf", width=16*3, height=16)


## 35.32038010242603, 139.55051667903828 Kamakura Station
draw_map(lon=139.55051667903828,lat=35.32038010242603,method="walking",name="")+
  draw_map(lon=139.55051667903828,lat=35.32038010242603,method="cycling",name="")+
  draw_map(lon=139.55051667903828,lat=35.32038010242603,method="driving",name="")
ggsave("output/Kamakura_3Maps.pdf", width=16*3, height=16)


### Four Courners 37.00933092716633, -109.04509298794375
draw_map(lon=-109.04509298794375,lat=37.00933092716633,method="walking",name="")+
  draw_map(lon=-109.04509298794375,lat=37.00933092716633,method="cycling",name="")+
  draw_map(lon=-109.04509298794375,lat=37.00933092716633,method="driving",name="")
ggsave("output/FourCornerMonument_3Maps.pdf", width=16*3, height=16)


### Monta Vista High School 37.315506149251995, -122.05625151808599
draw_map(lon=-122.05625151808599,lat=37.315506149251995,method="walking",name="")+
  draw_map(lon=-122.05625151808599,lat=37.315506149251995,method="cycling",name="")+
  draw_map(lon=-122.05625151808599,lat=37.3155061492519953,method="driving",name="")
ggsave("output/MontaVista_3Maps.pdf", width=16*3, height=16)



