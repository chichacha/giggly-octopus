library(tidyverse)
library(sf)
library(osmdata)


#my_pt <- c(49.227148701452705, -123.00004431588768)  ## lat, lon order 

draw_isochrone <- function(lon,lat,name,...){
  
  my_pt <- c(lat,lon)
  
  col_pal10 <- str_c("#",c("d9ed92", "b5e48c", "99d98c", "76c893", "52b69a", "34a0a4", "168aad", "1a759f", "1e6091", "184e77")
  )
  url <- (str_glue("https://api.mapbox.com/isochrone/v1/mapbox/walking/{lon}%2C{lat}?contours_minutes=5%2C10%2C15%2C20&polygons=false&denoise=1&access_token={Sys.getenv('MAPBOX')}"))
  url2 <- (str_glue("https://api.mapbox.com/isochrone/v1/mapbox/walking/{lon}%2C{lat}?contours_minutes=25%2C30%2C45%2C60&polygons=false&denoise=1&access_token={Sys.getenv('MAPBOX')}"))
  
  test <-st_read(url)
  test2 <- st_read(url2)
  
  iso_sf <- bind_rows(test,test2)
  iso_sf <-iso_sf %>% arrange(desc(contour)) %>%
    mutate(geometry=st_cast(geometry,"POLYGON"))
  
  st_bbox(iso_sf)
  
  res <- opq(bbox=st_bbox(iso_sf)) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()
  
  bb <- iso_sf$geometry[1]
  res_tr <- res %>% trim_osmdata(bb)
  
  ## I'm not sure but below make it work without error for now
  st_crs(res_tr$osm_lines) <- 4326
  #st_crs(iso_sf)
  st_crs(res_tr$osm_lines) <- 4326
  
  road_network <- res_tr$osm_lines %>%
    summarise(geometry=st_combine(geometry))
  
  road_network %>%
    ggplot() +
    geom_sf(data=iso_sf, aes(fill=contour), color="#ffffff00") +
    geom_sf(color="#ffffffde") +
    scale_fill_gradientn(colors=colorRampPalette(col_pal10)(10), breaks=c(5,10,15,20,25,30,45,60),
                         guide="none") +
    theme_void(base_family="Roboto Condensed") +
    labs(title=name, subtitle=str_glue("Latitude: {lat},Longitude: {lon}"))
  
  ggsave(str_glue("output/Isochrone_Station_",name,".pdf"), width=24, height=24, device=cairo_pdf )
  
}

test_station
draw_isochrone(lon=test_station$lon[6], lat=test_station$lat[6], name="Shibuya")  
draw_isochrone(lon=test_station$lon[5], lat=test_station$lat[5], name="Yokohama")  
draw_isochrone(lon=test_station$lon[4], lat=test_station$lat[4], name="Shinjuku")
draw_isochrone(lon=test_station$lon[1], lat=test_station$lat[1], name="Roppongi")  
draw_isochrone(lon=test_station$lon[2], lat=test_station$lat[2], name="Harajuku")  
draw_isochrone(lon=test_station$lon[3], lat=test_station$lat[3], name="Osaki")  
draw_isochrone(lon=test_station$lon[7], lat=test_station$lat[7], name="Machida") 
draw_isochrone(lon=test_station$lon[8], lat=test_station$lat[8], name="Fujisawa")   
draw_isochrone(lon=test_station$lon[9], lat=test_station$lat[9], name="Tsujido") 
draw_isochrone(lon=test_station$lon[10], lat=test_station$lat[10], name="Ginza")

## UC Berkely 37.87330969382552, -122.25853565647158
draw_isochrone(lon=-122.25853565647158, lat=37.87330969382552, name="UCBerkeley")

# 49.27285202149993, -123.13556588717736
draw_isochrone(lon=-123.13556588717736, lat=49.27285202149993, name="GranvilleIsland_Vancouver")

draw_isochrone(lon=4.8850607959045815, lat=52.36074034644443, name="Rijksmuseum")


#35.64375444164281, 139.6713317716351
draw_isochrone(lon=139.6713317716351,lat=35.64375444164281,name="Sancha")

#-8.693911122029663, 115.45101302307923
draw_isochrone(lon=115.45101302307923,lat=-8.693911122029663,name="YellowBridge_NusaLembongan")

#-8.419467256957278, 114.8053305406069
draw_isochrone(lon=114.8053305406069,lat=-8.419467256957278,name="Medewi_Bali")

#52.24995201635239, 21.012226181851783
draw_isochrone(lon=21.012226181851783,lat=52.24995201635239,name="Oldtown_Square_Warsaw")

#-12.046360518304697, -77.04275452876965 - Lima Peru (Plaza Dos de Mayo)
draw_isochrone(lon=-77.04275452876965,lat=-12.046360518304697,name="Lima_Peru_Plaza_Dos_de_Mayo")

#48.86078721853682, 2.337569310709589 Paris France Louvre Museum
draw_isochrone(lon=2.337569310709589,lat=48.86078721853682,name="Paris_France_Louvre_Museum")

#51.50858443139137, -0.08775151979390969 London Bridge
draw_isochrone(lon=-0.08775151979390969,lat=51.50858443139137,name="London Bridge")

#49.28320820636919, -123.12040329343608 Vancouver Art Gallery
draw_isochrone(lon=-123.12040329343608,lat=49.28320820636919,name="Vancouver_Art_Gallery")

#40.75864109884177, -73.98545676846578 Times Square
draw_isochrone(lon=-73.98545676846578,lat=40.75864109884177,name="TimesSquare_NYC")

## 32.74369717856008, 129.87296303348916 Dejima
draw_isochrone(lon=129.87296303348916,lat=32.74369717856008,name="Dejima_Nagasaki")


## Shibuya Hachico 35.65994705262562, 139.70075804749467
draw_isochrone(lon=139.70075804749467,lat=35.65994705262562,name="Shibuya_Hachiko")


## Cathedral of Barcelona 41.38431757944033, 2.1762671610183353
draw_isochrone(lon=2.1762671610183353,lat=41.38431757944033,name="Cathedral of Barcelona")


## Phoneix Arizona  33.44717305639869, -112.0738204674498
draw_isochrone(lon=-112.0738204674498,lat=33.44717305639869,name="Phoenix Arizona")

## Trevi Mountain 41.902411410673665, 12.483012061986356
draw_isochrone(lon=12.483012061986356,lat=41.902411410673665,name="TreviFountain_Rome")


## United States Capital #38.892215583167534, -77.0091068460909
draw_isochrone(lon=-77.0091068460909,lat=38.892215583167534,name="WashingtonDC_UnitedStates_Capital")

## Machu Pichu Peru -13.16279385097069, -72.5449101096784
draw_isochrone(lon=-72.5449101096784,lat=-13.16279385097069,name="MachuPichu_Peru")

## Tokyo Sky Tree 35.710258672176394, 139.81067894721477
draw_isochrone(lon=139.81067894721477,lat=35.710258672176394,name="Tokyo Sky Tree")


## 35.68583182448559, 139.75279246778513 Tokyo Imperal Palace
draw_isochrone(lon=139.75279246778513,lat=35.68583182448559,name="Imperial Palace Tokyo")


#35.689873128333396, 139.70055829378822 Shinjuku Station
draw_isochrone(lon=139.70055829378822,lat=35.689873128333396,name="Shinjuku Station")

## 52.38000277163783, 4.900347968533782  Amsterdam Centraal
draw_isochrone(lon=4.900347968533782,lat=52.38000277163783,name="Amsterdam Centraal")

# 21.017778, -101.256667 Guanajuato City
draw_isochrone(lon=-101.256667,lat=21.017778,name="Guranajuato Mexico")

# Portland City Hall 45.51510206172554, -122.67899116495603
draw_isochrone(lon=-122.67899116495603,lat=45.51510206172554,name="Portland City Hall Oregon")

## Ginza 35.67122550215823, 139.76504926209174 Harumi-Dori & Ginza Dori
draw_isochrone(lon=139.76504926209174,lat=35.67122550215823,name="Ginza_Tokyo")


## Cairo Egypt  30.044471681026295, 31.23570490926764
draw_isochrone(lon=31.23570490926764,lat=30.044471681026295,name="Cairo_Egypt")

## Savanna Georgia 32.06792791027225, -81.09656021185957
draw_isochrone(lon=-81.09656021185957,lat=32.06792791027225,name="Forsyth Park_Savanna_GA")

## Apple Park Cupertino 37.33461896680715, -122.01159107183051
draw_isochrone(lon=-122.01159107183051,lat=37.33461896680715,name="Apple Park Cupertino")




