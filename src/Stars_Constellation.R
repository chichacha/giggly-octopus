### I have problem with open.connection(x,"rb")  SSL certificate problem : certificate has expired... :( and can't run below anymore... :(

library(tidyverse)
library(sf)
library(skimr)
#library(rmapshaper)
library(see)
library(ggforce)
library(rvest)
library(ggrepel)
library(TSP)

tmp <- read_html("https://www.iau.org/public/themes/constellations/") %>%
  html_table() %>%
  pluck(1)

tmp2 <- read_html("https://www.iau.org/public/themes/constellations/") %>%
  html_elements("td p+ p a") %>%
  html_attr("href")

boundary_info_txt <-(str_c("https://www.iau.org",tmp2))[str_detect(tmp2,"(txt)$")]

const88 <- tmp[2:89,]
names(const88) <- c("name_pronouciation","abbr","english_name","genitive_pronounciation","dl_info")

get_data <- function(url){
  tmp3 <- read_lines(url) %>%
    as_tibble() %>%
    separate(value,into=c("x1","x2","x3"), sep="\\|", extra="drop") %>%
    separate(x1,into=c("hour","min","sec"), sep=" ", remove=F) %>%
    mutate_at(vars(hour:x2), as.numeric) %>%
    mutate(time_of_day = hour*3600+min*60+sec) %>%
    mutate(lon = scales::rescale(time_of_day,from=c(0,24*3600),to=c(180,-180)),
           lat = x2,
           dec_d = floor(x2),
           dec_m = floor((x2-dec_d)*60),
           dec_s = (x2-dec_d-dec_m/60)*3600) %>%
    mutate(search_q=str_c(x1,"  ",dec_d," ",dec_m," ",dec_s)) %>%
    mutate(idx=row_number())
  tmp3$source=url
  
  return(tmp3)
}

iau_constellation <-boundary_info_txt %>% map_dfr(get_data)

iau_constellation <-iau_constellation %>%
  group_by(x3) %>%
  mutate(diff_lon = max(lon)-min(lon),
         cnt=n(),
         mean_lon = mean(lon),
         mean_lat = mean(lat)) %>%
  ungroup() 

iau_constellation %>% write_rds(file="data/iau_constellation.rds")


iau_constellation %>%
  group_by(x3) %>%
  mutate(diff_lon = max(lon)-min(lon),
         cnt=n(),
         mean_lon = mean(lon),
         mean_lat = mean(lat)) %>%
  ungroup() %>%
  ggplot() +
  geom_path(aes(x=lon,y=lat, group=x3, color=str_sub(x3,1L,1L)), 
            data=. %>% filter(diff_lon<270)) +
  geom_path(aes(x=lon,y=lat, group=x3, color=str_sub(x3,1L,1L)), 
            data=. %>% filter(diff_lon>=270 & lon>0)) +
  geom_path(aes(x=lon,y=lat, group=x3, color=str_sub(x3,1L,1L)), 
            data=. %>% filter(diff_lon>=270 & lon<0)) +
  geom_text(aes(x=mean_lon,y=mean_lat, label=x3), 
            data=. %>% filter(idx==1 & diff_lon<270),
            family="Roboto Condensed")+ 
  geom_text(aes(x=lon, y=lat, label=x3), 
            data = . %>% filter(diff_lon>=270 & idx==1),
            family="Roboto Condensed") +
  coord_map(xlim=c(-180,180),ylim=c(-90,90)) +
  scale_color_material(guide="none") +
  theme_void() +
  scale_x_continuous() 





#d = int(15.7364635°) = 15°
#m = int((15.7364635° - 15°) × 60) = 44'
#s = (15.7364635° - 15° - 44'/60) × 3600 = 11.27"
#15.7364635°
#= 15° 44' 11.27"
# library(pdftools)
# library(patchwork)
# #test <- pdf_data("https://www.iau.org/static/public/constellations/pdf/ORI.pdf")
# 
# test[[1]] %>%
#   ggplot(aes(x=x,y=y)) +
#   geom_text(aes(label=text)) +
# tmp3 %>%
#   ggplot(aes(x=lon,y=lat)) +
#   geom_text(aes(label=idx)) +
#   coord_map()


urls <- read_html("https://en.wikipedia.org/wiki/Lists_of_stars_by_constellation") %>%
  html_elements("#t-info , .multicol a") %>%
  html_attr("href") %>%
  str_c("https://en.wikipedia.org",.)

urls <- urls[1:88]

urls[14] %>% read_html()

get_stars <- function(url){
  url <- urls[14]
  a_table <-url %>%
    read_html() %>%
    html_table() %>%
    pluck(1) %>%
    janitor::clean_names()
  
  df <- a_table %>%
    select(name,b,hd,hip,ra,dec,vis_mag,abs_mag,dist_ly,sp_class,notes) %>%
    filter(str_length(vis_mag)>0) %>%
    mutate(vis_mag=str_replace(vis_mag,"−","-"), abs_mag = str_replace(abs_mag,"−","-")) %>%
    mutate_at(vars(vis_mag,abs_mag), ~suppressWarnings(as.double(.))) %>%
    mutate(idx=row_number()) %>% 
    mutate(ra_deg = str_remove_all(ra,"(h|m|s)"),
           dec_deg = str_remove_all(dec,"(°|″|′)")) %>%
    separate(ra_deg,into=c("ra_h","ra_m","ra_s"),extra="merge") %>%
    separate(dec_deg,into=c("dec_sign","dec_h","dec_m","dec_s"),extra="merge") %>%
    mutate(dec_sign=if_else(str_detect(dec,"−"),-1,1)) %>%
    mutate_at(vars(ra_h:dec_s),~suppressWarnings(as.double(.))) %>%
    mutate(lon = scales::rescale((ra_h*3600 + ra_m*60 + ra_s),from=c(0,24*3600),to=c(180,-180)),
           lat = (dec_h + dec_m/60 + dec_s/3600)*dec_sign)
  
  #df <- df %>% filter(!is.na(vis_mag))
  df$hd <- as.character(df$hd)
  df$hip <- as.character(df$hip)
  df$dist_ly <- as.character(df$dist_ly)
  
  df$source <- url
  return(df)
  
}


df <- get_stars(urls[str_detect(urls,"Canis_M")]) 

stars_df <- urls[1:52] %>%
  map_dfr(get_stars)

stars_df2 <- urls[56:88]%>%
  map_dfr(get_stars)

stars_df3 <- urls[53:55] %>%
  map_dfr(get_stars)

stars_df_comb <- bind_rows(stars_df,stars_df2,stars_df3)
stars_df_comb %>% write_rds(file="data/stars_in_constellation.rds")


stars_df <- read_rds("data/stars_in_constellation.rds")

stars_df$source[1]

stars_df <- stars_df %>%
  mutate(const_name=str_remove(source,"https://en.wikipedia.org/wiki/List_of_stars_in_")) %>%
  group_by(const_name) %>%
  mutate(visible_stars=sum(vis_mag<6),
         bright_stars=sum(vis_mag<3),
         all_stars=n(),
         min_ra_h = min(ra_h),
         max_ra_h = max(ra_h),
         diff_ra_h=max_ra_h - min_ra_h) %>%
  ungroup() %>%
  mutate(dist_ly=as.numeric(dist_ly))

stars_df %>% count(min_ra_h,max_ra_h,diff_ra_h) %>%
  arrange(-diff_ra_h)

stars_df %>% ggplot(aes(x=vis_mag)) +
  stat_density()

range(stars_df$vis_mag)

my_grids <- list(lon=seq(-180,180,by=10),lat=c(-90,90,by=10)) %>%
  cross_df()


stars_df %>% 
  ggplot(aes(x=lon,y=lat)) +
  # geom_delaunay_segment(data=. %>% filter(vis_mag<5 & diff_ra_h<23) %>%
  #                         add_count(const_name) %>% filter(n>2),
  #                       color="white",
  #                       size=0.1, aes(group=source)) +
  # geom_path(data=iau_constellation %>% filter(diff_lon<180),
  #           aes(x=lon,y=lat, group=x3), color="white") +
  geom_point(aes(size=vis_mag), color="#E7D84B", alpha=0.6,
             data = . %>% filter(vis_mag<6), shape="triangle") +
  geom_point(color="#EFEAC5", aes(size=vis_mag,alpha=abs_mag),
             data = . %>% filter(vis_mag<6), shape=8) +
  geom_point(color="#EFEAC5",
             data = . %>% filter(vis_mag>=6), shape=18, size=0.05) +
  # geom_text(aes(label=str_sub(b,1L,1L)), color="#B5AC01", nudge_y=-0.5,
  #           data=. %>% filter(vis_mag<3),vjust=1) +
  scale_size_continuous(range=c(20,0.1), trans="log", guide="none",
                        limits=c(0.0001,6)) +
  scale_alpha_continuous(range=c(0.7,0.3),guide="none") +
  #coord_map(xlim=range(stars_df$lon,na.rm=T), ylim=range(stars_df$lat,na.rm=T)) +
  coord_map(xlim=c(-180,180), ylim=c(-90,90), projection="lambert",lat0=90,lat1=-90) +
  theme_void(base_family="Roboto Condensed") +
  theme(plot.background=element_rect(fill="grey8"),
        text = element_text(family="Roboto Condensed", color="white")) +
  scale_color_material(guide="none") 

ggsave("output/gilbert_proj_stars.pdf", width=16, height=16, device=cairo_pdf)

mat_col <- material_colors()[c(1:16)]
my_greys <- grey.colors(n=9, start=0.3, end=0.9)
my_greys19 <- c(my_greys,"#ffffff",rev(my_greys))
my_greys %>% scales::show_col()

seq(-90,90,by=10) %>% length()



projs_no_param <- c("mercator", "sinusoidal", "cylindrical", "mollweide", "gilbert", "azequidistant", "azequalarea", "gnomonic", "orthographic", "stereographic", "laue", "polyconic", "aitoff", "lagrange", "globular", "vandergrinten", "guyou", "square", "tetra", "hex")

library(patchwork)

draw_me <- function(my_proj){
  stars_df %>%
    ungroup() %>%
    arrange(vis_mag) %>%
    ggplot(aes(x=lon,y=lat)) +
    geom_delaunay_segment(data=. %>% slice_head(n=50), color="white") +
    geom_vline(data=NULL, xintercept=seq(-180,180,by=10), 
               color=colorRampPalette(colors=mat_col)(37),
               size=0.1) +
    geom_hline(data=NULL, yintercept=seq(-90,90,by=10), 
               color=my_greys19, linetype=c(rep(3,times=9),1,rep(9,times=9)),
               size=0.05) +
    geom_text(aes(label=str_c("  ",name)), data= . %>% slice_head(n=10), 
              family="Roboto Condensed",
              color="white", hjust=0) +
    geom_point(color="#EFEAC5", aes(size=vis_mag),
               data = . %>% filter(vis_mag<3), shape=8) +
    geom_point(color="#EFEAC5", aes(size=vis_mag,alpha=abs_mag),
               data = . %>% filter(vis_mag<6), shape=16) +
    coord_map(xlim=c(-180,180), ylim=c(-90,90), projection=my_proj) +
    scale_size_continuous(range=c(20,0.1), trans="log", guide="none",
                          limits=c(0.0001,6)) +
    scale_alpha_continuous(range=c(0.7,0.3),guide="none") +
    theme_void(base_family="Roboto Condensed", base_size=16) +
    theme(plot.background=element_rect(fill="grey8"),
          text = element_text(family="Roboto Condensed", color="white")) +
    labs(title=str_to_title(my_proj))
}

projs_no_param

draw_me(projs_no_param[[4]])



projs_no_param %>%
  map(draw_me) %>%
  wrap_plots(widths=1)

ggsave("output/stars_projection_no_param.png", width=16*0.7, height=12*0.7)
