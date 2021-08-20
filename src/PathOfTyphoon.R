library(rvest)
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)

col_pal <- str_c("#", str_split("006466-065a60-0b525b-144552-1b3a4b-212f45-272640-312244-3e1f47-4d194d","-")%>% unlist())

## Name of Typhoon - 140 names and recycles 
#https://www.jma.go.jp/jma/kishou/know/typhoon/1-5.html
#https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/tyname.html

typhoon_names <- read_html("https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/tyname.html") %>% html_table()

ty_japanese <- read_html("https://www.jma.go.jp/jma/kishou/know/typhoon/1-5.html") %>%
  html_table() %>% pluck(1)

names(ty_japanese) <-c("idx","country_name_jp","typhoon_name_en","typhoon_name_jp","meaning_jp")

typhoon_name_country <- typhoon_names[[1]] %>%
  clean_names() %>%
  pivot_longer(-contributed_by) %>%
  select(country=contributed_by,typhoon_name_en=value)


typhoon_name_meaning <- typhoon_names[2:6] %>% map_df(bind_rows) %>%
  mutate(typhoon_name_en=coalesce(`column I`,`column II`,`column III`,`column IV`,`column V`)) %>%
  select(typhoon_name_en, meaning_en=Meaning)

ty_master <- ty_japanese %>%
  inner_join(typhoon_name_country) %>%
  inner_join(typhoon_name_meaning)

ty_master %>% 
  count(country_name_jp, country)


### Get typhoon Location Tracking Data

## Column Names 
## Data format is specified here: http://www.data.jma.go.jp/fcd/yoho/typhoon/position_table/format_csv.html

column_titles <- c("yr", "mo", "day", "hr", "t_id", "t_name", "t_class", "lat", "long", "hpa", "knot", "kt50_dir", "kt50_major", "kt50_minor", "kt30_dir", "kt30_major", "kt30_minor", "landing")

typhoon <- tibble(
  prefix = "https://www.data.jma.go.jp/fcd/yoho/typhoon/position_table/table",
  year = c(2001:2020),
  suffix = ".csv"
) %>% mutate(url = str_c(prefix,year,suffix)) %>%
  select(url) %>%
  mutate(data = map(url,read_csv, 
                    skip=1, 
                    col_names=column_titles,
                    col_types = "ddddccdddddddddddd"))

typhoon_data <-typhoon %>% unnest(data) %>% as_tibble() %>%
  mutate(datetime=lubridate::ymd_h(str_c(yr,mo,day,hr,sep="-")),
         date = as.Date(datetime),
         wday = wday(datetime, label=T),
         yday = yday(datetime),
         mo = month(datetime,label=T))

typhoon_data_sf <- typhoon_data %>%
  st_as_sf(coords=c("long","lat"))

st_crs(typhoon_data_sf) <- 4326
typhoon_data_sf_summary  <-typhoon_data_sf %>%
  group_by(t_id,t_name) %>%
  summarise(date_range = min(datetime)%--%max(datetime),
            dur = as.duration(date_range),
            start_time=min(datetime),
            end_time=max(datetime),
            hpa_ave = mean(hpa),
            dt_points = n(),
            t_class_cnt = n_distinct(t_class),
            t_class_str = paste(t_class,collapse="-"),
            geometry = st_combine(geometry) %>% st_cast("LINESTRING"))  %>%
  ungroup() %>%
  mutate(line_length=st_length(geometry)) %>%
  mutate(year = str_c("20",str_sub(t_id,1L,2L)),
         num = str_sub(t_id,3L,4L)) 

typhoon_data_sf_summary %>%
  mutate(geometry=st_convex_hull(geometry)) %>%
  ggplot() +
  geom_sf(aes(#color=as.numeric(line_length/1000),
              fill=as.numeric(dur), 
              alpha=hpa_ave), color="#ffffff00") +
  geom_text(aes(x=-Inf,y=-Inf,label=n), 
            data=typhoon_data_sf_summary %>% st_drop_geometry() %>%
              count(year, sort=T), hjust=0, vjust=0, family="Roboto Condensed") +
  scale_color_material_c() +
  scale_fill_material_c() +
  scale_alpha_continuous(range=c(0.5,0.9), guide="none")+
  facet_wrap(~year) +
  scale_size_continuous(range=c(0.1,2), guide="none") +
  theme_graph(base_family="Roboto Condensed") 

ggsave("output/typhoon_convexhull.pdf", width=16, height=9, device=cairo_pdf)


typhoon_data %>%
  ggplot(aes(x=long,y=lat)) +
  geom_path(aes(group=t_id)) +
  geom_point(aes(size=hpa,color=factor(t_class), alpha=landing)) +
  scale_color_material() +
  scale_size_continuous(range=c(0.1,1)) +
  facet_wrap(~yr) +
  coord_map() +
  theme_graph()

world <- st_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
japan <- world %>% filter(id=="JPN")

  

typhoon_data_sf_summary %>%
  filter(year==2020) %>%
  ggplot() +
  geom_sf(aes(#color=as.numeric(line_length/1000),
    color=as.numeric(dur), 
    alpha=hpa_ave)) +
  geom_sf(data=japan, fill="#000000", color="#ffffff00") +
  # geom_text(aes(x=-Inf,y=-Inf,label=n), 
  #           data=typhoon_data_sf_summary %>% st_drop_geometry() %>%
  #             count(year, sort=T), hjust=0, vjust=0, family="Roboto Condensed") +
  scale_color_material_c(guide="none") +
  scale_fill_material_c(guide="none") +
  scale_alpha_continuous(range=c(0.5,0.9), guide="none")+
  facet_wrap(~year) +
  scale_size_continuous(range=c(0.1,2), guide="none") +
  theme_graph(base_family="Roboto Condensed") 
ggsave("output/typhoon_path_2020.pdf", width=16, height=16, device=cairo_pdf)

typhoon_data 


start_point <-typhoon_data_sf %>%
  group_by(t_id) %>%
  mutate(idx_within = row_number()) %>%
  filter(idx_within==1) %>%
  st_as_sf(coords=c("long","lat")) %>%
  mutate(year = str_c("20",str_sub(t_id,1L,2L)),
         num = str_sub(t_id,3L,4L)) 

?st_segmentize
typhoon_data_sf_summary %>%
  mutate(geometry=st_segmentize(geometry,0)) %>%
  ggplot() +
  geom_sf(aes(#color=as.numeric(line_length/1000),
    color=as.numeric(dur), 
    alpha=hpa_ave)) +
  geom_text(aes(x=-Inf,y=-Inf,label=n), 
            data=typhoon_data_sf_summary %>% st_drop_geometry() %>%
              count(year, sort=T), hjust=0, vjust=0, family="Roboto Condensed") +
  scale_color_material_c(guide="none") +
  scale_fill_material_c(guide="none") +
  scale_alpha_continuous(range=c(0.5,0.9), guide="none")+
  facet_wrap(~year) +
  scale_size_continuous(range=c(0.1,2), guide="none") +
  theme_graph(base_family="Roboto Condensed") 

ggsave("output/typhoon_same_start.pdf", width=16, height=16, device=cairo_pdf)



typhoon_data_sf_summary %>% st_drop_geometry() %>%
  count(year, sort=T) %>%
  DT::datatable()

typhoon_data %>%
  ggplot(aes(x=long, y=lat)) +
  geom_path(aes(group=t_id,color=line_length/1000)) +
  scale_color_gradientn(colours=col_pal)


