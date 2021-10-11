

library(sf)
library(terra)
library(dplyr)
library(spData)

# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")
p <- p %>% st_sf() %>% mutate(idx=row_number())
p

a
l
p_matrix
p_multi
st_sfc(p_multi) %>% st_cast("POINT")

ggplot() +
  geom_sf(data=a) +
  geom_sf(data=l) +
  geom_sf(data=p,size=10) +
  geom_sf_text(data=p, aes(label=idx), color="white")

st_intersects(p,a,sparse=F)


b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
b[1]
b[2]
b[3]<-st_intersection(b[1],b[2])

b[3] %>% plot()

st_union(b[1],b[2]) %>% plot()
st_sym_difference(b[1],b[2]) %>% ggplot() +
  geom_sf(fill="#F15500", color="#ffffff",size=3) +
  geom_sf(data=st_bbox(st_union(b[1],b[2])) %>% st_as_sfc(),fill="#00000000") +
  theme_void()


st_difference(b[1],b[2]) %>% ggplot() +
  geom_sf(fill="#F15500", color="#ffffff",size=3) +
  geom_sf(data=st_bbox(st_union(b[1],b[2])) %>% st_as_sfc(),fill="#00000000") +
  theme_void()

st_difference(b[2],b[1]) %>% ggplot() +
  geom_sf(fill="#F15500", color="#ffffff",size=3) +
  geom_sf(data=st_bbox(st_union(b[1],b[2])) %>% st_as_sfc(),fill="#00000000") +
  theme_void()

st_intersection(b[2],b[1]) %>% ggplot() +
  geom_sf(fill="#F15500", color="#ffffff",size=3) +
  geom_sf(data=st_bbox(st_union(b[1],b[2])) %>% st_as_sfc(),fill="#00000000") +
  theme_void()

st_union(b[2],b[1]) %>% ggplot() +
  geom_sf(fill="#F15500", color="#ffffff",size=3) +
  geom_sf(data=st_bbox(st_union(b[1],b[2])) %>% st_as_sfc(),fill="#00000000") +
  theme_void()



p <-create_complete(n=4) %>%
  create_layout("circle") %>%
  ggraph("nicely") +
  geom_edge_link0()

p + p + p + p + coord_fixed() +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(family="Roboto Condensed",size=10))

test %>%
  ggplot() +
  geom_sf()

rotate_me <- function(a_deg,geometry,x_mul=1,y_mul=1){
  
  g <- st_geometry(geometry)
  #g_cent <- st_centroid(g)
  g_cent <- st_line_sample(st_cast(g,"LINESTRING"),n=1)
  t = a_deg * pi / 180 #degrees to radians
  my_mat <-matrix(c(x_mul*cos(t), sin(t), -sin(t), y_mul*cos(t)), nrow = 2, ncol = 2)
  return(st_sfc(((g - g_cent) * my_mat)+g_cent) %>% pluck(1))
  
} 

rotate_me(10,g,2,1) %>% pluck(1)

?st_line_sample


g<- st_intersection(b[2],b[1]) %>% st_geometry()

neons <- rev(str_c("#",c("f72585","7209b7","3a0ca3","4361ee","4cc9f0")))
blues <- str_c("#",c("03045e","023e8a","0077b6","0096c7","00b4d8","48cae4","90e0ef","ade8f4","caf0f8"))

120
180+60+120/2

draw_lily <- function(openness=45,n=100){
  g100 <-rep(g,n)
  df <-tibble(idx=1:length(g100),a_deg=seq(from=180-openness,to=180+openness,length.out=n))
  df_g <- st_set_geometry(df,g100)
  
  df_g2 <- df_g %>% 
    mutate(idx2 = row_number(desc(abs(a_deg-180)))) %>%
    mutate(x_mul = scales::rescale(idx,to=c(1,1.05)),
           y_mul = scales::rescale(idx2,to=c(1,1.2)),
           a_deg = a_deg,
           color= idx%%length(neons)) %>%
    mutate(geom=pmap(list(a_deg=a_deg,geometry=geometry,x_mul,y_mul),rotate_me) )
  
  df_g3 <-st_set_geometry(df_g2 %>% st_drop_geometry(),st_as_sfc(df_g2$geom))
  
  p <-df_g3  %>%
    ggplot() +
    geom_sf(aes(fill=neons[color+1]),color="#ffffff", alpha=0.3, size=0.5) +
    theme_void() +
    scale_fill_identity() +
    coord_sf(xlim=c(-1.5,2.5),ylim=c(1.5,4.5)) +
    theme(plot.background=element_rect(fill="#2B222C",color="#ffffff00"),
          plot.margin=unit(c(-10,-10,-10,-10), "mm"))
  
  #return(p)
  print(p)
  ggsave(str_c("output/flower/flower_",format(lubridate::now(),"%M%S"),".pdf"),width=8, height=8,device=cairo_pdf)
  
}

?format
draw_lily(90,30)

draw_lily(50,30)
format(lubridate::now(),"%M_%S_%OS")

c(seq(0,85,length=25),c(87,90,95,90,85)) %>%
  map(draw_lily,n=34) 

ggsave("output/Flower_Opening.pdf",width=16, height=16, device=cairo_pdf)

library(magick)

my_files <-fs::dir_ls("output/flower")
flower_opening <-c(my_files,my_files[c(19:1,1,1,1,2,3,2,1)]) %>% map(~image_read_pdf(., density=240)) %>%
  image_join()

flower_opening %>% magick::image_write_video(path="output/flower/flower.mp4",framerate=12)
?image_write_video
