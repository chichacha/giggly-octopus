
## Week 37 Fire
## Sun, Candles, Lightning, Bioluminescence, Flames, Lightbulb, Neon

library(lubridate)
library(ambient)

week37 <- tibble(
  date = seq.Date(from=ymd("2021-09-05"),to=ymd("2021-09-11"),by="day"),
  prompt = c("Sun","Candles","Lightening","Bioluminescence","Flames","Lightbulb","Neon")
) %>% mutate(wday = wday(date,label=T,abbr=F))

col_10 <- colorRampPalette(str_c("#",c("f3b700","faa300","e57c04","ff6201","f63e02")))(10)
col_5 <- str_c("#",c("264653","2a9d8f","e9c46a","f4a261","e76f51"))


radical_ray <- tibble(
  date =  seq.Date(from=ymd("2021-01-01"),to=ymd("2021-12-31"),by="day"),
  t = seq(0,2*pi,length.out=366)[-366]
) %>% mutate(x=cos(t),
             y=sin(t))

cross_ray <- expand_grid(
  r = seq(0.5,1,length.out=24),
  t = seq(2*pi,0,length.out=366)[-366]+pi/2
) %>% 
  mutate(idx=dense_rank(-t),
         idx_r =dense_rank(r)) %>%
  mutate(x=r*cos(t),
         y=r*sin(t),
         date = ymd("2020-01-01")+ddays(idx),
         wday=wday(date,label=T),
         mo = month(date)) 

cross_ray$color <- sample(col_10,size=nrow(cross_ray),replace=T)
cross_ray$size <- sample(rbeta(nrow(cross_ray),2,1),replace=T)

range(cross_ray$x)
range(cross_ray$y)

seq(0.1,0.8,length.out=16)

for (i in c(1:30)){
  
  p <- runif(n=1,min=0.2,max=0.7)
  cross_ray %>%
    slice_sample(prop=p) %>%
    ggplot(aes(x=x,y=y)) +
    annotate(geom="rect", xmin=-1.2,xmax=1.2,ymin=-1.2,ymax=1.2,fill="#1E2528", color="#00000000") +
    geom_circle(data=.%>%count(mo),aes(x=NULL,y=NULL,x0=0,y0=0,
                                       r=as.numeric(mo)/26,fill=sample(mo)),
                alpha=0.7,color="#ffffff00") +
    geom_spoke(aes(radius=size/10,angle=atan2(y,x),color=color,size=size)) +
    # geom_arc_bar(data=. %>% group_by(mo) %>% summarise(t_min=min(t),t_max=max(t)),
    #              aes(x0=0,y0=0,r0=1.2,r=1.25,
    #                  start=t_min,end=t_max,x=NULL,y=NULL,fill=as.numeric(mo)),
    #              color="#00000000",alpha=0.9) +
    theme_void() +
    coord_fixed() +
    scale_color_identity() +
    scale_size_identity() +
    scale_fill_gradientn(colors=c(col_10),guide="none") +
    theme(plot.margin=unit(c(-10,-10,-10,-10),"mm"))
  
  ggsave(str_c("output/sun/sun_p",str_pad(i,2,"left","0"),".png"), width=8,height=8)
}

?theme

imgs <- fs::dir_ls("output/sun/")

imgs_mg <-imgs %>% map(~image_read(.)) #%>%

image_join(imgs_mg,rev(imgs_mg)) %>%
  image_write_video(path="output/sunny.mp4")



library(suncalc)
library(patchwork)
#49.68502585863953, -123.15482300115931
#35.659199725109694, 139.74549044803697
#-8.418753031691082, 114.80501019016725

hr_day <-  seq.POSIXt(from=ymd_h("2021-01-01 0"),to=ymd_h("2021-12-31 23"),by="hour")
van_2021 <- getSunlightPosition(date=hr_day,lat=49.68502585863953,lon=-123.15482300115931)
tok_2021 <- getSunlightPosition(date=hr_day,lat=35.659199725109694,lon=139.74549044803697)
honolulu_2021 <-getSunlightPosition(date=hr_day,lat=21.266920715225396, lon=-157.80596475352976)
fairbank_2021 <- getSunlightPosition(date=hr_day,lat=66.72205840633771, lon=-147.76602124779265)


van_2021 %>%
  ggplot(aes(x=cos(azimuth),y=sin(azimuth))) +
  geom_spoke(aes(radius=abs(atan2(sin(altitude),cos(altitude))),angle=azimuth, 
                 color=hour(date)),size=0.1)+ 
  #geom_spoke(aes(radius=atan2(sin(azimuth),cos(azimuth)),angle=altitude, 
                 #color=month(date)),size=0.1)+ 
  coord_fixed() +
  scale_color_gradientn(guide="none",colors=c("black","white")) +
  theme_void()  +
tok_2021 %>%
  ggplot(aes(x=cos(azimuth),y=sin(azimuth))) +
  geom_spoke(aes(radius=abs(atan2(sin(altitude),cos(altitude))),angle=azimuth, 
                 color=hour(date)),size=0.1)+ 
  #geom_spoke(aes(radius=atan2(sin(azimuth),cos(azimuth)),angle=altitude, 
  #color=month(date)),size=0.1)+ 
  coord_fixed() +
  scale_color_gradientn(guide="none",colors=c("black","white")) +
  theme_void() +
honolulu_2021 %>%
  ggplot(aes(x=cos(azimuth),y=sin(azimuth))) +
  geom_spoke(aes(radius=abs(atan2(sin(altitude),cos(altitude))),angle=azimuth, 
                 color=hour(date)),size=0.1)+ 
  #geom_spoke(aes(radius=atan2(sin(azimuth),cos(azimuth)),angle=altitude, 
  #color=month(date)),size=0.1)+ 
  coord_fixed() +
  scale_color_gradientn(guide="none",colors=c("black","white")) +
  theme_void() +
fairbank_2021 %>%
  ggplot(aes(x=cos(azimuth),y=sin(azimuth))) +
  geom_spoke(aes(radius=abs(atan2(sin(altitude),cos(altitude))),angle=azimuth, 
                 color=hour(date)),size=0.1)+ 
  #geom_spoke(aes(radius=atan2(sin(azimuth),cos(azimuth)),angle=altitude, 
  #color=month(date)),size=0.1)+ 
  coord_fixed() +
  scale_color_gradientn(guide="none",colors=c("black","white")) +
  theme_void()


df <-expand.grid(
  date = seq.POSIXt(from=ymd_h("2021-01-01 0"),to=ymd_h("2021-12-31 23"),by="hour"),
  lat = c(-90,-80,-70,seq(-60,60,length.out=31),70,80,90),
  lon = c(0)
) 

df <-suncalc::getSunlightPosition(data=df) %>%
  mutate(grp=interaction(lon,lat),
         yday=yday(date),
         hr = hour(date),
         t = scales::rescale(yday,to=c(2*pi,0)+pi/2),
         r = scales::rescale(hr,to=c(0.5,1)))

df_s <- df %>% group_split(lat)
length(df_s)

range(df$altitude)
range(df$azimuth)

for (i in c(1:length(df_s))) {
  
df_s[[i]] %>%
  ggplot(aes(x=r*cos(t),y=r*sin(t))) +
  annotate(geom="rect",xmin=-(0.5*pi+1),ymin=-(0.5*pi+1),xmax=0.5*pi+1,ymax=0.5*pi+1,
           fill="#031634",color="#ffffff00") +
  geom_spoke(aes(radius=altitude,angle=azimuth,color=month(date)), size=0.1) +
  coord_fixed() +
  theme_void() +
  scale_color_gradientn(colours=c(col_5,rev(col_5)),guide="none") 

  ggsave(str_c("output/sunpos/sunpos3_",str_pad(i,width=2,side="left",pad="0"),".png"),
         width=6,height=6)
}


magma_pal <- viridis::magma(n=12, end=0.8)


for (i in c(1:30)) {
  
  df_s[[i]] %>% ggplot(aes(x=cos(t),y=sin(t))) +
    geom_spoke(aes(radius=(atan2(sin(altitude),cos(altitude))),
                   angle=azimuth, 
                   color=hour(date)),size=0.1) +
    #scale_color_gradientn(colours=(material_colors()[c(9:16,1:8)]),name="hour")+
    scale_color_gradientn(colours=c(col_5,rev(col_5)),guide="none")+
    facet_wrap(~lat) +
    coord_fixed() +
    theme_void(base_family="Roboto Condensed")+
    geom_blank(data=NULL, aes(x=c(-1),y=c(-1))) +
    geom_blank(data=NULL, aes(x=c(1),y=c(1)))
  
  ggsave(str_c("output/sunpos/sunpos_",str_pad(i,width=2,side="left",pad="0"),".pdf"),
         width=6,height=6,device=cairo_pdf)
  
}

library(magick)

imgs <- fs::dir_ls("output/sunpos/")

imgs_mg <-imgs %>% map(~image_read(.)) #%>%

image_join(imgs_mg,rev(imgs_mg)) %>%
  image_write_video(path="output/sunpos_anim2.mp4")
?image_write_video

draw_test <- function(i){
  df_s[[i]] %>%
    mutate(mo=month(date),
           x = r*cos(t),y=r*sin(t)) %>%
    ggplot(aes(x=x,y=y)) +
    annotate(geom="rect", xmin=-1.2,xmax=1.2,ymin=-1.2,ymax=1.2,fill="#1E2528", color="#00000000") +
    geom_spoke(aes(radius=(pi-altitude)/24,angle=azimuth,color=azimuth,size=hour(date)/42))  +
    coord_fixed() +
    theme_void() +
    scale_color_gradientn(colors=c(col_5,rev(col_5)),guide="none") +
    scale_size_identity() +
    scale_fill_gradientn(colors=c(col_10),guide="none") 
}

?geom_spoke

draw_test(1)
c(1:25) %>% map(draw_test)


library(ggforce)

radical_ray %>%
  ggplot(aes(x=x,y=y)) +
  geom_segment(aes(xend=0.4*cos(t),yend=0.4*sin(t))) +
  geom_line() +
  coord_fixed() +
  theme_void() 
