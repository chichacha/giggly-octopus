

library(suncalc)
library(lubridate)
library(hms)


OlsonNames()

my_dates <-seq.Date(from=ymd("2021-01-01"),to=ymd("2021-12-31"), by="day")

##64.85999622997987, -147.84202714086632
#127.5° West and 112.5° West

alaska <- list(lat=64.85999622997987,lon=-147.84202714086632,tz="US/Alaska")
van <- list(lat=49.246292, lon=-123.116226,tz="America/Vancouver")
tofino <- list(lat=49.139666108,long=-125.88832978,tz="America/Vancouver")
tokyo <- list(lat=35.66141195957599,lon=139.74523630873813,tz="Asia/Tokyo")
bali <- list(lat=-8.739184, lon=115.171127,tz="Singapore")
squamish <- list(lat=49.73735317519481,lon=-123.09437663970874,tz="America/Vancouver")
fort_st_james <- list(lat=54.443685289426924, lon=-124.25437226062981,tz="America/Vancouver")

lat <-49
lon <--123
tz <- "US/Pacific"

draw_daylight <- function(lat,lon,tz="America/Vancouver"){
  
  lat <- round(lat,1)
  lon <- round(lon,1)
  
  my_dates <-seq.Date(from=ymd("2021-01-01"),to=ymd("2021-12-31"), by="day")
  home <-suncalc::getSunlightTimes(my_dates,lat=lat,lon=lon,tz=tz) %>%
    mutate_if(is.POSIXct,list(dt=as_date,time=as_hms)) %>%
    mutate(daylight=difftime(sunset,sunrise)) %>%
    mutate(daylight_rank1=row_number(daylight),
           daylight_rank2=row_number(desc(daylight)),
           dir_change=(as.numeric(daylight)-lead(as.numeric(daylight))))
  
  #home %>% filter(daylight_rank1==1) %>% select(date,daylight) %>% print()
  #home %>% filter(daylight_rank2==1) %>% select(date,daylight) %>% print()
  
  anno <- home %>% filter(date %in% ymd(c("2021-12-21","2021-03-20","2021-06-20","2021-09-22")))
  
  descr = str_c(
                round(as.numeric(anno$daylight[4]),1)," hours of daylight on Winter Solstice\n  ",
                round(as.numeric(anno$daylight[2]),1)," hours of daylight on Summer Solstice\n")
  
  
  p <-home %>% ggplot(aes(x=sunrise_dt,y=sunrise_time)) +
    geom_segment(aes(xend=sunset_dt,yend=sunset_time),color="#F2C94E") +
    geom_point(aes(x=solarNoon_dt,y=solarNoon_time),shape=1,color="#F24E4E") +
    geom_blank(aes(y=hm("00:00")))+
    geom_blank(aes(y=hm("24:00")))+
    geom_text(data=anno,aes(x=sunset_dt,y=sunset_time,label=format(sunset,"%R")),
              family="Roboto Condensed", color="white", hjust="outward", vjust="outward") +
    geom_text(data=anno,aes(x=sunrise_dt,y=sunrise_time,label=format(sunrise,"%R")),
              family="Roboto Condensed", color="white", hjust="inward", vjust="inward") +
    theme_void(base_family="Roboto Condensed") +
    #scale_y_time(breaks=hm(str_c(str_pad(c(0,6,12,18,24),width=2,side="left",pad="0"),":00"))) +
    scale_y_time(breaks=NULL)+
    scale_x_date(breaks=ymd(c("2021-12-21","2021-03-20","2021-06-20","2021-09-22")),label=date_format("%b\n%e\n%a")) +
    coord_polar(start=-(355/365)*2*pi) +
    expand_limits(ylim=hm(c("00:00","24:00"))) +
    theme(plot.background=element_rect(fill="#041122", color="#00000000"),
          axis.text=element_text(family="Roboto Condensed",color="white"),
          panel.grid.major=element_line(colour="white",linetype=3,size=0.1),
          text =element_text(family="Roboto Condensed", colour="white")) +
    labs(title=str_c("  2021: Daylight at Latitude: ",lat, " Longitude: ",lon," Timezone: ",tz),
         subtitle=str_c("  ",descr))
  
  print(p)
  ggsave(str_c("output/daylight/",str_pad(lat,width=2,"left",pad="0"),"_",lon,".pdf"),width=8, height=8, device=cairo_pdf)
  
}




params <- tibble(lat=seq(0,64,by=2),lon=-123,tz="US/Pacific")

params %>% pmap(draw_daylight)

my_imgs <- fs::dir_ls("output/daylight/")
my_mgk <-my_imgs %>% map(image_read_pdf,density=160) %>%
  image_join()
my_mgk %>% image_write_video(path="output/daylight.mp4", framerate=5)
?image_write_video


draw_daylight(tokyo$lat, tokyo$lon,tokyo$tz)
draw_daylight(van$lat, van$lon,van$tz)



