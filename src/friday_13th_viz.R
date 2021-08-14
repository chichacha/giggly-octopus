
library(tidygraph)
library(ggraph)
library(tidyverse)
library(ggforce)
library(ambient)
library(see)
library(particles)
library(scales)
library(sf)
library(lubridate)

## My Colour Palette
col <- str_c("#",unlist(str_split("f94144-f3722c-f8961e-f9844a-f9c74f-90be6d-43aa8b-4d908e-577590-277da1","-")))
col %>% show_col()


## 73049 days between Jan 1 1900 - 2100 12-31 

df <- tibble(
  x = seq.Date(from=lubridate::ymd("1900-01-01"),lubridate::ymd("2300-01-01"), by="day")
) %>% mutate(wday=wday(x,label=T),
             yr = year(x),
             day = day(x),
             yday = yday(x),
             mo = month(x,label=T),
             mo_num=month(x),
             leap = leap_year(x),
             first_wday = wday(floor_date(x,"year"),label=T)) %>%
  filter(wday=="Fri" & day==13) %>%
  add_count(yr,name="occ") %>%
  mutate(yr_type = str_c(as.numeric(first_wday),".",first_wday, " - ",ifelse(leap,"Leap Year","Not Leap Year"))) %>%
  mutate(time_next = as.numeric(lead(x)-x),
         since_last = as.numeric(x - lag(x)),
         time_next_norm = rescale(time_next, from=c(0,500),to=c(0,0.5)),
         since_last_norm = rescale(since_last,from=c(0,500),to=c(0,0.5)))

unique(df$yr_type)
col14 <-colorRampPalette(col)(14)

df %>%
  ggplot(aes(x=mo_num,y=yr)) +
  geom_tile(aes(fill=yr_type, height=leap+3,width=sqrt(1/occ)*0.95)) +
  geom_segment(aes(x=mo_num-since_last_norm,
                   xend =mo_num+time_next_norm, yend=yr ),color="#ffffff") +
  geom_point(aes(x=mo_num+time_next_norm),shape=18, color="#ffffffae") +
  geom_point(aes(x=mo_num-since_last_norm),shape=15,color="#ffffff80") +
  scale_fill_manual(values=col14, name="Starting Day of Year & Year Type") +
  theme_blackboard(base_family="Roboto Condensed", base_size=18) +
  scale_y_reverse(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=c(1:12), labels=month.abb) +
  labs(x="Month when 13th of Friday happens",y="Year (1900 - 2399)",
       title="Friday the 13th Occurence",
       subtitle="688 13th of Fridays in 400 years between 1900 - 2300")

ggsave("output/Friday13th_Chart.pdf", width=16, height=16, device=cairo_pdf)

df %>%
  ggplot(aes(x=mo_num,y=yr)) +
  geom_tile(aes(fill=yr_type, height=leap+3,width=sqrt(1/occ)*0.95)) +
  geom_segment(aes(x=mo_num-since_last_norm,
                   xend =mo_num+time_next_norm, yend=yr ),color="#ffffff") +
  geom_point(aes(x=mo_num+time_next_norm),shape=18, color="#ffffffae") +
  geom_point(aes(x=mo_num-since_last_norm),shape=15,color="#ffffff80") +
  scale_fill_manual(values=col14, name="Starting Day of Year & Year Type", guide="none") +
  scale_y_reverse(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=c(1:12), labels=month.abb) +
  theme_void(base_family="Roboto Condensed", base_size=18)

ggsave("output/Friday13th_Pattern.pdf", width=16, height=16, device=cairo_pdf)

df %>% count(mo)
df %>% summarise(n_distinct(yr))
2399-1900

