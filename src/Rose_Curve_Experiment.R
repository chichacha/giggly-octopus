

library(tidyverse)
library(ambient)



rose_df <- function(k){
  #k <- 3
  rose <- tibble(
    t = seq(0,6*pi,length.out=1000),
    idx = c(1:1000)
  ) %>% mutate(r = 0.45*cos(t*k)) %>%
    mutate(x = r*cos(t),
           y = r*sin(t))
  return(rose)
}

c(c(1:10)+1, 1/c(2:11), sqrt(c(2:81)))

9*16
my_grid <- long_grid(x=c(1:9),y=c(1:9))  %>%
  mutate(k = (c(2:82)),
         grp = c(2:82)) %>%
  rename(x0=x,y0=y) %>%
  mutate(df = map(k,rose_df)) %>%
  unnest(df)

my_rose_grid <- my_grid %>%
  mutate(x=x0+x,y=y0+y)

col_pal10 <- str_c("#",c("d9ed92", "b5e48c", "99d98c", "76c893", "52b69a", "34a0a4", "168aad", "1a759f", "1e6091", "184e77"))

my_rose_grid %>%
  ggplot() +
  geom_path(aes(x=x,y=y,group=grp,color=factor((x0*y0)%%10)), size=0.3) +
  geom_text(aes(label=str_c(grp), x=x0,y=y0), family="Roboto Condensed", data=. %>% count(x0,y0,grp,k)) +
  coord_fixed() +
  theme_void() +
  scale_color_manual(values=col_pal10, guide="none")

ggsave("output/Rose_Curve2.pdf", width=16, height=16, device=cairo_pdf)

my_rose_grid %>%
  mutate(y=ifelse(x0%%2==0,y+0,y+0.5),
         y0=ifelse(x0%%2==0,y0,y0+0.5)) %>%
  ggplot() +
  geom_polygon(aes(x=x,y=y,group=grp,color=atan2(y0,x0))) +
  geom_text(aes(label=str_c(grp), x=x0,y=y0), family="Roboto Condensed", data=. %>% count(x0,y0,grp,k)) +
  coord_fixed() +
  theme_void() +
  scale_color_gradientn(colors=col_pal10, guide="none")


