
library(tidyverse)
library(ggforce)
library(scales)


col_5 <- str_c("#",c("264653","2a9d8f","e9c46a","f4a261","e76f51"))  ## Green & Red
col_10 <- colorRampPalette(str_c("#",c("f3b700","faa300","e57c04","ff6201","f63e02")))(10)  ## Fire Colour Pal

make_tear <- function(m=1,n=100,w=1,start=0,end=2*pi,a=0.5) {
  
  teardrop <- tibble(
    m = m, ## larger m makes bottom heavy
    w = w, ## if you use value < 1 then makes tear skinny and tall. 
    t = seq(start,end,length=n),
  ) %>% mutate(
    x0 = w * sin(t) * sin(a*t)^(m),
    y0 = cos(t)
  ) 
  
  return(teardrop)
}

make_tear(n=1000,start=0,end=2*pi,a=pi,m=5,w=1.5) %>%
  ggplot() +
  geom_path(aes(x=x0,y=y0)) +
  coord_fixed()

my_col <-str_c("#",c("d9ed92","b5e48c","99d98c","76c893","52b69a","34a0a4","168aad","1a759f","1e6091","184e77"))

params <- tibble(
  m = seq(0,9,length=9),
  n = 100,
  w = seq(1,3,length=9)
) %>% expand.grid() %>%
  #filter(dense_rank(w)%%2==0) %>%
  mutate(start=0,end=2*pi) %>%
  pmap_dfr(make_tear) %>% arrange(-w) %>%
  group_split(m)




length(params)

?geom_tile

for(i in c(1:length(params))){
  
  #x_adj <- scales::rescale(max(params[[i]]$m),from=c(0,9),to=c(-17,18))
  
  params[[i]] %>%
    ggplot(aes(x=x0,y=sqrt(m+1)*((y0*w)+w))) +
    annotate(geom="rect",xmin=-20,xmax=20,ymax=20,ymin=-20, fill="#ffffff00")+
    #annotate(geom="rect",xmin=-15,xmax=15,ymax=15,ymin=-15, fill="#1D0B38")+
    geom_shape(aes(group=rev(interaction(w,m)), fill=w), color="#ffffff00") +
    #geom_segment(data=NULL, aes(x=0,y=-1,xend=0,yend=1), color=my_col[10]) +
    scale_color_gradientn(colors=col_10,guide="none") +
    scale_fill_gradientn(colors=col_10, guide="none") +
    theme_void(base_family="Roboto Condensed") +
    coord_fixed(xlim=c(-7.5,7.5),ylim=c(-1,14)) +
    annotate(geom="tile",x=0,y=-0.5,width=0.2,height=1,color="#ffffff00",fill="#ffffff")+
    annotate(geom="tile",x=0,width=1.5,y=-8,height=15,color="#ffffff00",fill="#BCBDAC") +
    theme(plot.margin=unit(c(-10,-10,-10,-10),"mm"))
  
  #ggsave(str_c("output/candle/",i,".pdf"), width=8, height=8, device=cairo_pdf)
  ggsave(str_c("output/candle/",i,".png"), width=8, height=8)
  
}

img_png <- fs::dir_ls("output/candle/")
img_png[sample(c(1,2,3,4,5,6,7,8,9),size=51,replace=T, prob=c(1,2,3,4,3,2,1,1,1))] %>% 
  map(image_read) %>%
  image_join() %>%
  image_write_video("output/candle3.mp4",framerate=4)


img_png[c(1:9)] %>% 
  map(image_read) %>%
  image_join() %>%
  image_mosaic() %>%
  image_write("output/candle3.png")


?image_mosaic



params_r <- tibble(
  m = sqrt(10)-sqrt(seq(0,9,length=12)),
  n = 100,
  w = seq(1,3,length=12),
) %>% expand.grid() %>%
  filter(dense_rank(w)%%2==0) %>%
  mutate(start=0,end=pi)

params_l <- tibble(
  m = sqrt(10)-sqrt(seq(0,9,length=12)),
  n = 100,
  w = seq(1,3,length=12),
) %>% expand.grid() %>%
  filter(dense_rank(w)%%2==0) %>%
  mutate(start=pi,end=2*pi)

params <- bind_rows(params_r,params_l)



tear_df_r <- params_r %>% pmap_dfr(make_tear) %>% arrange(-w)
tear_df_l <- params_l %>% pmap_dfr(make_tear) %>% arrange(-w)
tear_df <- params %>% pmap_dfr(make_tear) %>% arrange(-w)


tear_df %>%
  ggplot(aes(x=x0,y=y0)) +
  #geom_shape(aes(group=rev(interaction(w,m)), fill=w), color="#ffffff00") +
  geom_shape(aes(group=rev(interaction(w,m)), fill=w), color="#ffffff00",data=tear_df_l) +
  geom_shape(aes(group=rev(interaction(w,m)), fill=w), color="#ffffff00",data=tear_df_r) +
  #geom_segment(data=NULL, aes(x=0,y=-1,xend=0,yend=1), color=my_col[10]) +
  scale_color_gradientn(colors=col_10,guide="none") +
  scale_fill_gradientn(colors=col_10, guide="none") +
  facet_wrap(~m, ncol=6) +
  theme_void(base_family="Roboto Condensed") 

ggsave("output/half_tear.pdf",width=16, height=16, device=cairo_pdf)





