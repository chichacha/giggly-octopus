

library(tidyverse)
library(ggforce)
library(see)
library(ggfx)
library(scales)
library(TSP)

library(patchwork)
library(ambient)


gen_mtn <- function(mtn=21,offset=0,w=60,h=22,seed=777,...){
  
  my_blues <- str_c("#",c("03045e","023e8a","0077b6","0096c7","00b4d8","48cae4","90e0ef","ade8f4","caf0f8"))
  
  set.seed(seed)
  
  n <- mtn ## Number of Mountain
  df <-tibble(r=rbeta(n=n,shape1=8,shape2=6),x=runif(n=n,min=0,max=2)) %>%
    mutate(y=r/2,
           z_idx=row_number(desc(r))) %>%
    arrange(z_idx) %>%
    mutate(fill=colorRampPalette(my_blues)(n))
  
  num_star_w <- w
  num_star_h <- h
  
  df_stars <- list(t=seq(-pi,pi,length=num_star_w),
                   r=seq(min(df$r)*2,max(df$x)*1.2,length=num_star_h)) %>%
    cross_df() %>% 
    mutate(x = r * cos(t) + 1,
           y = r * sin(t),
           angle=runif(num_star_w*num_star_h,0,360)) %>%
    slice_sample(prop=0.6, weight_by=r)
  
  hc_res <-hclust(dist(df_stars %>% select(x,y), method="minkowski"))
  k<-ceiling(nrow(df_stars)/5)
  df_stars$grp <- cutree(hc_res,k=k)
  
  df_stars <- df_stars %>% add_count(grp,name="grp_mem") %>%
    mutate(size=(r/grp_mem)+rbeta(nrow(df_stars),1,1))
  
  
  stars_tsp <-dist(df_stars %>% select(x,y) %>% mutate_all(rescale), method="euclidean") %>% as.TSP()
  stars_tour <-solve_TSP(stars_tsp, method="nn") %>% as.numeric()
  
  df_stars2 <- df_stars[stars_tour,]
  
  noise <- 0
  
  df_stars <- df_stars %>% 
    mutate(t = t+noise,
           x = r * cos(t + offset) + 1 ,
           y = r * sin(t + offset))
  
  df_stars2 <- df_stars2 %>% 
    mutate(t = t+noise,
           x = r * cos(t + offset) + 1,
           y = r * sin(t + offset))
  
  
  p <-df %>% ggplot() +
    geom_path(data=df_stars2, aes(x=x,y=y,group=factor(grp)), color="white", size=0.1) +
    geom_text(data=df_stars, aes(x=x,y=y, label="*", size=size, angle=angle), 
              color="#E7D84B", family="Roboto Condensed", fontface="bold") +
    geom_regon(aes(x0=x,y0=r/2,r=r,sides=3,angle=0),fill="gray8",
               alpha=1, radius=unit(5,"mm")) +
    geom_regon(aes(x0=x,y0=r/2,r=r,sides=3,angle=0,fill=fill), 
               data=. %>% filter(z_idx%%3==0) ,
               alpha=0.7, radius=unit(5,"mm")) +
    geom_regon(aes(x0=x,y0=r/2,r=r,sides=3,angle=0,fill=fill), 
               data=. %>% filter(z_idx%%3==1) ,
               alpha=0.7, radius=unit(5,"mm")) +
    geom_regon(aes(x0=x,y0=r/2,r=r,sides=3,angle=0,fill=fill), 
               data=. %>% filter(z_idx%%3==2), 
               alpha=0.7, radius=unit(5,"mm")) +
    annotate(geom="rect",xmin=min(df$x)-max(df$r), xmax=max(df$x)+max(df$r),
             ymin = -0.5, ymax=0.1, fill="gray3", alpha=1) +
    coord_fixed(xlim=range(df$x),ylim=c(0,diff(range(df$x))), clip="on") +
    theme_void() +
    theme(plot.margin=margin(0,0,0,0,"mm"),
          plot.background=element_rect(fill="grey9", color="#00000000")) +
    scale_fill_identity() +
    scale_size_continuous(range=c(0,10), guide="none") +
    scale_alpha_continuous(range=c(0.2,1), guide="none") 
  
  print(p)
  
}

gen_mtn(21,pi/4,w=60,h=22,seed=777)
gen_mtn(21,0,w=60,h=22,seed=777)

params <- list(mtn=sample(c(11:27),size=10, replace=F),offset=sqrt(pi/5),w=60,h=22,seed=c(777)) %>%
  cross_df()

params %>% pmap(.,gen_mtn) %>%
  iwalk(., ~ggsave(str_c("output/mtn/",str_pad(.y,3,"left","0"),".pdf"),plot=.x, width=8, height=8, device=cairo_pdf))
  
library(magick)
my_frames <- fs::dir_ls("output/mtn")



img <- c(my_frames) %>%
  map(~image_read_pdf(.x, density=120)) %>%
  image_join()

img %>%
  image_write_video(path="output/mtn_stars.mp4", framerate=2)

ggsave("output/mountain.pdf", width=16, height=16, device=cairo_pdf)
  
?coord_fixed
