library(patchwork)  
library(tidyverse)
library(ggforce)

data <- tibble(
  x = rnorm(10000,0,1),
  y = rnorm(10000,0,1)
)

n=10
shape=23
size=25
draw_test <- function(n=10, sides=3){
  
  h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
  dens <- MASS::kde2d(data$x, data$y, h=h, n=n)
  
  nx <- nrow(data) # number of observations in this group
  df <- expand.grid(x = dens$x, y = dens$y)
  df$density <- as.vector(dens$z)
  #df$group <- data$group[1]
  df$ndensity <- df$density / max(df$density, na.rm = TRUE)
  df$count <- nx * df$density
  df$n <- nx
  df$level <- 1
  df$piece <- 1
  df$sides <- sides
  #df$angle <- scales::rescale(df$x,to=c(-2*pi,2*pi))
  df$angle <- atan2(df$y,df$x)
  df$r <- ((range(df$x) %>% diff())/n)/2
  df$y2 <- if_else(dense_rank(df$x)%%2==0,df$y+df$r,df$y)
  
  my_cols <- str_c("#",c("c9cba3","ffe1a8","e26d5c","723d46","472d30"))
  
  df %>% ggplot() +
    geom_regon(aes(x0=x,y0=y2,fill=density, sides=sides, angle=angle,r=r),
               color="#ffffff00",radius = unit(2,"mm"), expand=unit(1,"mm")) +
    geom_regon(aes(x0=x,y0=y2,fill=density, sides=sides, angle=angle+pi/4,r=r/2),
               color="#ffffff90",size=2) +
    scale_fill_gradientn(colors=c("#ffffff00",my_cols), guide="none") +
    #scale_size_continuous(guide="none", range=c(10,25))+
    theme_void() +
    coord_fixed(xlim=c(-2,2),y=c(-2,2)) +
    theme(panel.background =element_rect(fill="#FFF8EB", color=NULL)) 
  #coord_fixed()
  
  ggsave(str_glue("output/circle/pat_{str_pad(n,3,'left','0')}_{str_pad(sides,3,'left','0')}.pdf"), width=8, height=8, device=cairo_pdf)
  
}

draw_test(25,9)

sqrt(2)

list(n=c(11,12,13,14,24,25),sides=c(3,4,5,6,8,9,11,12)) %>%
  cross_df() %>%
  pmap(., draw_test) 

library(magick)
my_frames <- fs::dir_ls("output/circle")

img <- sort(my_frames) %>%
  map(~image_read_pdf(.x, density=120)) %>%
  image_join()

48/12

img %>%
  image_write_video(path="output/pattern_anim3_3.mp4", framerate=12)

img[sample(c(1:48),size=4)] %>% image_append()


xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels = breaks
  )
}

data <- tibble(
  x = rnorm(10000,0,1),
  y = rnorm(10000,0,1)
)

draw_nested <- function(ring=20,seed=777,a=8,b=8){
  
  set.seed(seed)
  data <- tibble(
    x = scales::rescale(rbeta(10000,a,b)),
    y = scales::rescale(rbeta(10000,a,b))
  )
  
  my_blues <- str_c("#",c("03045e","023e8a","0077b6","0096c7","00b4d8","48cae4","90e0ef","ade8f4","caf0f8"))
  
  
  n <- 100
  h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
  dens <- MASS::kde2d(data$x, data$y, h=h, n=n)
  
  nx <- nrow(data) # number of observations in this group
  df <- expand.grid(x = dens$x, y = dens$y)
  df$z <- as.vector(dens$z)
  
  df <-df %>%
    mutate(angle=atan2(y,x),r=scales::rescale(x,to=c(0,max(x))),
           angle2 = scales::rescale(z,to=c(-pi,pi)))
  
  p <-df %>%
    ggplot(aes(x=x,y=y)) +
    geom_contour(aes(z=z),bins=ring) +
    coord_fixed(xlim=c(-2,2),ylim=c(-2,2)) +
    scale_color_gradientn(colors=my_cols, guide="none") +
    theme_void()
  
  p <-p %>% ggplot_build()
  
  p$data[[1]] %>% as_tibble() %>% #count(group)
    ggplot(aes(group=group, x=x,y=y)) +
    annotate(geom="rect",xmin=0,xmax=1,ymin=0,ymax=1,fill="#010228", color="#ffffff00") +
    geom_shape(aes(fill=order)) +
    #geom_path(color="#ffffffde", size=0.05) +
    # with_shadow(
    #   geom_path(aes(group=group), colour="#00000090",size=1) ,
    #   x_offset=0.5,y_offset=-0.5, sigma=15, stack=F
    # ) +
    # with_shadow(
    #   geom_path(aes(group=group), colour="#ffffffde",size=0.05) ,
    #   x_offset=-1,y_offset=1, sigma=5, stack=T
    # ) +
    scale_fill_gradientn(colors=rev(my_blues), guide="none", trans="log") +
    theme_void() +
    coord_fixed(xlim=c(0,1),ylim=c(0,1)) +
    theme(plot.background=element_rect(fill="#010228", color="#ffffff00"))
}

draw_nested(20,777,10,10)
draw_nested(20,777,9,9)
draw_nested(20,777,8,8)
draw_nested(20,777,7,7)
draw_nested(20,777,6,6)
draw_nested(20,777,5,5)
draw_nested(20,777,4,4)
draw_nested(20,777,3,3)
draw_nested(20,777,2,2)
draw_nested(20,777,1,1)


draw_nested(10,777)
length(c(9:24))

param_df <- list(ring=c(12:36),seed=c(777),a=c(10),b=c(10)) %>% cross_df() %>% arrange(a,b)

plots <- pmap(param_df, draw_nested)

names(plots) <- c(1:nrow(param_df))
paths <- stringr::str_c("output/circle/",str_pad(names(plots),3,"left","0"), ".pdf")

pwalk(list(paths,plots), ggsave, width=8, height=8, device=cairo_pdf)


library(magick)
my_frames <- fs::dir_ls("output/circle")
c(my_frames,rev(my_frames))

img <- c(my_frames,rev(my_frames)) %>%
  map(~image_read_pdf(.x, density=120)) %>%
  image_join()


img %>%
  image_write_video(path="output/pattern_anim_beta3.mp4", framerate=10)

img[sample(c(1:48),size=4)] %>% image_append()


?rnorm

param_df <- list(n=100,mean=seq(0,6,length=4),sd=seq(0.1,10,length=8)) %>%
  cross_df() %>%
  mutate(data=pmap(.,rnorm))

?slice_sample
param_df %>% unnest(data) %>%
  mutate(grp=str_c(mean,sd)) %>%
  slice_sample(prop=0.7, weight_by=sd) %>%
  ggplot() +
  geom_density(aes(x=mean+sd,y=after_stat(density),fill=grp), 
               color="#ffffff00") +
  scale_fill_manual(values=colorRampPalette(my_blues)(nrow(param_df)), guide="none") +
  theme_void() +
  coord_cartesian(ylim=c(0,0.75)) +
  scale_y_continuous(trans="sqrt")

ggsave("output/deleteme.pdf", width=8, height=8, device=cairo_pdf)

