
library(tidyverse)
library(packcircles)
library(ggraph)
library(tidygraph)
library(ggforce)


my_col <-str_c("#",c("d9ed92","b5e48c","99d98c","76c893","52b69a","34a0a4","168aad","1a759f","1e6091","184e77"))
r <-rbeta(n=1000,shape1=1,shape2=3)

my_layout <-packcircles::circleProgressiveLayout(x=r, sizetype="radius") %>%
  mutate(color=sample(my_col,size=1000,replace=T))


my_layout %>%
  ggplot() +
  geom_circle(aes(x0=x,y0=y,r=r,fill=r), color="#ffffff00") +
  coord_fixed() +
  scale_fill_gradientn(colours=my_col) +
  theme_void()
ggsave("output/texture_packcircle.pdf",width=16,height=16)

my_layout %>%
  mutate(color=sample(my_col,size=1000,replace=T,prob=c(1,1,2,3,3,3,5,8,13,13))) %>%
  ggplot(aes(x=x,y=y)) +
  geom_voronoi_tile(max.radius=0.9, aes(fill=radius)) +
  #geom_voronoi_tile(max.radius=0.9, aes(fill=color)) +
  coord_fixed() +
  #scale_fill_identity() +
  scale_fill_gradientn(colours=my_col, guide="none") +
  theme_void()

ggsave("output/texture_voronoi.pdf",width=16,height=16)

my_deldir <- deldir::deldir(my_layout)
my_deldir$summary
my_deldir$delsgs %>%
  ggplot() +
  geom_voronoi_tile(max.radius=0.9, aes(fill=radius,x=x,y=y), data=my_layout) +
  geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2),color="white") +
  geom_voronoi_tile(max.radius=0.3, aes(fill=radius,x=x,y=y), data=my_layout) +
  coord_fixed() +
  theme_void() +
  scale_fill_gradientn(colours=my_col, guide="none") 

ggsave("output/texture_voronoi2.pdf",width=16,height=16)

my_layout %>%
  mutate(color=sample(my_col,size=1000,replace=T,prob=c(1,1,2,3,3,3,5,8,13,13))) %>%
  ggplot(aes(x=x,y=y)) +
  geom_delaunay_tile(aes(fill=my_deldir$summary$dir.area)) +
  #geom_voronoi_tile(max.radius=0.9, aes(fill=color)) +
  coord_fixed() +
  #scale_fill_identity() +
  scale_fill_gradientn(colours=my_col, guide="none") +
  theme_void()


star_df <- create_star(n=8) %>%
  create_layout("star") 

star_df %>%
  ggraph("nicely") +
  geom_edge_link() +
  coord_fixed()
