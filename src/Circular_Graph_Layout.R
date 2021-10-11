

library(tidyverse)
library(tidygraph)
library(ggraph)
library(particles)
library(see)
library(ggforce)
library(patchwork)

my_cols <- str_c("#",c("cb997e","ddbea9","ffe8d6","b7b7a4","a5a58d","6b705c"))
my_cols2 <- str_c("#",c("b7094c","a01a58","892b64","723c70","5c4d7d","455e89","2e6f95","1780a1","0091ad"))
my_cols3 <- str_c("#",c("c9cba3","ffe1a8","e26d5c","723d46","472d30"))

my_cols2 <- my_cols3

ch<-5
make_pattern <- function(ch=4){
  #ch <- 4
  pt <- ch ^ c(0:ch)
  n <- ch ^ c(0:ch) %>% reduce(`+`)
  cut_pt <- ch ^ c(0:ch) %>% accumulate(`+`)
  g <-create_tree(n,ch) %>%
    activate("edges") %>%
    group_by(from) %>%
    mutate(idx_within = sample(row_number())) %>%
    ungroup() %>%
    activate("nodes") %>%
    mutate(idx=row_number(),
           rnk = rep(c(1:(ch+1)),times=pt))
  
  
  le_type1 <- sample(c("round","square","butt"),1)
  le_type2 <- sample(c("round","square","butt"),1)
  ew1 <- runif(n=1,min=0,max=1)
  #ew1 <-1
  #ew2 <-5
  ew2 <- runif(n=1,min=0,max=4)
  shape_type <- sample(c(1,3,4,5,15,16,18,20,19,21,22,23),1)
  shape_size <- sample(c(0,1,3,4,5),1)
  
  x_line <-g %>% as_tibble() %>% filter(rnk>3) %>%pull(rnk) 
  
  p<-g %>%
    ggraph("treemap", circular=T) +
    geom_edge_link0(aes(color=idx_within, 
                        filter=.N()$rnk[from] %in% 
                          c(sample(x_line,size=5))),
                    edge_width=ew1,
                    lineend=le_type1,
                    arrow = NULL)+  ## round, butt square
    geom_edge_link0(aes(color=idx_within, 
                        filter=.N()$rnk[from] %in% 
                          c(sample(x_line,size=5))),
                    edge_width=ew2,
                    lineend=le_type2,
                    arrow = NULL)+  ## round, butt square 
    geom_node_point(aes(color=idx%%ch, 
                        #filter=rnk==ch,
                        size=(idx%%shape_size)),
                    shape=shape_type)+  
    scale_edge_color_gradientn(colors=my_cols2,guide="none") +
    scale_color_gradientn(colors=my_cols2,guide="none") +
    coord_fixed() +
    theme(panel.background =element_rect(fill="#FFF8EB", color=NULL)) +
    scale_size_identity()
  
  finame <- str_c(str_pad(round(ew2*10),3,"left","0"),str_pad(round(ew1*100),3,"left","0"),shape_type)
  finame2 <- sample(str_pad(c(1:100),width=3,side="left","0"),1)
  
  print(p)

  #ggsave(str_glue("output/treemap_pat/{finame}{finame2}.pdf"), 
                  #width=8, height=8, device=cairo_pdf)
  
}



rep(4,9) %>%
  map(make_pattern) %>%
  reduce(`+`)

library(magick)
my_frames <- fs::dir_ls("output/treemap_pat/")

img <- sort(my_frames) %>%
  map(~image_read_pdf(.x, density=120)) %>%
  image_join()

img %>%
  image_write_video(path="output/pattern_anim2.mp4", framerate=6)


######  extra notes below

rep(5,10) %>%
  map(make_pattern) 

c(5,5,5,5,5,5,5,5,5) %>%
  map(make_pattern) %>%
  reduce(`+`)


ch <- 4
pt <- ch ^ c(0:ch)
n <- ch ^ c(0:ch) %>% reduce(`+`)
cut_pt <- ch ^ c(0:ch) %>% accumulate(`+`)
g <-create_tree(n,ch) %>%
  activate("edges") %>%
  group_by(from) %>%
  mutate(idx_within = sample(row_number())) %>%
  ungroup() %>%
  activate("nodes") %>%
  mutate(idx=row_number(),
         rnk = rep(c(1:(ch+1)),times=pt))


le_type1 <- sample(c("round","square","butt"),1)
le_type2 <- sample(c("round","square","butt"),1)
ew1 <- runif(n=1,min=0,max=1)
ew2 <- runif(n=1,min=1,max=4)
shape_type <- sample(c("square","triangle","circle"),1)
shape_size <- sample(c(0,1,2,3,4,5),1)

g %>%
  ggraph("treemap", circular=T) +
  geom_edge_link0(aes(color=idx_within, 
                      filter=.N()$rnk[from]==ch-2),
                  edge_width=ew1,
                  lineend=le_type1,
                  arrow = NULL)+  ## round, butt square
  geom_edge_link0(aes(color=idx_within, 
                    filter=.N()$rnk[from]>ch-1),
                  edge_width=ew2,
                  lineend=le_type2,
                  arrow = NULL)+  ## round, butt square 
  geom_node_point(aes(color=idx%%ch, 
                      filter=rnk==ch,
                      size=(idx%%shape_size)+2),
                  shape=shape_type)+  
  scale_edge_color_gradientn(colors=my_cols2,guide="none") +
  scale_color_gradientn(colors=my_cols2,guide="none") +
  coord_fixed() +
  theme(panel.background =element_rect(fill="#FFF8EB", color=NULL)) +
  scale_size_identity()




g %>%
  ggraph("tree", circular=T) +
  geom_edge_link0(aes(color=idx_within, filter=.N()$rnk[from] %in% c(ch,ch-2)),edge_width=1)+
  scale_edge_color_gradientn(colors=my_cols2,guide="none") +
  coord_fixed() +
  theme(panel.background =element_rect(fill="#FFF8EB", color=NULL))


## ch-1--> ch-ch

rng_min<-0
rng_max<-0.3
draw_dendro <- function(ch=5,rng_min=0,rng_max=5,rings=2){
  
  #ch <- 5
  pt <- ch ^ c(0:ch)
  n <- ch ^ c(0:ch) %>% reduce(`+`)
  cut_pt <- ch ^ c(0:ch) %>% accumulate(`+`)
  g <-create_tree(n,ch) %>%
    activate("edges") %>%
    group_by(from) %>%
    mutate(idx_within = sample(row_number())) %>%
    ungroup() %>%
    activate("nodes") %>%
    mutate(idx=row_number(),
           rnk = rep(c(1:(ch+1)),times=pt))
  
  edge_rng <- runif(n=2,min=rng_min,max=rng_max)
  rings <- rings
  my_lineend <- sample(c("butt","square","round"),1)
  
  g %>%
    activate("edges") %>%
    arrange(-idx_within) %>%
    ggraph("tree",circular=T) +
    geom_edge_diagonal0(aes(color=idx_within, 
                            filter=.N()$rnk[from] %in% c(ch:(ch-rings)), 
                            edge_width=.N()$rnk[to]),
                        lineend=my_lineend)+
    scale_edge_color_gradientn(colors=my_cols2,guide="none") +
    scale_edge_width_continuous(range=edge_rng,guide="none") +
    coord_fixed() +
    theme(panel.background =element_rect(fill="#FFF8EB", color=NULL))
  
  ggsave(filename=str_glue("output/circle/diag_{rings}_{my_lineend}_{round((edge_rng[1])*100)}_{round(edge_rng[2]*100)}.pdf"), width=8, height=8, device=cairo_pdf)
  
  g %>%
    activate("edges") %>%
    arrange(-idx_within) %>%
    ggraph("tree",circular=T) +
    geom_edge_bend0(aes(color=idx_within, 
                        filter=.N()$rnk[from] %in% c(ch:(ch-rings)), 
                        edge_width=.N()$rnk[to]),
                    lineend=my_lineend)+
    scale_edge_color_gradientn(colors=my_cols2,guide="none") +
    scale_edge_width_continuous(range=edge_rng,guide="none") +
    coord_fixed() +
    theme(panel.background =element_rect(fill="#FFF8EB", color=NULL))
  
  ggsave(filename=str_glue("output/circle/bend_{rings}_{my_lineend}_{round((edge_rng[1])*100)}_{round(edge_rng[2]*100)}.pdf"), width=8, height=8, device=cairo_pdf)
  
  g %>%
    activate("edges") %>%
    arrange(-idx_within) %>%
    ggraph("linear",circular=T,use.numeric=T,offset=-pi/2) +
    geom_edge_link0(aes(color=idx_within, 
                        filter=.N()$rnk[from] %in% c(ch:(ch-rings)), 
                        edge_width=.N()$rnk[to]),
                    lineend=my_lineend)+
    scale_edge_color_gradientn(colors=my_cols2,guide="none") +
    scale_edge_width_continuous(range=edge_rng,guide="none") +
    coord_fixed() +
    theme(panel.background =element_rect(fill="#FFF8EB", color=NULL))
  
  ggsave(filename=str_glue("output/circle/link_{rings}_{my_lineend}_{round((edge_rng[1])*100)}_{round(edge_rng[2]*100)}.pdf"), width=8, height=8, device=cairo_pdf)
  
}

draw_dendro(ch=5,rng_min=0,rng_max=2,rings=5)
list(ch=5,rng_min=0,rng_max=1,rings=c(1:5)) %>%
  cross_df() %>%
  pmap(.,draw_dendro)



library(magick)
my_frames <- fs::dir_ls("output/circle/") %>% sort()

img <- sort(my_frames) %>%
  map(~image_read_pdf(.x, density=120)) %>%
  image_join()

img %>%
  image_write_video(path="output/pattern_circle2.mp4", framerate=10)




cp_layout <- g %>% create_layout("circlepack")

cp_layout %>%
  filter(depth>=ch-2) %>%
  ggplot() +
  geom_voronoi_tile(aes(x=x,y=y, fill=idx%%ch), 
                    max.radius=min(cp_layout$r)*10, color="white")+
  scale_fill_gradientn(colors=my_cols2,guide="none") +
  coord_fixed() +
  theme_void()






