

library(particles)
library(tidygraph)
library(scales)
library(ambient)

my_grid <-long_grid(x=seq(0,1,length.out=100),y=seq(0,1,length.out=100))
my_grid$perlin <-gen_perlin(x=my_grid$x,y=my_grid$y)
plot(my_grid,perlin)

my_grid %>% mutate(angle=rescale(perlin,to=c(-pi,pi))) %>%
  ggplot(aes(x=x,y=y)) +
  geom_spoke(aes(angle=angle, radius=0.1*perlin, color=angle)) +
  theme_void() +
  scale_color_viridis_c() +
  facet_wrap(~z)

my_noise <- noise_perlin(dim=c(10,10))
my_tree_lc %>% count(x,y) %>% as_tibble()

?gen_perlin

create_ring(16) %>%
  ggraph() +
  geom_edge_link() +
  coord_fixed()

5^c(0:5) %>% cumsum()

my_tree <- create_tree(781,5) %>%
  mutate(idx=row_number(),
         color=sample(cols,size=781,replace=T)) ## Random Colour Assignment for Now

my_tree_l<-my_tree %>% create_layout("tree")%>% 
    mutate(x=case_when(y==0 ~ 2.5*x,
                       y==1 ~ 1.5*x,
                       y==3 ~ 0.7*x,
                       y==4 ~ 0.2*x,
                       TRUE ~ x),
           y = log(y+1))


library(fuzzyjoin)


my_tree_lc<-my_tree %>% create_layout("tree", circular=T) %>%
  mutate(angle= atan2(y,x),
         r = round(x/cos(angle),2),
         r2 = ifelse(r==3/4,0.3,ifelse(r==1/2,0.1,ifelse(r==1/4,0.05,r))),
         x = r2*cos(angle),
         y = r2*sin(angle)) 
  



## Alter the position bit here.


my_tree_l  %>%
  ggraph("manual",x=.N()$x,y=.N()$y) +
  geom_edge_bend(n=3,aes(color=.N()$color[from])) +
  #geom_node_text(aes(label=idx)) +
  theme_void() +
  scale_edge_color_identity() 

my_tree_lc  %>%
  ggraph("manual",x=.N()$x,y=s.N()$y) +
  geom_edge_bend(n=3,aes(color=.N()$color[from])) +
  theme_void() +
  scale_edge_color_identity()  +
  coord_fixed()

ggsave("output/Flower.pdf", width=16, height=16, device=cairo_pdf)

r*cos(angle)=x
my_tree_lc %>% count(r)


# Simulation
graph %>% simulate(velocity_decay = 0.7, setup = predefined_genesis(x, y)) %>% 
  wield(collision_force, radius = radius, n_iter = 2) %>% 
  wield(x_force, x = 0, strength = 0.002) %>% 
  wield(y_force, y = 0, strength = 0.002) %>% 
  evolve(on_generation = graph_plot)

