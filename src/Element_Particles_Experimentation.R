### Experimentation with Particles Package

library(tidyverse)
library(particles)
library(tidygraph)
library(ggraph)

sim <- create_empty(1000) %>%
  mutate(x_orig = (runif(1000,min=-50,max=50)),
         y_orig = (runif(1000,min=-80,max=-50))) %>%
  mutate(idx = row_number()) %>%
  mutate(grp=rescale(idx%%16,from=c(0,16),to=c(0,2*pi)))



circle_poly <- tibble(
  t = seq(0,2*pi,length=360)
) %>% mutate(x=(sin(0.5*t)^3*sin(t))*100,
             y=cos(t)*100) %>%
  select(x,y) %>%
  as.matrix()

sim_res <- sim %>%
  simulate(setup=predefined_genesis(x=x_orig,y=y_orig)) %>%
  #simulate(setup=bigbang_genesis(vel_min=0,vel_max=0.1)) %>%
  wield(collision_force,radius=2.5) %>%
  #wield(reset_force, xvel = x_orig/10, yvel = y_orig/10) %>% 
  wield(x_force,x=cos(grp)*100) %>%
  wield(y_force,y=sin(grp)*100) %>%
  #wield(trap_force,polygon=circle_poly) %>%
  impose(polygon_constraint,polygon=circle_poly) %>%
  #impose(infinity_constraint,xlim=c(0,100),ylim=c(0,100)) %>%
  evolve(100,record)


hmm <-sim_res$history %>% map_dfr(.,~position(.) %>% as_tibble(),.id="evo") 
hmm2 <- sim_res$history %>% map_dfr(.,~velocity(.) %>% as_tibble()) 
hmm$grp = rep(1:1000,times=100)


hmm_comb <- bind_cols(hmm,hmm2)
names(hmm_comb) <- c("evo","x","y","grp","x_vel","y_vel")

hmm_comb %>% 
  arrange(grp%%16) %>%
  ggplot(aes(x=x,y=y,color=factor(grp%%16)))+
  geom_path(aes(group=grp,color=factor(grp%%16))) +
  #geom_point(aes(group=grp,color=factor(grp%%16), 
                 #alpha=-as.numeric(evo),size=-as.numeric(evo))) +
  #geom_point(data=. %>% filter(evo=="100")) +
  theme_minimal() +
  coord_fixed() +
  scale_color_material_d(guide="none") +
  scale_size_continuous(range=c(0.1,1)) +
  geom_path(data=circle_poly %>% as_tibble(), aes(x=x,y=y,group=1), color="black")


ggsave("output/hairball.pdf", width=10,height=10,device=cairo_pdf)


#Firework colours
colours <- str_c("#",c("001219","005f73","0a9396","94d2bd","e9d8a6","ee9b00","ca6702","bb3e03","ae2012","9b2226"))

draw_mesh <- function(strength=-30){
  
  create_lattice(dim=c(21,21)) %>%
    mutate(id=row_number()) %>%
    simulate() %>%
    wield(link_force, distance=c(1:441)%%21) %>%
    wield(manybody_force, strength=strength) %>%
    evolve() %>%
    as_tbl_graph() %>%
    mutate(idx=row_number()) %>%
    ggraph("manual",x=.N()$x,y=.N()$y) +
    #geom_edge_elbow0(aes(direction=.N()$x_vel[from]), color="blue") +
    geom_edge_link0(aes(color=.N()$idx[to]%%10)) +
    theme_void(base_family="Roboto Condensed") +
    scale_edge_color_gradientn(colours=colours, guide="none") +
    labs(title=str_c(strength))
  
}


draw_mesh2 <- function(strength=-30){
  
  create_lattice(dim=c(21,21)) %>%
    mutate(id=row_number()) %>%
    simulate() %>%
    wield(link_force, distance=c(1:441)%%21) %>%
    wield(manybody_force, strength=strength) %>%
    evolve() %>%
    as_tbl_graph() %>%
    mutate(idx=row_number()) %>%
    as_tibble() %>%
    ggplot(aes(x=x,y=y)) +
    geom_voronoi_tile(aes(fill=idx%%10), color="#ffffff00", max.radius=150) +
    scale_fill_gradientn(colours=colours, guide="none") +
    theme_void() +
    coord_fixed(xlim=c(-500,500),ylim=c(-500,500)) 
  
  ggsave(str_glue("output/voronoi/{str_pad(round(abs(strength)*10),width=3,side='left','0')}.pdf"),width=10, height=10, device=cairo_pdf)
  
}

?str_pad

library(ggforce)

draw_mesh(-1.6)

seq(-1,-9,by=-0.1) %>% map(draw_mesh2)

my_imgs <- fs::dir_ls("output/voronoi")
library(magick)
img_joined <-my_imgs %>% map(~image_read_pdf(.,density=100)) %>% image_join() 
c(img_joined,rev(img_joined)[-1]) %>% image_write_video("output/vononoi_anim2.mp4", framerate=4)

seq(-1,-40, by=-1) %>%
  map(draw_mesh2)

seq(-130,-1000, by=-100) %>%
  map(draw_mesh2)





sim_res %>% as_tbl_graph()

sim_res_g <-sim_res %>%  as_tbl_graph() %>%
  mutate(idx=row_number(),
         diff_x=x-x_orig,
         diff_y=y-y_orig,
         angle=atan2(diff_y,diff_x),
         dist = sqrt(diff_x^2+diff_y^2)) 
sim_res_g %>%
  ggraph(layout="manual", x=.N()$x, y=.N()$y) +
  geom_spoke(aes(x=x_orig,y=y_orig,angle=angle,alpha=dist,radius=dist, color=factor(att_x)), 
             data=sim_res_g %>% as_tibble()) +
  geom_point(aes(x=x_orig,y=y_orig, color=factor(att_x)), data=sim_res %>% as_tibble()) +
  geom_edge_link0(alpha=0.001)+
  geom_node_point(size=10,aes(color=factor(att_x)))+
  geom_node_text(aes(label=idx), color="white") +
  scale_color_material_d() 


sim %>% as_tbl_graph() %>%
  mutate(idx=row_number()) %>%
  ggraph(layout="manual", x=.N()$x, y=.N()$y) +
  geom_edge_link0()+
  geom_node_point(size=10)+
  geom_node_text(aes(label=idx), color="white") +
  coord_fixed()

sim_res %>% as_tibble()



sim1 <-sim %>% 
  simulate(velocity_decay = 0.6, setup = petridish_genesis(vel_max = 0)) %>% 
  wield(link_force) %>% 
  wield(manybody_force) %>% 
  impose(polygon_constraint, 
         polygon = cbind(c(-100, -100, 100, 100), c(-100, 100, 100, -100))) %>% 
  evolve(0)

sim_inf <-sim %>% 
  simulate(velocity_decay = 0.6, setup = petridish_genesis(vel_max = 0)) %>% 
  wield(link_force) %>% 
  wield(manybody_force) %>% 
  impose(polygon_constraint, 
         polygon = cbind(c(-100, -100, 100, 100), c(-100, 100, 100, -100))) %>% 
  evolve()

sim_inf$evolutions
sim_inf %>% unwield(100) %>%
  ggraph(layout="manual", x=.N()$x,y=.N()$y) +
  geom_node_point() +
  geom_edge_link0()

?unwield

sim %>% as_tbl_graph()
sim1 %>% as_tbl_graph() %>%
  ggraph(layout="manual", x=.N()$x,y=.N()$y) +
  geom_node_point() +
  geom_edge_link0()

sim_inf %>% as_tbl_graph() %>%
  ggraph(layout="manual", x=.N()$x,y=.N()$y) +
  geom_node_point() +
  geom_edge_link0()

