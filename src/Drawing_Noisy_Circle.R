library(tidyverse)
library(ambient)
library(ggforce)
library(packcircles)
library(see)

col_pal <- str_c("#",c("f94144", "f3722c", "f8961e", "f9844a", "f9c74f", "90be6d", "43aa8b", "4d908e", "577590", "277da1"))

my_df <- tibble(
  idx = c(1:1000),
  t = seq(0,2*pi,length.out=1000),
  x = cos(t),
  y = sin(t)
) %>%
  mutate(noise1 = normalize(gen_cubic(x,y,octave=2)),
         noise2 = normalize(gen_cubic(x,y,octave=3)),
         noise3 = normalize(gen_cubic(x,y,octave=4)),
         noise4 = normalize(gen_cubic(x,y,octave=3,fractal="none")),
         noise5 = normalize(gen_cubic(x,y,octave=3,fractal="billow")),
         noise6 = normalize(gen_cubic(x,y,octave=3,fractal="rigid-multi")),
         noise7 = normalize(gen_cubic(x,y,frequency=1)),
         noise8 = normalize(gen_cubic(x,y,frequency=3)),
         noise9 = normalize(gen_cubic(x,y,frequency=5)),
         noise10 = normalize(gen_cubic(x,y,frequency=0.2)),
         noise11 = normalize(gen_cubic(x,y,frequency=0.5)),
         noise12 = normalize(gen_cubic(x,y,frequency=2, lacunarity = 3)),
         noise13 = normalize(gen_cubic(x,y,frequency=2, lacunarity = 5)),
         noise14 = normalize(gen_cubic(x,y,frequency=2, lacunarity = 7)),
         noise15 = normalize(gen_cubic(x,y,frequency=2, pertubation = "normal")),
         noise16 = normalize(gen_cubic(x,y,frequency=2, pertubation = "fractal")),
         noise17 = normalize(gen_cubic(x,y,frequency=2, pertubation = "normal", pertubation_amplitude=3))) %>%
  pivot_longer(cols=noise1:noise17) %>%
  mutate(r=as.numeric(factor(name))+1,
         value=normalize(value,to=c(0.1,0.9))) %>%
  arrange(desc(r)) %>%
  mutate(name2=fct_inorder(str_c(r,name)))


range(my_df$r)
?circleProgressiveLayout
packcircle_df <- packcircles::circleProgressiveLayout(rbeta(50,1,1))  %>%
  mutate(grp = row_number(),
         n = floor(radius/0.05),
         max_ring =n) %>%
  uncount(weights=n) %>%
  group_by(grp) %>%
  mutate(idx_within = row_number(),
         r_adj = idx_within/max_ring * radius) %>%
  rename(x0=x,y0=y)

packcircle_df %>%
  ggplot() +
  geom_circle(aes(x0=x0,y0=y0,r=r_adj, color=idx_within)) +
  theme_void() +
  scale_color_material_c()

make_noise_circle <- function(r_adj,...){
  
  tibble(
    i = c(1:100),
    t = seq(0,2*pi,length.out=100),
    x = (r_adj*cos(t)),
    y = (r_adj*sin(t)) 
  ) %>% mutate(noise=normalize(gen_cubic(x,y),to=c(0.01,0.05))) %>%
    mutate(x = x + noise,
           y = y + noise)
}




big_df <- packcircle_df %>% 
  mutate(data = map(r_adj, make_noise_circle)) %>%
  unnest(data) %>%
  mutate(x2 = x0+x,
         y2 = y0+y) %>%
  arrange(desc(r_adj),grp,desc(idx_within))

big_df %>%
  ggplot(aes(x=x2,y=y2)) +
  geom_shape(aes(group=interaction(grp,idx_within),fill=factor(idx_within))) +
  geom_path(aes(group=interaction(idx_within,grp),color=factor(idx_within))) +
  scale_color_manual(values=colorRampPalette(col_pal)(17), guide="none", name="") +
  scale_fill_manual(values=colorRampPalette(col_pal)(17), name="") +
  theme_void() +
  coord_fixed()

ggsave("output/packed_tree_ring.pdf", width=16, height=16, device=cairo_pdf)


make_noise_circle() %>%
  ggplot(aes(x=x,y=y)) +
  geom_path() +
  coord_fixed()

my_df %>% 
  ggplot(aes(x=x,y=y)) +
  geom_path() +
  #geom_path(aes(x=(value+r)*x,y=(value+r)*y,color=name),size=0.3) +
  #geom_path(aes(x=(value-r)*x,y=(value-r)*y,color=name),size=0.3) +
  #geom_shape(aes(x=(r+value)*x,y=(r+value)*y,fill=name2),size=0.3) +
  geom_shape(aes(x=(r-value)*x,y=(r-value)*y,fill=name2),size=0.3) +
  coord_fixed() +
  scale_color_manual(values=colorRampPalette(col_pal)(17)) +
  scale_fill_manual(values=colorRampPalette(col_pal)(17)) +
  theme_void() 

ggsave("output/treering.pdf", width=16, height=16, device=cairo_pdf)


?geom_ribbon
my_df %>% 
  ggplot(aes(x=idx)) +
  geom_ribbon(aes(fill=name, ymin=value+r, ymax=r-value)) +
  scale_color_manual(values=colorRampPalette(col_pal)(17)) +
  scale_fill_manual(values=colorRampPalette(col_pal)(17)) +
  theme_void() 

ggsave("output/treering_ribbon.pdf", width=16, height=16, device=cairo_pdf)

my_df %>% 
  ggplot(aes(x=idx)) +
  geom_area(aes(fill=name, y=r*value),position="stack") +
  scale_color_manual(values=colorRampPalette(col_pal)(17)) +
  scale_fill_manual(values=colorRampPalette(col_pal)(17)) +
  theme_void() +
  coord_polar()

ggsave("output/treering_ribbon2.pdf", width=16, height=16, device=cairo_pdf)
