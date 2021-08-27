

library(tidyverse)
library(see)
library(packcircles)
library(ggforce)
library(ambient)

col_10 <- str_c("#",c("f94144","f3722c","f8961e","f9844a","f9c74f","90be6d","43aa8b","4d908e","577590","277da1"))

my_grid <- long_grid(x=c(1:10),y=c(1:10)) %>%
  mutate(shape1 = runif(1,5,n=100),
         shape2 = 1,
         n=10) %>%
  mutate(offset=(x%%2)*0.5,
         y_offset=y+offset)

my_grid_long <- my_grid %>% 
  mutate(radius = pmap(list(n=n,shape1=shape1,shape2=shape2),rbeta),
         radius_norm = map(radius,normalize,to=c(0.2,1))) %>%
  unnest(radius:radius_norm) %>%
  mutate(angle=normalize(radius_norm,to=c(0,2*pi)))

my_grid_long %>%
  ggplot() +
  #geom_circle(aes(x0=x,y0=y_offset,r=radius_norm/2.1,color=radius),n=60) +
  geom_regon(aes(x0=x,y0=y_offset,r=radius_norm/1.8,color=(radius),sides=4,
                 angle=sqrt(angle)+pi/5),fill="#ffffff00") +
  coord_fixed() +
  scale_color_gradientn(colours=col_10,guide="none") +
  theme_void() +
  theme(plot.background = element_rect(fill="#141621", color="#ffffff00"),
        legend.box.spacing = unit(0, "mm"))

ggsave("output/Random_Not_Random_Square4.pdf", width=16, height=16, device=cairo_pdf)

