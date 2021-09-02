## Tear Drop Simpliefied

library(tidyverse)
library(particles)
library(ambient)
library(see)
library(sf)
library(packcircles)
library(ggforce)

### Week of Element : Water
## Mon: Droplet
## Tue: Pool
## Wed: Glass of Water
## Thursday : Rain
## Snow/Ice
## River
## Wave



make_tear <- function(m,n=100,...) {
  
  teardrop <- tibble(
    m = m,
    t = seq(0,2*pi,length=n),
  ) %>% mutate(
    x0 = sin(t) * sin(0.5*t)^(m),
    y0 = cos(t)
  ) 
  
  return(teardrop)
}

log(148)
exp(4)

test <-c(log(exp(4))-log(seq(1,exp(4),length=120))) %>%
  map_dfr(make_tear) %>%
  mutate(grp = rep(c(1:120),each=100))



test %>% ggplot(aes(x=m)) +
  stat_count()

test %>%
  ggplot(aes(x=x0,y=y0,grp=grp)) +
  geom_path(aes(color=m)) +
  coord_fixed() +
  scale_color_material_c()

test_sf <- test %>%
  st_as_sf(coords=c("x0","y0")) %>%
  group_by(grp,m) %>%
  summarise(geometry=st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  mutate(area=st_area(geometry),
         amp = 1/(pi/as.numeric(area)))

test_sf %>% st_drop_geometry() %>% arrange(area)


packed_coords <- circleRepelLayout(data.frame(x=test_sf$m,
                                              y=test_sf$grp%%5,
                                              test_sf$amp), sizetype="radius",
                                   maxiter=5000)
packed_coords

my_layout <- packed_coords$layout

test_comb <- bind_cols(test %>% group_nest(m,grp),my_layout) %>%
  unnest(data)

my_col <-str_c("#",c("d9ed92","b5e48c","99d98c","76c893","52b69a","34a0a4","168aad","1a759f","1e6091","184e77"))


test_comb %>%
  ungroup() %>%
  arrange(desc(grp)) %>%
  ggplot(aes(x=(x0*radius),y=(y0*radius)+radius*0.3)) +
  geom_shape(aes(fill=factor(floor((grp-1)/12)),group=grp), color="#ffffff00")+
  geom_circle(data=tibble(grp=c(0:11),x0=-0.2,y0=0,r=runif(12,0.15,0.20)), 
              aes(x0=x0,y0=y0,r=r),
              inherit.aes=F, fill="#ffffff", color="#ffffff00") +
  geom_circle(data=tibble(grp=c(0:11),x0=0.2,y0=0,r=runif(12,0.15,0.20)), 
              aes(x0=x0,y0=y0,r=r),
              inherit.aes=F, fill="#ffffff", color="#ffffff00") +
  geom_circle(data=tibble(grp=c(0:11),x0=-0.2,y0=0,r=runif(12,0.13,0.14)), 
              aes(x0=x0,y0=y0,r=r),
              inherit.aes=F, fill="#000000", color="#ffffff00") +
  geom_circle(data=tibble(grp=c(0:11),x0=0.2,y0=0,r=runif(12,0.12,0.14)), 
              aes(x0=x0,y0=y0,r=r),
              inherit.aes=F, fill="#000000", color="#ffffff00") +
  geom_regon(data=tibble(grp=c(0:11),x0=0,y0=-0.3,r=runif(12,0.05,0.09),
                         angle =seq(0,2*pi,length=12),sides=sample(c(5:12),size=12,replace=T)), 
              aes(x0=x0,y0=y0,r=r,angle=angle,sides=sides),
              inherit.aes=F, fill="#000000", color="#ffffff00", alpha=0) +
  #geom_text(aes(label=round(m), x=x,y=y-0.3), data=. %>% count(x,y,m)) +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values=colorRampPalette(my_col)(12), guide="none") +
  scale_color_material_c(guide="none") +
  facet_wrap(~(grp-1)%%12,ncol=4)

ggsave("output/rain_droplets.pdf",width=16, height=16, device=cairo_pdf)



test_comb %>%
  ungroup() %>%
  arrange(desc(grp)) %>%
  ggplot(aes(x=(x0*radius)+x,y=(y0*radius)+y)) +
  geom_delaunay_segment(data=my_layout, aes(x=x,y=y), linetype=3)+
  geom_shape(aes(fill=factor(floor((grp-1)/12)),group=grp), color="#ffffff00") +
  geom_circle(data=my_layout,aes(x0=x-(radius*0.1),y0=y,r=radius*0.3),
              inherit.aes=F, fill="white", color="#ffffff00") +
  geom_circle(data=my_layout,aes(x0=x+(radius*0.3),y0=y,r=radius*0.3),
              inherit.aes=F, fill="white", color="#ffffff00") +
  # geom_circle(data=my_layout,aes(x0=x-(radius*0.1),y0=y,r=radius*0.12),
  #             inherit.aes=F, fill="black", color="#ffffff00") +
  geom_point(data=my_layout %>% mutate(shape=sample(c(20,19,16,120),size=nrow(my_layout),replace=T)),
              aes(x=x-radius*0.1,y=y,shape=shape,size=radius/2),
              color="black") +
  geom_point(data=my_layout %>% mutate(shape=sample(c(20,19,16,120),size=nrow(my_layout),replace=T)),
             aes(x=x+radius*0.3,y=y,shape=shape,size=radius/3),
             color="black") +
  # geom_circle(data=my_layout,aes(x0=x+(radius*0.3),y0=y,r=radius*0.12),
  #             inherit.aes=F, fill="black", color="#ffffff00") +
  # geom_point(data=my_layout %>% mutate(shape=sample(c(20,19,16,4,120,111,79,94,95),
  #                                                   size=nrow(my_layout),replace=T)),
  #            aes(x=x+0.1, y=y-radius*0.5, shape=shape, size=radius*1.3),
  #            inherit.aes=F, color="#e76f51") +
  #geom_text(aes(label=round(m), x=x,y=y-0.3), data=. %>% count(x,y,m)) +
  #coord_fixed(xlim=c(-5,5),ylim=c(-5,5)) +
  coord_fixed() +
  theme_void() +
  scale_fill_manual(values=colorRampPalette(my_col)(12), guide="none") +
  scale_color_material_c(guide="none") +
  scale_shape_identity(guide="none")+
  scale_size_area(max_size=8, guide="none")

ggsave("output/rain_droplets_party.pdf",width=16, height=16, device=cairo_pdf)


rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
rot(pi)
scale_me = function(s) matrix(c(s,0,0,s),2,2)
scale_me(3)

scale_geo = function(s,geometry){matrix(c(s,0,0,s),2,2)*st_geometry(geometry)}

## Affine Transformation

test_sf2 <- bind_cols(test_sf %>% mutate(geometry2=st_centroid(geometry)),
                      packed_coords$layout %>% st_as_sf(coords=c("x","y"))) 
test_sf2 <-test_sf2 %>% janitor::clean_names()



packed_coords$layout %>%
  ggplot() +
  geom_circle(aes(x0=x,y0=y,r=radius)) +
  geom_path(aes(x=x,y=y,group=1)) +
  coord_fixed()

test_sf2 %>%
  ggplot() +
  geom_sf(aes(geometry=(geometry_3)*scale_me(1.2)+geometry_8))


test_sf %>%
  ggplot() +
  geom_sf(fill="#ffffff00",aes(color=as.numeric(area)/pi))
