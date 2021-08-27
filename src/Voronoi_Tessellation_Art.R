

library(tidyverse)
library(ambient)
library(see)
library(ggforce)
library(packcircles)


my_col <- str_c("#",c("001219","005f73","0a9396","94d2bd","e9d8a6","ee9b00","ca6702","bb3e03","ae2012","9b2226"))
my_col %>% scales::show_col()

create_df <- function(a,b){
  my_seeds <- tibble(
    x = round(rbeta(200,a,1),1),
    y = round(rbeta(200,1,b),1),
    z = sample(my_col[1:8],200,prob=c(1,2,3,5,7,13,21,34),replace=T)
  )
  return(my_seeds %>% group_by(x,y) %>% mutate(z=first(z)))
}

my_grid <- tibble(
  a = c(1:3),
  b = c(1:3)
) %>% expand.grid() %>%
  mutate(group = str_c(a,b))

my_grid_long <- my_grid %>%
  mutate(df = pmap(list(a,b),create_df)) %>%
  unnest(df)

my_grid_long %>%
  ggplot(aes(x=x,y=y)) +
  geom_voronoi_tile(bound=c(0,1,0,1), aes(fill=z,group=1),color="#ffffff00",
                    radius = unit(2, 'mm'), expand=unit(-0.5,"mm"))+
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  facet_wrap(~group) +
  theme(strip.text= element_blank(),
        panel.spacing = unit(1,"mm"))
?panel.spacing

ggsave("output/test4.pdf", width=16*1.5, height=16*1.5, device=cairo_pdf)

my_seeds %>%
  ggplot(aes(x=x,y=y)) +
  geom_voronoi_tile(bound=c(0,1,0,1), aes(fill=z,group=1),color="#ffffff00",
                    radius = unit(3, 'mm'), expand=unit(-2,"mm"))+
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed() +
  theme_void()


ggsave("output/test.pdf", width=16, height=16, device=cairo_pdf)
