library(tidyverse)
library(ggforce)

pentomino <- c("I", "P", "P", "Y", "Y", "Y", "Y", "V", "V", "V", "I", "P", "P", "X", "Y", "L", "L", "L", "L", "V", "I", "P", "X", "X", "X", "F", "Z", "Z", "L", "V", "I", "T", "W", "X", "F", "F", "F", "Z", "U", "U", "I", "T", "W", "W", "N", "N", "F", "Z", "Z", "U", "T", "T", "T", "W", "W", "N", "N", "N", "U", "U")

# my_color <- str_c("#",c("001219","005f73","0a9396","94d2bd","e9d8a6","ee9b00","ca6702","bb3e03","ae2012","9b2226"))

# blue 
#solid "177e89","084c61","db3a34","ffc857","323031"
my_seed <-str_c("#",c("03045e","023e8a","0077b6","0096c7","00b4d8","48cae4","90e0ef","ade8f4","caf0f8"))
my_probs <- rbeta(length(my_seed),1,2)
col_12 <- sample(my_seed,size=12,replace=T)

my_seed


pentomino_df <- tibble(
  x = rep(c(1:6), each=10),
  y = rep(c(1:10), times=6),
  g = pentomino
)
pentomino_df %>% count(g,sort=T)

pentomino_df %>%
  ggplot(aes(x=y,y=x,fill=g)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_manual(values=colorRampPalette(colors=my_seed)(12)) +
  theme_void()

ggsave("output/pentomino.pdf", width=9, height=9, device=cairo_pdf)


sol1 <- c("IWWFFPPPIYWWFFPPIYAWFBNNIYYXNNNTIYXXXTTTVZCXUDUTVZZZUUULVVVZLLLL") %>%
  str_split(pattern="", simplify=T) %>% t()

sol1 <- c("AUUUZZFBIUNUZFFFINNZZFLVINTLLLLVINTTTVVVIXTYYYYWXXXPPYWWCXPPPWWD") %>%
  str_split(pattern="", simplify=T) %>% t()

sq_grid <- tibble(
  x = rep(c(1:8), each=8),
  y = rep(c(1:8), times=8),
  g = sol1
) %>% 
  mutate(g_factor=factor(g)) %>%
  group_by(g) %>%
  mutate(xyg=mean(x+y+as.numeric(factor(g))),
         xy = mean(x+y)) %>%
  ungroup() %>%
  mutate(g_rank_xyg=dense_rank(xyg),
         g_rank_xy=dense_rank(xy))

sqrt(sqrt(0.5)+sqrt(0.5))

sq_grid %>%
  ggplot(aes(group=g,fill=as.numeric(g_factor))) +
  #geom_tile(aes(x=x,y=y,group=g,fill=as.numeric(factor(g)))) +
  geom_regon(aes(x0=x,y0=y,sides=4,angle=0,r=sqrt(0.5)), color="#ffffff") +
  geom_path(aes(x=x,y=y,group=interaction(x,g), color=as.numeric(g_factor)), size=3) +
  geom_path(aes(x=x,y=y,group=interaction(y,g), color=as.numeric(g_factor)), size=3) +
  coord_fixed() +
  scale_fill_gradientn(colors=rev(my_seed), guide="none") +
  scale_color_gradientn(colors=rev(my_seed), guide="none") +
  theme_void()
  #scale_fill_manual(values=colorRampPalette(colors=my_seed)(16)) 

ggsave("output/pentomino_sq3.pdf", width=9, height=9, device=cairo_pdf)

