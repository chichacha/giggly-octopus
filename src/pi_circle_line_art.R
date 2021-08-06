

## Here are packages I'm going to use.
library(tidyverse)  
library(circlize)
library(see)
library(ggraph)
library(tidygraph)
library(scales)
library(ggforce)


x <- read_lines("https://www.angio.net/pi/digits/10000.txt")


pi_txt <- x %>% str_split("") %>% unlist()
head(pi_txt)

pos_df <-tibble(
  t = seq(pi,-pi,length.out=11)[-1] 
) %>% mutate(t = t-pi/5,
             x=cos(t),
             y=sin(t),
             idx = c(0:9)) 
  
test %>%
  ggplot(aes(x=x,y=y)) +
  geom_text(aes(label=idx)) +
  coord_fixed() +
  theme_void()


pi_df <- tibble(
  from = pi_txt[-2] %>% as.integer()
) %>% mutate(
             to = lead(from),
             pos = row_number()) %>%
  inner_join(pos_df %>% select(from=idx,from_t=t)) %>%
  inner_join(pos_df %>% select(to=idx,to_t=t))
  
pi_df_sub <- pi_df %>%  
  filter(pos<=2000) %>%
  mutate(difs = from-to) %>%
  group_by(from) %>%
  mutate(from_grp_idx = rescale(row_number()/n(),to=c(0,pi/5-pi/50))) %>%
  ungroup() %>%
  group_by(to) %>%
  mutate(to_grp_idx = rescale(row_number()/n(),to=c(0,pi/5-pi/50))) %>%
  ungroup() %>%
  mutate(from_t_adj = from_t+from_grp_idx,
         to_t_adj = to_t+to_grp_idx) %>%
  mutate(from_x = cos(from_t_adj),
         from_y = sin(from_t_adj),
         to_x = cos(to_t_adj),
         to_y = sin(to_t_adj)) %>%
  mutate(line_grp=ifelse(abs(difs)>5L,10-abs(difs),abs(difs)))



mat10 <- colorRampPalette(material_colors(1:15) %>% as.vector())(10)

pi_df_sub %>% filter(difs==0)

pi_df_sub %>%
  filter(pos<=2000) %>%
  ggplot(aes(x=from_x,y=from_y,color=factor(from))) +
  geom_segment(aes(xend=to_x,yend=to_y),size=0.5,
               data = . %>% filter(line_grp==5)) +
  geom_segment(aes(xend=to_x,yend=to_y),size=0.5,
               data = . %>% filter(line_grp==4)) +
  geom_segment(aes(xend=to_x,yend=to_y),size=0.7,
               data = . %>% filter(line_grp==3)) +
  geom_segment(aes(xend=to_x,yend=to_y),size=1,
               data = . %>% filter(line_grp==2)) +
  ## So that I know where to group the layers
  #annotate(geom="rect",xmax=1,ymax=1,xmin=-1,ymin=-1, fill="#000000de") +
  geom_curve(aes(xend=to_x,yend=to_y), size=1, curvature =0.5 ,
             data=. %>% filter(difs %in% c(-1,9))) +
  geom_curve(aes(xend=to_x,yend=to_y), size=1, curvature=-0.5, 
             data=. %>% filter(difs %in% c(1,-9))) +
  coord_fixed() +
  scale_color_manual(values=mat10, guide="none") +
  scale_fill_manual(values=mat10, guide="none") +
  theme_void() +
  facet_wrap(~line_grp)

ggsave("output/LinePi_Art_Facet.pdf", width=16*3, height=16*2, device=cairo_pdf)



pi_df %>%
  count(from,to,sort=T) %>%
  ggplot(aes(x=from,y=n)) +
  geom_col(aes(fill=factor(to))) +
  coord_polar() +
  scale_fill_material()

pi_e <-pi_df %>%
  count(from,to,sort=T)

pi_g <- as_tbl_graph(pi_e)

pi_g



material_colors()

pi_g %>%
  ggraph("fr") +
  geom_edge_diagonal(aes(color=.N()$name[from])) +
  geom_node_point(aes(color=name)) +
  coord_fixed() +
  scale_edge_color_manual(values=mat10) +
  scale_color_manual(values=mat10)


names(mat10) <- c(0:9)
mat10

pi_df_1k <- pi_df %>% filter(pos<1500) %>% as.data.frame()

?cairo_pdf
cairo_pdf(filename="output/Test.pdf", width=16, height=16)
circos.par(start.degree = 90 )
chordDiagram(pi_df_1k %>% select(from,to),
             order = c(0:9),
             grid.col=mat10, 
             annotationTrack=c("grid","name"),
             directional = 1,
             self.link=1)
circos.clear()
dev.off()






