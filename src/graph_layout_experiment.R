library(tidygraph)
library(see)
library(ggthemes)
library(tidyverse)
library(ggraph)
library(scales)


rep_me <- function(x){
  rep(x,x)
}

col <- str_c("#",str_split("264653-2a9d8f-e9c46a-f4a261-e76f51","-") %>%
               unlist() )

my_df <- tibble(
  from = c(3:45) %>% map(rep_me) %>% unlist()
) %>% group_by(from) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  mutate(to = max(from)*from+idx) %>%
  mutate(origin=0)

my_edges <- bind_rows(my_df %>% select(from=origin,to=from) , 
                      my_df %>% select(from=from,to=to)) %>%
  unique()


g <-as_tbl_graph(my_edges %>% select(from,to)) %>%
  mutate(idx=row_number()) 


g %>% ggraph("tree", circular=T) +
  geom_edge_bend(aes(edge_color=.N()$idx[to], edge_width=.N()$idx[from]),n=3) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() +
  scale_edge_width_continuous(range=c(0.3,0.1), guide="none") +
  coord_fixed() 

ggsave("output/graph_layout_tree.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("matrix") +
  geom_edge_diagonal(aes(edge_color=.N()$idx[to], edge_width=.N()$idx[from])) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() +
  scale_edge_width_continuous(range=c(0.3,0.1), guide="none") 

ggsave("output/graph_layout_matrix.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("sugiyama") +
  geom_edge_link(aes(edge_color=.N()$idx[to], edge_width=.N()$idx[from]),n=3) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() +
  scale_edge_width_continuous(range=c(0.3,0.1), guide="none") 

ggsave("output/graph_layout_sugiyama.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("fr") +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]<100)), edge_width=0.1) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>100)), edge_width=3) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() 

ggsave("output/graph_layout_fr.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("kk") +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]<100)), edge_width=0.1) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>100)), edge_width=3) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() 

ggsave("output/graph_layout_kk.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("star") +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]<100)), edge_width=0.3) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>100)), edge_width=0.1) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() 

ggsave("output/graph_layout_star.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("dendrogram", circular=T) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]<100)), edge_width=0.1) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>100)), edge_width=0.1) +
  #geom_node_point(aes(color=idx)) +
  scale_color_gradientn(colors=col,guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() 

ggsave("output/graph_layout_dendro.pdf", width=16, height=16, device=cairo_pdf)


g %>% ggraph("partition", circular=T) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]<100)), edge_width=0.1) +
  geom_edge_bend(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>100)), edge_width=0.1) +
  geom_node_point(aes(color=idx),shape=18,alpha=0.3) +
  scale_color_gradientn(colors=c(col,rev(col)),guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() 

ggsave("output/graph_layout_partition.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("circlepack", circular=T) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]==0)), edge_width=0.1) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]<=45)), edge_width=0.3) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>=46)), edge_width=3) +
  geom_node_point(aes(filter=idx<=45,size=idx), alpha=0.3) +
  scale_color_gradientn(colors=c(col,rev(col)),guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  theme_void() 

ggsave("output/graph_layout_circlepack.pdf", width=16, height=16, device=cairo_pdf)


g %>% ggraph("circlepack", circular=T) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]==0)), edge_width=0.1) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]<=45)), edge_width=0.3) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>=46)), edge_width=3) +
  geom_node_point(aes(filter=idx<=45,size=idx), alpha=0.3) +
  scale_color_gradientn(colors=c(col,rev(col)),guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  scale_size_continuous(range=c(3,8), guide="none") +
  theme_void() 

ggsave("output/graph_layout_circlepack2.pdf", width=16, height=16, device=cairo_pdf)

g %>% ggraph("kk") +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]==0)), edge_width=0.1) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]<=45)), edge_width=0.3) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>=46)), edge_width=3) +
  scale_color_gradientn(colors=c(col,rev(col)),guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  scale_size_continuous(range=c(3,6), guide="none") +
  theme_void() 

ggsave("output/graph_layout_kk2.pdf", width=16, height=16, device=cairo_pdf)


g %>% ggraph("grid") +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]==0)), edge_width=0.1) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[from]<=45)), edge_width=0.3) +
  geom_edge_link(aes(edge_color=.N()$idx[to],
                     filter=(.N()$idx[to]>=46)), edge_width=3) +
  scale_color_gradientn(colors=c(col,rev(col)),guide="none") +
  scale_edge_color_gradientn(colors=c(col,rev(col)), guide="none") +
  scale_size_continuous(range=c(3,6), guide="none") +
  theme_void() 

