

library(rvest)
library(jsonlite)
library(urltools)

## This will get list of all ModelNo listed on Fredhopper 
get_mods <- function(country,lang,...){

   country <- country
   lang <- lang
   #domain <- "https://outlet.arcteryx.com"
  # country <- "fi"
  # lang <- "en"
  
  fh_base_w <- str_c("https://outlet.arcteryx.com/",country,"/",lang,"/api/fredhopper/query?fh_location=//catalog01/en_CA/gender>{womens}&fh_country=",country,"&fh_refview=lister&fh_view_size=all&fh_context_location=%//catalog01%")
  fh_base_m <- str_c("https://outlet.arcteryx.com/",country,"/",lang,"/api/fredhopper/query?fh_location=//catalog01/en_CA/gender>{mens}&fh_country=",country,"&fh_refview=lister&fh_view_size=all&fh_context_location=%//catalog01%")
  
  tmp_w <- fromJSON(fh_base_w)
  tmp_m <- fromJSON(fh_base_m)
  
  tmp_w_att <-tmp_w$universes$universe$`items-section`$items$item[[2]] %>% select(id,attribute)
  tmp_m_att <-tmp_m$universes$universe$`items-section`$items$item[[2]] %>% select(id,attribute)
  tmp_comb <- bind_rows(tmp_w_att,tmp_m_att) %>% unique()
  
  attr_df <- tibble(attr_name = tmp_comb$attribute[[1]]$name,
                    attr_type = tmp_comb$attribute[[1]]$basetype,
                    attr_selected=tmp_comb$attribute[[1]]$selected) %>%
    mutate(idx=row_number())
  
  tmp <- tibble(
    model_no= tmp_comb$id,
    country = country,
    lang= lang,
    detail_df = tmp_comb$attribute %>% map(unnest,cols=c(value)) 
  )
  
  return(tmp)
  
  
}

## This is just a helper function to get nested information out. 
get_prod_detail <- function(df,country){
  
  x <- str_glue("(name|new|revised|slug|subname|short_description|price_{country})")
  #df <- tmp$detail[[1]]
  #df %>% count(name, selected) %>% filter(n==1) %>% pull(name)
  x_tmp <-df %>% filter(str_detect(name,x)) %>%
    select(name,value) %>% pivot_wider(names_from=name, values_from=value) 
  x_tmp_list <- df %>% filter(str_detect(name,"collections|activities|color_mapping|subcategories|imageurl|secondid")) %>%
    select(name,value) %>% pivot_wider(names_from=name,values_from=value,values_fn=list)
  return(bind_cols(x_tmp,x_tmp_list))
}

## Example way to run the function
us_mod <- get_mods("us","en") %>% 
  mutate(df2 = map2(detail_df,country,get_prod_detail)) %>%
  unnest_wider(col=df2)

ca_mod <- get_mods("ca","en") %>% 
  mutate(df2 = map2(detail_df,country,get_prod_detail)) %>%
  unnest_wider(col=df2)

gb_mod <- get_mods("gb","en") %>% 
  mutate(df2 = map2(detail_df,country,get_prod_detail)) %>%
  unnest_wider(col=df2)

us_second_id <-us_mod %>% pull(secondid) %>% unlist() %>% str_split("_", simplify=T) %>%
  as_tibble() %>%
  filter(V2!="") %>%
  rename(model_no=V1,color=V2,size=V3) %>%
  left_join(us_mod %>% select(model_no,name,price=price_us,slug,country,lang)) %>%
  mutate(slug = str_replace(slug,"shop","us/en/shop"))


ca_second_id <-ca_mod %>% pull(secondid) %>% unlist() %>% str_split("_", simplify=T) %>%
  as_tibble() %>%
  filter(V2!="")%>%
  rename(model_no=V1,color=V2,size=V3) %>%
  left_join(ca_mod %>% select(model_no,name,price=price_ca,slug,country,lang)) %>%
  mutate(slug = str_replace(slug,"shop","ca/en/shop"))


gb_second_id <-gb_mod %>% pull(secondid) %>% unlist() %>% str_split("_", simplify=T) %>%
  as_tibble() %>%
  filter(V2!="") %>%
  rename(model_no=V1,color=V2,size=V3) %>%
  left_join(gb_mod %>% select(model_no,name,price=price_gb,slug,country,lang)) %>%
  mutate(slug = str_replace(slug,"shop","gb/en/shop"))


us_second_id %>% count(model_no,slug,price,sort=T)
ca_second_id %>% count(model_no,slug,price,sort=T)
gb_second_id %>% count(model_no,slug,price,,sort=T)


library(UpSetR)


fromList(list(us=us_second_id %>% mutate(article=str_c(model_no,color,sep="_")) %>% pull(article) %>% unique(),
              ca=ca_second_id %>% mutate(article=str_c(model_no,color,sep="_")) %>% pull(article) %>% unique(),
              gb=gb_second_id %>% mutate(article=str_c(model_no,color,sep="_")) %>% pull(article) %>% unique())) %>% upset(order.by="freq")

?upset




us_mod <- get_mods("us","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2) 
ca_mod <- get_mods("ca","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
gb_mod <- get_mods("gb","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
jp_mod <- get_mods("jp","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
fi_mod <- get_mods("fi","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
se_mod <- get_mods("se","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
pl_mod <- get_mods("pl","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
no_mod <- get_mods("no","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)
de_mod <- get_mods("de","en") %>% mutate(df2 = map(detail_df,get_prod_detail)) %>% unnest_wider(col=df2)

names(po_mod)

se_mod %>% select(-detail_df)
us_mod %>% select(-detail_df)

test<-bind_rows(us_mod %>% rename(price=price_us),
                ca_mod %>% rename(price=price_ca),
                gb_mod %>% rename(price=price_gb),
                fi_mod %>% rename(price=price_fi),
                no_mod %>% rename(price=price_no),
                pl_mod %>% rename(price=price_pl),
                de_mod %>% rename(price=price_de))
test

test %>% 
  mutate(model_no = fct_reorder(model_no,name,min)) %>%
  select(model_no,name,country) %>%
  count(model_no,name,country) %>%
  add_count(model_no,name="nn") %>%
  filter(nn<7) %>%
  ggplot(aes(x=model_no,y=fct_infreq(country))) +
  geom_tile(aes(fill=name)) +
  geom_text(aes(label=name), family="Roboto Condensed") +
  coord_flip() +
  scale_fill_material(guide="none") +
  theme_abyss()


listviewer::jsonedit(tmp)

tmp_comb$attribute[[1]]$name 
tmp_comb$attribute[[1]]$value

tmp_comb$attribute%>% listviewer::jsonedit()


test_url <- "fh_view_size=all&fh_country=ca&fh_location=//catalog01/en_CA/gender>{womens}&fh_sort=-review_count&fh_refview=lister&fh_secondid=24111&fh_lister_pos=0&fh_modification=c7e3000d03d14104abb5bedfb21eea6f"

tmp <- fromJSON(str_c(fh_base,test_url))
listviewer::jsonedit(tmp)

url_decode(base_url)
url_decode("https://arcteryx.com/fi/en/api/fredhopper/query?fh_location=%2F%2Fcatalog01%2Fen_CA%2Fgender%3E%7Bwomens%7D&fh_country=fi&fh_refview=lister&fh_view_size=all&fh_context_location=%25%2F%2Fcatalog01%25")
url_encode(base_url)

test <- fromJSON(base_url)
items <- test$universes$universe$`items-section`$items$item[[2]]

items$link[[1]]
