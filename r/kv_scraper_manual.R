library(rvest)
library(tidyverse)

time1 <- Sys.time()
total_data <- data.frame()

max_sheet_url <- "https://www.kv.ee/?act=search.simple&deal_type=1&county=1&search_type=new&parish=1061&city%5B0%5D=1001&city%5B1%5D=5701&city%5B2%5D=1003&city%5B3%5D=1004&city%5B4%5D=1006&city%5B5%5D=1007&city%5B6%5D=1008&city%5B7%5D=1010&city%5B8%5D=1011&city%5B9%5D=5700"
max_sheet <- read_html(max_sheet_url) %>% 
  html_node("a.count") %>% 
  html_text() %>% 
  as.numeric()

wait_time <- 5


for (i in 1:max_sheet){
  
  link_url <- paste0("https://www.kv.ee/?act=search.simple&deal_type=1&county=1&search_type=new&parish=1061&city%5B0%5D=1001&city%5B1%5D=5701&city%5B2%5D=1003&city%5B3%5D=1004&city%5B4%5D=1006&city%5B5%5D=1007&city%5B6%5D=1008&city%5B7%5D=1010&city%5B8%5D=1011&city%5B9%5D=5700&page=",
                     i)
  
  html_data <- read_html(link_url)
  
  name_data <- html_data %>% 
    html_nodes(".object-list-table") %>% 
    html_table() %>% 
    as.data.frame() %>% 
    filter(Kirjeldus != "")
  
  total_data <- rbind(total_data,name_data)
  
  Sys.sleep(rnorm(wait_time,mean = wait_time*0.2, sd = 0.01*wait_time))
  print(paste0("Sheet-",i," ",Sys.time()-time1))
}


tallinn_analysis <- total_data %>% 
  select(2:6) %>% 
  separate(Kirjeldus, c("county",
                        "city",
                        "region",
                        "address"), 
           sep = ",", 
           remove = FALSE) %>% 
  separate(region, c("region"), sep = "\n") %>% 
  mutate(region = str_replace(region,"linna","linn")) %>% 
  group_by(region) %>% 
  mutate(count = n()) %>% 
  filter(count > 1) %>% 
  ungroup() %>% 
  mutate(Hind = as.character(Hind)) %>% 
  mutate(Hind = str_remove_all(Hind,"\n"),
         Hind = str_remove_all(Hind,"[[:space:]]")) %>% 
  separate(Hind, c("total_price","sq_price","delete1","no_pics"), sep = "[[€]]") %>% 
  separate(Pind, c("sq_area"),sep = c("[[:space:]]")) %>% 
  select(-delete1, -LISATUD) %>% 
  mutate(sq_area = as.numeric(as.character(sq_area)),
         total_price = as.numeric(as.character(total_price)),
         sq_price = as.numeric(as.character(sq_price)),
         no_pics = as.numeric(as.character(no_pics)))

write.csv2(tallinn_analysis, file = paste0("D:/Prog/R/Projects/property_price_scraper/results/kv_posting_data_",Sys.Date(),".csv"),row.names = FALSE)
saveRDS(object = tallinn_analysis, file = paste0("D:/Prog/R/Projects/property_price_scraper/data/tallinn_data_",Sys.Date(),".RDS"))

print(Sys.time()-time1)

# plot_data <- tallinn_analysis %>% 
#   filter(total_price < 1000000 & sq_area < 500)
# 
# ggplot(plot_data,aes(region,total_price))+
#   geom_boxplot()
# 
# ggplot(plot_data,aes(as.factor(Tube),total_price))+
#   geom_boxplot()
# 
# ggplot(plot_data, aes(total_price, fill = region))+
#   geom_histogram(alpha = .8)
# 
# ggplot(plot_data, aes(x = sq_area, y = sq_price, color = as.factor(Tube)))+
#   geom_point(alpha = 0.6)
# 
# ggplot(plot_data, aes(sq_area, fill = region))+
#   geom_histogram(alpha = 0.6, binwidth = 5)+
#   scale_x_continuous(breaks = seq(0,300,25), limits = c(0,200))
# 
# 
