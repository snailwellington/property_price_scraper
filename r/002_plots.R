library(ggplot2)
library(forcats)

plot_data <- readRDS("data/tallinn_data.RDS")


ggplot(plot_data,aes(fct_reorder(region,-count)))+
  geom_histogram(stat = "count")
  


ggplot(subset(plot_data,Tube <= 5),aes(x = fct_reorder(region,-total_price), y = sq_price))+
  facet_wrap(.~as.factor(Tube))+
  geom_boxplot()

  

library(tidyverse)

options(scipen = 999)

data_files <- list.files("results/", pattern = "*.csv")



import_data_func <- function(filename){
  
  loc <- paste0("results/",filename)
  
  tmp_file <- readr::read_csv2(loc, locale = locale(encoding = "windows-1252")) %>% 
    nest()
  return(tmp_file)
}


test <- data_files %>% 
  as.data.frame()

colnames(test) <- c("query_result")
  
result <- test %>% 
  mutate(price_data = map(.x = as.character(query_result), .f = import_data_func))
  
unnest_result <- result %>%   
  group_by(query_result) %>% 
  unnest() %>% 
  unnest() %>% 
  separate(query_result, c("del1","date"), sep = "kv_posting_data_") %>% 
  select(-del1) %>% 
  mutate(date = as.POSIXct(str_replace(date,pattern = ".csv", replacement = ""), format = "%Y-%m-%d"),
         total_price = total_price/1000) # prices are in thousands of euros

tln_reg_price_chg <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(price = mean(total_price,na.rm = TRUE))

ggplot()+
  labs(y = "Property price, k€",
       color = "Region")+
  geom_line(data = tln_reg_price_chg,aes(x = date, y = price, color = region))+
  ggrepel::geom_label_repel(data = subset(tln_reg_price_chg,date == min(tln_reg_price_chg$date)),
                            (aes(x = date, y = price,label = region)))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%d %b %Y")

  # scale_y_continuous(breaks = seq(100,1000,100), limits = c(100,500))
  
tln_reg_price_chg    

tln_reg_total_val <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(value = sum(total_price,na.rm = TRUE)/100) %>% 
  ggplot(aes(x = date, y = value, color = region))+
  geom_line()+
  labs(y = "Total value, M€")

tln_reg_total_val    



tln_mean_price <- unnest_result %>% 
  group_by(date) %>% 
  summarise(mean_price = mean(total_price,na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean_price))+
  geom_line()

tln_mean_price

