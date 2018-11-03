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
  labs(y = "Mean property prices by regions, k€",
       color = "Region")+
  geom_line(data = tln_reg_price_chg,aes(x = date, y = price, color = region),size = 1, alpha = 0.9)+
  ggrepel::geom_label_repel(data = subset(tln_reg_price_chg,date == min(tln_reg_price_chg$date)),
                            (aes(x = date, y = price,label = region)), alpha = 0.7)+
  theme_minimal()+
  theme(text = element_text(size = 36),
        legend.position = "none",
        axis.title.x = element_blank())+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%d %b %Y")

ggsave(filename = "output/region_price_chg.png", width = 16, height = 9,dpi = 300)
  # scale_y_continuous(breaks = seq(100,1000,100), limits = c(100,500))
  
  

tln_reg_total_val <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(value = sum(total_price,na.rm = TRUE)/1000) %>% 
  ggplot(aes(x = date, y = value, color = region))+
  geom_line(size = 1, alpha = 0.9)+
  labs(y = "Region property total value, M€",
       color = "Region")+
  theme_minimal()+
  theme(text = element_text(size = 36),
        axis.title.x = element_blank())+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%d %b %Y")

tln_reg_total_val    

ggsave(filename = "output/region_total_value_chg.png", width = 16, height = 9, dpi = 300)

tln_mean_price <- unnest_result %>% 
  group_by(date) %>% 
  summarise(mean_price = mean(total_price,na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean_price))+
  geom_line()

tln_mean_price

ggplot(data = subset(unnest_result,date = max(unnest_result$date)), aes(x = total_price, fill = region))+
  geom_histogram(alpha = 0.5, bins = 30)+
  labs(x = "Price from adds",
       y = "Number of offers",
       fill = "Region")+
  facet_wrap(~region, scales = "free_x")+
  theme_minimal()+
  theme(text = element_text(size = 36))

ggsave(filename = "output/region_price_dist.png", width = 16, height = 9, dpi = 300)
