library(ggplot2)
library(forcats)

# plot_data <- readRDS("data/rent/tallinn_data_2018-11-03.RDS")
# 
# 
# ggplot(plot_data,aes(fct_reorder(region,-count)))+
#   geom_histogram(stat = "count")
# 
# 
# ggplot(subset(plot_data,Tube <= 5),aes(x = fct_reorder(region,-total_price), y = sq_price))+
#   facet_wrap(.~as.factor(Tube))+
#   geom_boxplot()



library(tidyverse)

options(scipen = 999)


result <- readRDS("data/rent/comb_data.RDS")

unnest_result <- result %>%   
  group_by(query_result) %>% 
  unnest() %>% 
  unnest() %>% 
  separate(query_result, c("del1","date"), sep = "kv_posting_data_") %>% 
  select(-del1) %>% 
  mutate(date = as.POSIXct(str_replace(date,pattern = ".csv", replacement = ""), format = "%Y-%m-%d"),
         total_price = total_price) %>% 
  filter(total_price < 10000)

tln_reg_price_chg <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(price = median(total_price,na.rm = TRUE))

ggplot()+
  labs(y = "Mean rental prices by regions, EUR",
       color = "Region")+
  geom_line(data = tln_reg_price_chg,aes(x = date, y = price, color = region),size = 1, alpha = 0.9)+
  ggrepel::geom_label_repel(data = subset(tln_reg_price_chg,date == max(tln_reg_price_chg$date)),
                            (aes(x = date, y = price,label = paste(region,round(price,0), "EUR"))), alpha = 0.7)+
  theme_minimal()+
  theme(text = element_text(size = 30),
        legend.position = "none",
        axis.title.x = element_blank())+
  scale_x_datetime(date_labels = "%b %Y")

ggsave(filename = "output/rent/region_price_chg.png", width = 16, height = 9,dpi = 300)
# scale_y_continuous(breaks = seq(100,1000,100), limits = c(100,500))



tln_reg_total_val <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(value = sum(total_price,na.rm = TRUE)/1000) %>% 
  ggplot(aes(x = date, y = value, color = region))+
  geom_line(size = 1, alpha = 0.9)+
  labs(y = "Region total rental value, kEUR",
       color = "Region")+
  theme_minimal()+
  theme(text = element_text(size = 30),
        legend.position = "top",
        axis.title.x = element_blank())+
  scale_x_datetime(date_labels = "%b %Y")

tln_reg_total_val    

ggsave(filename = "output/rent/region_total_value_chg.png", width = 16, height = 9, dpi = 300)

tln_mean_price <- unnest_result %>% 
  group_by(date) %>% 
  summarise(mean_price = mean(total_price,na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean_price))+
  geom_line()

tln_mean_price
ggsave(filename = "output/rent/median_price_tallinn.png", width = 16, height = 9, dpi = 300)


ggplot(data = subset(unnest_result,date == max(unnest_result$date)), aes(x = total_price, fill = region))+
  geom_histogram(alpha = 0.5, bins = 30)+
  labs(x = paste("Price from ads on",max(unnest_result$date)),
       y = "Number of offers",
       fill = "Region")+
  # facet_wrap(~region, scales = "free")+
  theme_minimal()+
  theme(text = element_text(size = 16))+
  scale_x_continuous(limits = c(0,1000))

ggsave(filename = "output/rent/region_price_dist.png", width = 16, height = 9, dpi = 300)

