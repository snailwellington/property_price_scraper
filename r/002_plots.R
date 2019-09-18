library(ggplot2)
library(forcats)
# 
# plot_data <- readRDS("data/tallinn_data.RDS")
# 
# 
# ggplot(plot_data,aes(fct_reorder(region,-count)))+
#   geom_histogram(stat = "count")
#   
# 
# 
# ggplot(subset(plot_data,Tube <= 5),aes(x = fct_reorder(region,-total_price), y = sq_price))+
#   facet_wrap(.~as.factor(Tube))+
#   geom_boxplot()

  

library(tidyverse)

options(scipen = 999)
getwd()



result <- readRDS("data/comb_data.rds")
  
unnest_result <- result %>%   
  group_by(query_result) %>% 
  unnest() %>% 
  unnest() %>% 
  separate(query_result, c("del1","date"), sep = "kv_posting_data_") %>% 
  select(-del1) %>% 
  mutate(date = as.POSIXct(str_replace(date,pattern = ".csv", replacement = ""), format = "%Y-%m-%d"),
         total_price = total_price/1000) %>% # prices are in thousands of euros
  filter(sq_price < 10000)


add_count <- unnest_result %>% 
  group_by(date) %>% 
  summarise(count = n())

ggplot(add_count,aes(date,count)) +
  geom_line()+
  expand_limits(y=0)

ggsave("output/add_count_line.png", width = 16, height = 9,dpi = 300)

add_count_region <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(count = n())

ggplot(add_count_region,aes(date,count, color = region)) +
  geom_line(size = 1)+
  expand_limits(y=0)

ggsave("output/add_count_region_line.png", width = 16, height = 9,dpi = 300)


tln_groups <- unnest_result %>% 
  # filter(date == max(unnest_result$date)) %>% 
  filter(is.na(Tube) == FALSE & Tube <=5) %>% 
  group_by(date,Tube) %>% 
  summarise(sq_median = median(sq_price, na.rm = TRUE))

ggplot(tln_groups, aes(x = date, y = sq_median, color = as.factor(Tube)))+
  # facet_grid(region~.,scales = "free")+
  geom_line()

ggplot(tln_groups, aes(x = date, y = sq_median, color = as.factor(Tube)))+
  # facet_grid(region~.,scales = "free")+
  geom_line()


tln_reg_price_chg <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(price = median(total_price,na.rm = TRUE),
            sq_price = median(sq_price,na.rm = TRUE))

median_change <- unnest_result %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  group_by(date) %>% 
  summarise(med_price = median(sq_price, na.rm = TRUE))


ggplot(tln_reg_price_chg, aes(x = date, y = sq_price, color = region))+
  geom_line()+
  expand_limits(y=0)

## median asking price
median(median_change$med_price)

# rooms <- unnest_result %>% 
#   group_by(Tube,region) %>% 
#   summarise(sq_price = mean(sq_price,na.rm = TRUE))


ggplot()+
  labs(y = "Median property prices by regions, kEUR",
       color = "Region")+
  geom_smooth(data = tln_reg_price_chg,aes(x = date, y = price))+
  geom_line(data = tln_reg_price_chg,aes(x = date, y = price, color = region),size = 1, alpha = 0.9)+
  ggrepel::geom_label_repel(data = subset(tln_reg_price_chg,date == min(tln_reg_price_chg$date)),
                            (aes(x = date, y = price,label = region)), alpha = 0.7)+
  theme_minimal()+
  theme(text = element_text(size = 30),
        legend.position = "none",
        axis.title.x = element_blank())+
  scale_x_datetime(date_labels = "%b %Y")+
  expand_limits(y=0)

ggsave(filename = "output/region_price_chg.png", width = 16, height = 9,dpi = 300)
  # scale_y_continuous(breaks = seq(100,1000,100), limits = c(100,500))
  


ggplot()+
  labs(y = "Median m2 prices by regions, EUR",
       color = "Region")+
  geom_smooth(data = tln_reg_price_chg,aes(x = date, y = sq_price))+
  geom_line(data = tln_reg_price_chg,aes(x = date, y = sq_price, color = region),size = 1, alpha = 0.9)+
  ggrepel::geom_label_repel(data = subset(tln_reg_price_chg,date == min(tln_reg_price_chg$date)),
                            (aes(x = date, y = sq_price,label = region)), alpha = 0.7)+
  theme_minimal()+
  theme(text = element_text(size = 30),
        legend.position = "none",
        axis.title.x = element_blank())+
  scale_x_datetime(date_labels = "%b %Y")+
  expand_limits(y=1500)+
  scale_y_continuous(breaks = seq(0,4000,250))

ggsave(filename = "output/region_m2_price_chg.png", width = 16, height = 9,dpi = 300)
  

tln_reg_total_val <- unnest_result %>% 
  group_by(date,region) %>% 
  summarise(value = sum(total_price,na.rm = TRUE)/1000) %>% 
  ggplot(aes(x = date, y = value))+
  geom_line(size = 1, alpha = 0.9,aes(color = region))+
  geom_smooth()+
  labs(y = "Region property total value, MEUR",
       color = "Region")+
  theme_minimal()+
  theme(text = element_text(size = 30),
        legend.position = "top",
        axis.title.x = element_blank())+
  scale_x_datetime(date_labels = "%b %Y")+
  expand_limits(y=0)

tln_reg_total_val    

ggsave(filename = "output/region_total_value_chg.png", width = 16, height = 9, dpi = 300)

tln_mean_price <- unnest_result %>% 
  group_by(date) %>% 
  summarise(mean_price = median(total_price,na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean_price))+
  geom_line()+
  # scale_y_continuous(limits = c(0,200))+
  expand_limits(y=0)


tln_mean_price
ggsave(filename = "output/median_price_tallinn.png", width = 16, height = 9, dpi = 300)

ggplot(data = subset(unnest_result,date == max(unnest_result$date)), aes(x = total_price, fill = region))+
  geom_histogram(alpha = 0.5, bins = 50)+
  labs(x = paste("Price from ads on",max(unnest_result$date)," in kEUR"),
       y = "Number of offers",
       fill = "Region")+
  # facet_wrap(~region, scales = "free")+
  theme_minimal()+
  theme(text = element_text(size = 16))+
  scale_x_continuous(limits = c(0,1000))+
  expand_limits(y=0)

ggsave(filename = "output/region_price_dist.png", width = 16, height = 9, dpi = 300)

