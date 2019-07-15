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

result <- readRDS(here("data","comb_data.RDS"))
  
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
  summarise(price = median(total_price,na.rm = TRUE))

median_change <- unnest_result %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  group_by(date) %>% 
  summarise(med_price = median(sq_price, na.rm = TRUE))


ggplot(median_change, aes(x = date, y = med_price))+
<<<<<<< HEAD
  geom_line()+
  scale_y_continuous(limits = c(0,3000))
=======
  geom_line()
>>>>>>> 2bdabf746df730786ee45b7bfae0407bcd5f2686

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

ggsave(filename = here("output","region_price_chg.png"), width = 16, height = 9,dpi = 300)
  # scale_y_continuous(breaks = seq(100,1000,100), limits = c(100,500))
  
  

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

ggsave(filename = here("output","region_total_value_chg.png"), width = 16, height = 9, dpi = 300)

tln_mean_price <- unnest_result %>% 
  group_by(date) %>% 
  summarise(mean_price = median(total_price,na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean_price))+
  geom_line()+
<<<<<<< HEAD
  scale_y_continuous(limits = c(0,200))
=======
  expand_limits(y=0)
>>>>>>> 2bdabf746df730786ee45b7bfae0407bcd5f2686

tln_mean_price
ggsave(filename = here("output","median_price_tallinn.png"), width = 16, height = 9, dpi = 300)

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

ggsave(filename = here("output","region_price_dist.png"), width = 16, height = 9, dpi = 300)

