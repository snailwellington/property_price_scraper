library(ggplot2)
library(forcats)

plot_data <- readRDS("data/tallinn_data.RDS")


ggplot(plot_data,aes(fct_reorder(region,-count)))+
  geom_histogram(stat = "count")
  


ggplot(subset(plot_data,Tube <= 5),aes(x = fct_reorder(region,-total_price), y = sq_price))+
  facet_wrap(.~as.factor(Tube))+
  geom_boxplot()

  

