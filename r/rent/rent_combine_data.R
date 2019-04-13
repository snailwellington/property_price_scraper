

library(tidyverse)

# import function
import_data_func <- function(filename){
  
  loc <- paste0("results/rent/",filename)
  
  tmp_file <- readr::read_csv2(loc, locale = locale(encoding = "windows-1252")) %>% 
    nest()
  return(tmp_file)
}



data_files <- list.files("results/rent/", pattern = "*.csv") %>% as.data.frame()

names(data_files) <- c("url")

data_files <- data_files %>% 
  mutate(change_time = file.mtime(paste0("results/rent/",url)))



if (file.exists("data/rent/comb_data.RDS") != TRUE){
  
  tmp <- data_files %>% 
    as.data.frame() %>% 
    select(-change_time)
  
  colnames(tmp) <- c("query_result")
  
  result <- tmp %>%
    mutate(price_data = map(.x = as.character(query_result), .f = import_data_func))
  
  
  
  saveRDS(result,"data/rent/comb_data.RDS")
} else {
  result <- readRDS("data/rent/comb_data.RDS")
  
  tmp <- data_files[data_files$change_time == max(data_files$change_time), 1] %>% as.data.frame()
  
  colnames(tmp) <- c("query_result")
  result_d <- tmp %>%  mutate(price_data = map(.x = as.character(query_result), .f = import_data_func))
  
  result <- rbind(result, result_d)
  saveRDS(result,"data/rent/comb_data.RDS")
  
}
