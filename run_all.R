

conn <- file(paste0("D:/Prog/R/Projects/property_price_scraper/logi/run_",Sys.Date(),".log"))
sink(conn, append = TRUE)
sink(conn, append = TRUE, type = "message") 

source("r/kv_scraper.R")
source("r/rent/kv_rent_scraper.R")
