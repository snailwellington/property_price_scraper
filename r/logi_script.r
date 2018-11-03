
conn <- file(paste0("C:/Users// ... your dir ... //folder_checker/log/run_",Sys.Date(),".log"))
sink(conn, append = TRUE)
sink(conn, append = TRUE, type = "message") 

library(RDCOMClient)

