library(RODBC)
library(tidyverse)

con <- RODBC::odbcDriverConnect( 'driver={PostgreSQL ODBC Driver(UNICODE)};Uid=postgres;host=localhost;Pwd=Narnia3levant;port=5432;dbname=db_housing_market;trusted_connection=true')

## create table in database
## create database with all regions and all timestamps as separate columns
## city, query_datetime





sqlTables(con)






RODBC::odbcCloseAll()

