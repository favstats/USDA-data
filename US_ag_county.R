rm(list = ls())
#Install if not installed#

install.packages("httr")
install.packages("xml2")
install.packages("rnassqs")
install.packages(c( "jsonlite", "lubridate"))
install.packages("dplyr")
install.packages("tidyr")
install.packages("data.table")
install.packages("sparklyr")
install.packages("gtools")
install.packages("purrr")
install.packages("foreign")
install.packages("plm")
install.packages("xlsx")
install.packages("readxl")
install.packages("stringr")

#Library#
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(sparklyr)
library(gtools)
library(purrr)
library(foreign)
library(plm)
library(plyr)
library(readxl)
library(xlsx)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
#body#




DF_county<- function(CROP, STATE, YEAR){
  
  ## Import quickstats using API request
  
  Single_df<-function(CROP, STATE, YEAR){
    
    d.frame <- fromJSON(paste(c('http://quickstats.nass.usda.gov/api/api_GET/?key=   API KEY   =',CROP,'&year=',YEAR,'&state_alpha=',STATE,'&format=JSON'),collapse=''))
    df<- dplyr::bind_rows(d.frame)
    df<- spread(df, statisticcat_desc, Value)
    
    return(df)
  }
  
  ## Combine multiple years, states, crops
  
  Multi_df<-function(CROP, STATE, YEAR){result<-mapply(Single_df,CROP,STATE, YEAR) %>% bind_rows() %>% select(.,"commodity_desc","agg_level_desc","state_alpha","short_desc","unit_desc","source_desc","county_name","county_code","year","AREA HARVESTED","AREA PLANTED","PRICE RECEIVED","PRODUCTION","SALES","YIELD") %>%filter(agg_level_desc=="COUNTY")
  return(result)
  }
  
  ##Clean and manipulate data 
  
  DF<-Multi_df(CROP,STATE,YEAR)
  DF[DF == "(D)"] <- NA
  DF[DF == "(L)"] <- NA
  
  
  DF<- DF %>% select(.,"commodity_desc","state_alpha","county_name","county_code","year","AREA HARVESTED","AREA PLANTED","PRICE RECEIVED","PRODUCTION","SALES","YIELD")
  DF[,-c(1,2,3,5)] <- as.numeric(gsub(",", "", as.matrix(DF[,-c(1,2,3,5)])))
  DF[, 6:11] <- sapply(DF[, 6:11], as.numeric)
  
  DF<- DF %>% group_by(commodity_desc,county_code,year,state_alpha,county_name) %>% summarise_all(funs(sum(., na.rm=TRUE)))
  DF[DF == 0] <- NA
  
  ##Prepare state level data (espeicially, price Here)
  
  State_df<-function(CROP,STATE,YEAR){result<-mapply(Single_df,CROP,STATE,YEAR) %>% bind_rows() %>% select(.,"freq_desc","commodity_desc","agg_level_desc","state_alpha","short_desc","unit_desc","source_desc","county_name","county_code","year","AREA HARVESTED","AREA PLANTED","PRICE RECEIVED","PRODUCTION","SALES","YIELD") %>%filter(agg_level_desc=="STATE") %>% filter(freq_desc=="MONTHLY")
  return(result)
  }
  
  #Clean state level data
  
  SDF1<-State_df(crop,s,yr) %>% select(.,"commodity_desc","state_alpha","year","PRICE RECEIVED")
  SDF1[SDF1 == "                 (D)"] <- NA
  SDF1[SDF1 == "                 (L)"] <- NA
  SDF1[SDF1 == "                 (S)"] <- NA
  SDF1[SDF1 == "                (NA)"] <- NA
  
  SDF1[, 4] <- sapply(SDF1[, 4], as.numeric)
  SDF1<- SDF1 %>% group_by(commodity_desc,state_alpha,year) %>% summarise_all(funs(mean(., na.rm=TRUE)))
  
  #Append state lelvel data to county level data
  
  TDF<-merge(DF,SDF1,by=c("commodity_desc","state_alpha","year")) %>% select(-c("PRICE RECEIVED.x","SALES")) %>% rename(., c("PRICE RECEIVED.y" = "PRICE"))
  
  
  ## Import, clean and manipulate county level ERS data
  
  url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.xls?v=43244"
  GET(url, write_disk("unemployment1.xls", overwrite=TRUE))
  df.ers <- read_excel("unemployment1.xls",sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 7)
  df.ers <- df.ers %>% tidyr::gather(key = stat, value = Value, -FIPStxt, -State, -Area_name)
  df.ers1 <- df.ers %>% separate(stat, into = c('stat', 'year'), sep = -4, convert = TRUE)
  df.ers1$stat = substr(df.ers1$stat,1,nchar(df.ers1$stat)-1)
  df.ers1$'Area_name' = substr(df.ers1$'Area_name',1,nchar(df.ers1$'Area_name')-4)
  df.ers1 <- df.ers1 %>% separate('Area_name', into = c('county_name', 'county?'), sep = -6, convert = TRUE) 
  df.ers.county <- df.ers1 %>% filter(df.ers1$`county?`=="County") 
  df.ers.county$county_name <- toupper(df.ers.county$county_name) 
  df.ers.county <- spread(df.ers.county, stat, Value)
  df.ers.county$county_name <- gsub('\\s+', '', df.ers.county$county_name)
  colnames(df.ers.county)[2] <- "state_alpha"
  df.ers.county<- transform(df.ers.county, year = as.character(year))
  
  
  ##Merge ERS data and quickstats data by year and county
  
  agg_data_county<- merge(TDF, df.ers.county, by=c("state_alpha","year","county_name"),all.x=TRUE)
  
  return(agg_data_county)
}

## Example
crop<-c("BEANS","CORN")
s<-c("CA","KS")
yr<-2013:2014

df.example<- DF_county(crop,s,yr)

