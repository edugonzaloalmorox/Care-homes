#################################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# House prices 
#################################################


library(tidyverse)
library(rio)
library(lubridate)
library(purrr)
library(dummies)
library(janitor)
library(haven)
library(readxl)
library(readr)
library(stringr)

# load all the prices 
prices = read_csv("http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv")

variables = c("code", "price_house", "date", "postcode", "property_type","old_new", "duration",
              "address", "address2", "street", "locality", "town_city", "district", "county", "ppd_category", "record_status")
  

names(prices) = variables

write.csv(prices, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/house_prices.csv", row.names = FALSE)

# select relevant prices from  2005 onwards 


prices = prices %>% mutate_at(vars(date), funs(as.Date)) %>% 
  select(date, postcode, price_house) %>% 
  filter(date >= "2005-01-01") %>% 
  mutate(post_code = gsub("[[:blank:]]", "", postcode)) %>%
  filter(!is.na(post_code))



# add information on oslauas - geography

ons = import("data/geography/ONSPD_NOV_2017_UK/Data/ONSPD_NOV_2017_UK.csv")

ons_prices = ons %>%  mutate(post_code = gsub("[[:blank:]]", "", pcd)) %>% 
  select(post_code, oslaua, pcd) %>%
  filter(post_code %in% unique(prices$post_code))

# link information 

prices_complete = left_join(prices, ons_prices, by = "post_code")

prices_complete = prices_complete %>%
  filter(!is.na(oslaua)) %>% # filter those postcodes that do not have district
  select(-pcd)


# summarise the transactions 

library(psych) # for computing geometric mean


prices_mean_oslaua = prices_complete %>%
  mutate(year_transaction =  year(date)) %>%
  group_by(oslaua, year_transaction) %>% 
  summarise(mean_price =  geometric.mean(price_house, na.rm =TRUE)) %>% 
  filter(oslaua != "")


# filter only oslauas from England

england = prices_mean_oslaua %>% 
  filter(str_detect(oslaua, "^E"))

write.csv(england, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/house_prices_oslaua_year.csv", row.names = FALSE)



