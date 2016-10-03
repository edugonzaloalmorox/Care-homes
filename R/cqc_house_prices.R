# Link CQC to house prices

# created: 2/10/2016
# modified: 3/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: link house prices to information associated with geographical registry
#       link throughout postal codes and LSOA and MSOA codes
#       use geolocated CQC dataset and data from the Land Registry
# -------------------------------------------------------------------------------------------------------------------------



setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(rio)
library(dplyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(lubridate)

# ---------------------------------------------
# Get house prices data from the Land Registry
# --------------------------------------------

# Note: prices as to september 2016
# ---------------------------------
prices = read.csv("http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv", sep = ",", header = TRUE)


vars = c("item", "price",	"date", "postcode",	"property.type", "old/new", "duration",	"PAON",	"SAON",	"street",	"locality",	"city",	"district",	"county",	"PPD","record.status")
colnames(prices) = vars

prices = import("prices.csv")

prices = prices %>% select( -item, -date.1)
  

 library(lubridate)

          # Create a variable 'date.1' to get the date. It removes the time component (as.Date and striptime don´t work in firstly)
          # Need to strip the string 
          # Order according to date - earlier transactions first 
                  prices$date.1 = with(prices, stri_sub(fecha,1,10))
                  
                  prices$date.1 = as.Date(prices$date.1, format = '%Y-%m-%d')
                  prices = prices %>% arrange(date.1) %>% select(trans.date = date.1, price, postcode:record.status)
                  write.csv(prices, "prices.csv", row.names = FALSE)

          # Select a subsample - transactions since 2010

                  prices2010 = prices %>% filter(trans.date >= "2010-01-01")
                
              write.csv(prices2010, "prices2010.csv", row.names = FALSE)

# --------------------------------
# Geolocate transaction postcodes # 
# ---------------------------------
                
            # Load geographical information 
                # Idea: Create a subset of geographical information with those postcodes subject to the transactions 
                # Link the MSOA and LSOA to the prices 
                # Link the geocode transactions to the care homes 
              
                
                coords = read.csv("http://geoportal.statistics.gov.uk/datasets/8f7f8bc2bd7c4960b44261ed36da185b_0.csv", sep = ",", header = TRUE)
                
            # create a postcode without blanks in both datasets 
 
                coords$post2 = with(coords, str_replace_all(pcd, "[[:blank:]]", ""))
                prices2010$post2 = with(prices2010, str_replace_all(postcode, "[[:blank:]]", ""))
                
            # drop observations where there is not postcode and where there is not geographical information.  
             
                # I do this first with the prices.2010 datast 
                posts = unique(prices2010.clean$post2)

                post.coords = with(coords, unique(post2))
                
                apestados = setdiff(posts, post.coords)
              # ---------------------------------------------------
                
                coords.trans = coords %>% filter(post2 %in% posts) 
                prices2010.clean = prices2010 %>% filter(!(post2 %in% apestados))
                
                
              write.csv(coords.trans, "house.transactions.geolocated.csv", row.names = FALSE)
              
              geo.house.prices = left_join(prices2010.clean, coords.trans, by = "post2")
              
              write.csv(geo.house.prices, "house.transactions.geolocated.csv", row.names = FALSE)
                
                
              
                