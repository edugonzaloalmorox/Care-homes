# Link CQC to house prices

# created: 3/10/2016
# modified: 4/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: link house prices to information associated with geographical registry
#       link throughout postal codes and LSOA and MSOA codes
#       create house prices at LSOA and MSOA level
#       use geolocated CQC dataset and data from the Land Registry
#       
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
                
            # drop observations where there is not postcode ( " ") and where there is not geographical information.  
             
                # I do this first with the prices.2010 datast 
                
                posts = unique(prices2010.clean$post2)

                post.coords = with(coords, unique(post2))
                
                apestados = setdiff(posts, post.coords)
              # ---------------------------------------------------
                
                coords.trans = coords %>% filter(post2 %in% posts) 
                prices2010.clean = prices2010 %>% filter(!(post2 %in% apestados))
                
                
            
              geo.house.prices = left_join(prices2010.clean, coords.trans, by = "post2")
              
              write.csv(geo.house.prices, "house.transactions.geolocated.csv", row.names = FALSE) # house prices that are geolocated 
              
                
# -----------------------------------
# Select variables for the analysis #   
# -----------------------------------
              
          # Select the variables that I am going to include in the CQC 
              
              house.prices = geo.house.prices %>% select(trans.date, price, postcode, post2, city, lsoa11, 
                                                         msoa11, oa11, imd, long, lat)
              
              # Get average prices per year and LSOA, MSOA, OA 
              
              house.prices$year.trans = format(as.Date(house.prices$trans.date, "%m/%d/%Y %H:%M"), "%Y")
              
             
              # LSOA level 
              # --------
                      
              year.house.prices.lsoa = house.prices %>% group_by(lsoa11, year.trans) %>% 
                        summarise (mean_price= mean(price), max_price = max(price), min_price = min(price), n.transactions = n()) %>% arrange(lsoa11, year.trans)
                      
                      # checks
                      # .............................................
                      check = house.prices %>% filter(lsoa11 == " ")
                      # WF17 1AA postcode no longer in use : drop it  
                      
                      # .............................................
                      
                      year.house.prices.lsoa = year.house.prices.lsoa %>% filter(lsoa11 != " ")
                      
                      write.csv(year.house.prices.lsoa, "year.house.prices.lsoa.csv", row.names = FALSE) # mean house prices per year and LSOA
                      
              # MSOA level 
              # ----------
                      
                      year.house.prices.msoa = house.prices %>% group_by(msoa11, year.trans) %>% 
                        summarise (mean_price= mean(price), max_price = max(price), min_price = min(price), n.transactions = n()) %>% arrange(msoa11, year.trans)
                      
                      
                      year.house.prices.msoa = year.house.prices.msoa %>% filter(msoa11 != " ")
              
                      write.csv(year.house.prices.msoa, "year.house.prices.msoa.csv", row.names = FALSE) # mean house prices per year and MSOA
              
# --------------------------              
# Link CQC and house prices           
# -------------------------- 
            # I use CQC geolocated 
            # I create two datasets; one with LSOA and MSOA levels
                      
    # Load datasets
                      cqc = import("cqc.geolocated.csv")  # this is the cqc geolocated (use this dataset as a reference for other issues)
                      house.prices.lsoa = import("year.house.prices.lsoa.csv")  
                      house.prices.msoa = import("year.house.prices.msoa.csv")              
                                                  
    
      # Extract the year in CQC 
      #             This is referred to the time span of each period (a year)
      #             Date to consider: start location (not start provider)
      #             Create 'year.entry': is the variable of the year in the location
      #             Note: this variable may change if the panel has different time interval
      # ------------------------------------------------------------------------------------
        
            cqc = cqc %>% select(location.id:postcode2, oa11:msoa11, lat, long, imd) %>%
                        mutate_each(funs(as.Date), date)
                      
                      
            # Extract the year in the 'date' variable
            cqc = cqc %>% mutate(year.entry = format(date, "%Y"))
            
            cqc = cqc %>% mutate(year.entry = ifelse(registry == "location.start", year.entry, NA)) %>%
              select(location.id:date, year.entry, reg:imd)
        
      # Link datasets at lsoa and msoa level  
      
            # check lsoa levels in both datasets
            # ------------------------------------
             lsoa.house = with(house.prices.lsoa, unique(lsoa11)) 
             lsoa.cqc = with(cqc, unique(lsoa11))
             
             setdiff(lsoa.cqc, lsoa.house)
            # there are no differences [character 0]- all lsoas from cqc are contained in house prices
            # -------------------------------------
             
             
             
         house.prices.lsoa = house.prices.lsoa %>%
           rename(lsoa.mean.price = mean_price, lsoa.max.price = max_price,
                  lsoa.min.price = min_price, lsoa.house.transactions = n.transactions)
        

        cqc.prices.lsoa = left_join(cqc, house.prices.lsoa, by = c("lsoa11" = "lsoa11", "year.entry" = "year.trans"))
        
        cqc.prices.msoa = left_join(cqc, house.prices.msoa, by = c("msoa11" = "msoa11", "year.entry" = "year.trans"))
        
        
        
        write.csv(cqc.prices.lsoa, "cqc.prices.lsoa.csv", row.names = FALSE)
              
              
              
              
                