# Calculate and average house prices 
# Purpose: Calculate mean prices at local planning authority  
# I calculate evertyhing at local planning authority level --> district level (given by oslaua)
# This calculation is based on grouping by districts and waeves 

# created: 5/12/2016
# modified: 6/12/2016


setwd("/Users/Personas/Documents/research/other/visualizations/house prices/house_prices_england/data /raw")

# ----------------
library(pander)
library(rio)
library(dplyr)
library(plm)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(tidyr)
library(plyr)
library(magrittr)
library(ggplot2)
library(pglm)
library(stargazer)
library(lme4)
library(AER)
library(Hmisc)
library(gridExtra)
# -----------------

geoprices = import("house_prices_geolocated.csv")


# create waves

          geoprices = geoprices %>% mutate_each(funs(as.Date), date.1)
           
          geoprices = geoprices %>% mutate(wave = ifelse(date.1 < "2011-03-01",0,
                                                  ifelse(date.1 >= "2011-03-01" & date.1 < "2013-03-01", 1, 
                                                  ifelse(date.1 >= "2013-03-01" & date.1 < "2015-03-01", 2,
                                                  ifelse(date.1 >= "2015-03-01", 3, NA)))))
           
 
 check = geoprices %>% filter(oslaua == "")


# calculate information corresponding to prices 

        sum_prices = geoprices %>% group_by(oslaua, wave) %>% dplyr::summarise(mean_price = mean(price, na.rm =TRUE),
                                                                        max_price = max(price, na.rm = TRUE), 
                                                                        min_price = min(price, na.rm =TRUE),
                                                                        transactions = n())
        # note: WF17 1AA does not have a oslaua code associated with it --> drop 
        
        sum_prices = sum_prices %>% filter(oslaua != "")
        sum_prices$oslaua = droplevels(sum_prices$oslaua)
        
      # elminate prices in Scotland and Wales ("S", "W")  
       sum_prices = sum_prices %>% filter(!grepl("^W", oslaua)) %>% filter(!grepl("^S", oslaua))
       sum_prices$oslaua = droplevels(sum_prices$oslaua)
       
       write.csv(sum_prices, 
                 "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/house_prices_districts_waves.csv", 
                 row.names = FALSE)
   


       
       
       
      