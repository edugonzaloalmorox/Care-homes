# Information regarding house prices of the Land Registry
# Assess the duplicates and the different information from LR

#         Prices: Price paid data: Accessed 22/05/2017
#         Postal codes: Postalcode directory (ONS): Accessed 23/05/3016



# libraries for loading data
library(rio)

# libraries for manipulating data
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

# libraries for dates 
library(lubridate)

# libraries for plotting 
library(ggplot2)

# libraries for summary statistics

library(psych)

# --------------------------------------------------------------------------------------------------- #

# load data from the LR - only observations from 2012

prices2012 = prices1995 %>% filter(date1 >= "2012-10-01" & date1 < "2016-10-01")
 
write.csv(prices2012, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/prices2012.csv", row.names = FALSE)

prices2012 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/prices2012.csv")


prices2012 = prices2012 %>% mutate_each(funs(as.Date), date1)

# clean empty postcodes

  prices2012 = prices2012 %>% filter(postcode != "")
  

# get rid of duplicates (in case there are any) 
  
  prices2012_unique = unique(prices2012)
  

# nrows that are duplicated
  
  nrow(prices2012) - nrow(prices2012_unique)
  
  #(2057 duplicate rows)
  
 
# create a postcode for linking the information 
  
  # create a variable postcode for linking information       
  prices2012_unique =  prices2012_unique %>% mutate(postcode2 =
                                        gsub(" ", "",postcode))
  
  prices2012_unique= prices2012_unique %>% mutate(postcode2 = str_trim(postcode2, "both"))
  
   
# ------------------------------------------------------------
# Link geographical information referred to local authorities 
# ------------------------------------------------------------
  
  
  geo = import("/Users/Personas/Downloads/ONSPD_FEB_2017_UK/Data/ONSPD_FEB_2017_UK.csv")
  
# create a cleaned postcode (without gaps) for merging data 
  
      geo =  geo %>% mutate(postcode2 =
                              gsub(" ", "",pcd)) %>% select(postcode2, pcd:imd) 
      
      geo = geo %>% mutate(postcode2 = str_trim(postcode2, "both"))
      
# select relevant information: postalcode, oslaua
  
      postgeo = geo %>% select(postcode2, pcd, oslaua)
      
      postgeo = unique(postgeo)
      
  
# link data 
  
      prices_postcodes = unique(prices2012_unique$postcode2)
      
      postgeoprices = postgeo %>% filter(postcode2 %in% prices_postcodes)
      
      geoprices = left_join(prices2012_unique, postgeoprices, by  = "postcode2" )
      
  # note: there are 597 postcodes that are not found in postcode directory (0.015% of the postcode )
  
      check = geoprices %>% filter(is.na(oslaua))
      
      geoprices = geoprices %>% filter(!is.na(oslaua)) # observations without missing oslauas
      
     
   write.csv(geoprices, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/prices2012.csv", row.names = FALSE)
  
  
# ------------------
# Create the waves
# -----------------
   
   
   geoprices= geoprices %>% mutate(wave = ifelse(date1 <= "2013-09-30", 1, ifelse(
     date1 > "2013-09-30" & date1 <= "2014-09-30", 2, ifelse(
       date1 > "2014-09-30" & date1 <= "2015-09-30", 3, ifelse(
         date1 > "2015-09-30", 4, NA)))))
  
   
   prices_england = geoprices %>% filter(grepl("^E", oslaua)) %>% as.data.frame()

   # prices from oslauas in england
    prices_oslaua = unique(geoprices$oslaua)
    oslaua_england = unique(prices_england$oslaua)
  
    setdiff(prices_oslaua, oslaua_england)
  
  # note: 23 oslauas from wales, 1 for scotland, and 1 empty 
  
# ------------------
# Summary of prices
# ------------------
    
    # per month and year 
    sum_prices = prices_england %>% mutate(month = month(date1), year = year(date1), year_month = paste(year, month, sep = "-")) %>%
      group_by(year, month) %>%
      summarise(av.price = mean(price),
                geoavprice = geometric.mean(price),
                min = min(price),
                max = max(price), 
                n = n()) %>%
      arrange(year, month)
    
    sum_prices = sum_prices %>% mutate(year_month = paste(year, month, sep = "-"))# plot the series
    
    write.csv(sum_prices, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/sumprices.csv", row.names = FALSE)
    
    sum_prices = as.data.frame(sum_prices)
    
    # plot series
    prices = ggplot(linked_unique, aes(Period)) + 
      geom_line(aes(y = averagePrice, colour = "averagePrice", group = 1)) + 
      geom_line(aes(y = geoavprice, colour = "geoavprice", group = 1)) +
      geom_line(aes(y = av.price, colour = "av.price", group = 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    prices
    
    
# ---------------------------------------
    
    # per wave and oslaua 
    
    sum_prices_waves = prices_england %>% mutate(month = month(date1), year = year(date1), year_month = paste(year, month, sep = "-")) %>%
      group_by(wave, oslaua) %>%
      summarise(av.price = mean(price),
                geoavprice = geometric.mean(price),
                min = min(price),
                max = max(price), 
                n = n()) %>%
      arrange(oslaua, wave)
    
    sum_prices_waves = as.data.frame(sum_prices_waves)
    
    
    
    

 write.csv(sum_prices_waves, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/sum_prices_waves_oslaua.csv", row.names = FALSE)
    
 
 ##########################
# Assessment of duplicates # 
 ##########################
 
 #-----------------------------------------------------------------------------
 
 # asess the duplicate rows 
 
 dup = geoprices %>% group_by(date1, postcode, price, number, flat) %>% filter(n()>1)
 
 # note: there are 7369 duplicates. Most of them correspond to the same transaction that is defined differently
 
 # ------------------#
 # Assess duplicates #
 #-------------------#
 
 # transactions that are double counted considering various types of property
 check =  prices2012 %>% group_by(date1, postcode, price, number, flat) %>% summarise(n_distinct(type)) %>% rename(types = `n_distinct(type)`)
 
 various_types =  check %>%filter(types >1)
 severaltypes = unique(various_types$postcode) # postcodes of these transaction
 
 
 # transactions that are double counted considering various types of estate type (e.g. freehold leasehold)
 check1 = prices2012 %>% group_by(date1, postcode, price, number, flat) %>% summarise(n_distinct(freehold_leasehold)) 
 various_leases = check1 %>% filter(lease >1) %>% group_by(postcode) %>% unique() %>% select(postcode) %>% as.character()
 severaleases = unique(various_leases$postcode)
 
 dup_posts = unique(dup$postcode)
 
 
 
 # note: most of the duplicates are because the same transaction is registered as a freehold and a leasehold. Also there are few that are considered as different types. 
 #       I don´t consieres errors in the variable A or A.1
 
 # select relevant information 
 
 prices2012_sample = prices2012_unique %>% select(date1:price, new, number:county)
 
 prices2012_sample = unique(prices2012_sample) 
 
 
 # -----------------------#
 # Summary of transactions#
 # -----------------------#
 
 sum_p2012 = prices2012 %>% mutate(month = month(date1), year = year(date1), year_month = paste(year, month, sep = "-")) %>%
   group_by(year, month) %>%
   summarise(av.price = mean(price),
             geoavprice = geometric.mean(price),
             min = min(price),
             max = max(price), 
             n = n()) %>% 
   arrange(year, month)
 
 # get information from the LR to check differences
 
 landregistry = import("/Users/Personas/Downloads/ukhpi-england-and-wales-from-october-2012-to-september-2016.csv")
 
 land = landregistry %>% select(Period:percentageAnnualChange)
 
 # link information for representations
 sum_p2012 = as.data.frame(sum_p2012)
 
 linked_unique = cbind(land, sum_prices)
 
 # Assess differences with the information of the land registry
 
 # prepare data
 transactions_long_unique = linked_unique %>% mutate(difference = SalesVolume - n) %>% select(Period, difference, SalesVolume, n) %>% gather(transactions, value, SalesVolume:n) %>% arrange(Period)
 
 # differences in transactions between my data and the data from the land registry
 transactions_unique = ggplot(transactions_long_unique, aes(x=Period, y=value, fill=transactions))+
   geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 
 transactions_unique
 # representation of differences      
 transactions_diff_unique = ggplot(transactions_long_unique, aes(x=Period, y=difference))+
   geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(title = "Differences transactions (no duplicated)",
        caption = "Negative differences denote more transactions than reported by the LR", 
        x = "Period", y = "difference") 
 
 
 transactions_diff_unique
 
 # prices 
 
 prices_long_unique = linked_unique %>% mutate(difference = averagePrice - geoavprice) %>% select(Period, difference, averagePrice, geoavprice) %>% gather(avprices, value, averagePrice:geoavprice) %>% arrange(Period)
 
 
 prices_unique = ggplot(prices_long_unique, aes(x=Period, y=value, fill=avprices))+
   geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 prices_unique
 
 
 prices_diff_unique = ggplot(prices_long_unique, aes(x=Period, y=difference))+
   geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(title = "Differences transactions (no duplicated)",
        caption = "Negative differences denote more transactions than reported by the LR", 
        x = "Period", y = "difference") 
 
 
 prices_diff_unique
 
 
 prices = ggplot(linked_unique, aes(x=Period, y=av.price))+
   geom_bar(position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(title = "Differences transactions (no duplicated)",
        caption = "Negative differences denote more transactions than reported by the LR", 
        x = "Period", y = "difference") 
 
 
 
 prices
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  