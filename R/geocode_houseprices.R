# Geocode house prices from 2010 
# Big datasets(>3G): I clean first prices and then geographical information
# First version: 1/12/2016
# This version: 1/12/2016
# Edu Gonzalo Almorox 
# ------------------

# ------
# Prices 
# ------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves")

prices_waves = read.csv("http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.txt", sep = ",", header = TRUE)
head(prices_waves)
tail(prices_waves)


vars = c("item", "price",	"date", "postcode",	"property.type", "old/new", "duration",	"PAON",	"SAON",	"street",	"locality",	"city",	"district",	"county",	"PPD","record.status")
colnames(prices_waves) = vars

# clean prices_waves
      prices_waves = prices_waves %>% mutate(date.1 = gsub("00:00", "", date))
      
      prices_waves = prices_waves %>% mutate(date.1 = str_trim(date.1, side = "both")) 
        
      prices_waves = prices_waves %>% mutate_each(funs(as.Date), date.1)

      
      
# create a subsample with transactions from 2010 onwards
      prices2010 = prices_waves %>% filter(date.1 >="2010-01-01") %>% 
        select(price:duration, -date, locality:county, date.1) %>% 
        arrange(date.1)
      
      setwd("/Users/Personas/Documents/research/other/visualizations/house prices/house_prices_england/data /raw")
      write.csv(prices2010, "england.prices.stats.csv", row.names = FALSE)


# -------
# Geocode
# -------
      
      # get info from postocode directory
      
      setwd("/Users/Personas/Downloads/ONSPD_AUG_2016_UK_V2/Data")
      
      onspd = import("onspd_aug16.csv")
      
      # create post2 -> variable that is the postcode without blank spaces (this is going to be created in both prices and the geographical information)
      
      onspd = onspd %>% mutate(post2 = gsub("\\s", "",pcd)) 
      prices2010 = prices2010 %>% mutate(post2 = gsub("\\s","", postcode))
    
      # clean the postcodes that are missing
      
      prices2010 = prices2010 %>%  filter(postcode != "")  # note: data frame that 5513944 observations 
    
      
       # select the codes in the code directory associated with the postcodes in prices
      
      house_post = levels(as.factor(prices2010$post2))
      check <- intersect(house_post, onspd$post2) 
      check.1 <- setdiff(house_post, onspd$post2) 
      # note check: there are 362 postcodes that are not registered in the postcode directory 
      
      
    
      # filter the code directory in terms of the house_post --> aim: select the postcodes corresponding to the transactions that are in the postcode directory
      
      ons_house = onspd %>% filter(post2 %in% house_post) %>% arrange(post2)
      
      str(ons_house)
      str(prices2010)
      
      
      # link datasets throughout 
      
      prices2010c = prices2010 %>% filter(post2 %in% check)
      
      geoprices = left_join(prices2010c, ons_house, by = "post2")
      write.csv(geoprices, "house_prices_geolocated.csv")
      
      
      
      
     
      
      
      
     
      
      
      
      
      