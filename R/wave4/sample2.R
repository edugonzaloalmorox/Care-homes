# Sample - yearly data (2014 - 2016)
# 
# Objective: create a sample with the same time frame as the quality ratings
# --------------------------------------------------------------------------


library(dplyr)
library(tidyverse)
library(rio)
library(lubridate)
library(purrr)

# set the time frames considering the quality ratings 
#     - quality: ("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings.csv")
#     - cqc: ("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc_geolocation_waves.csv)



# load data 

cqc_geolocated = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc_geolocation_waves.csv")


cqc_geolocated = cqc_geolocated %>% mutate_each(funs(as.Date), date)


# create waves: 0 before Oct 2014, 1 till Sept 2015 and 2 after that 
# waves are different for entries and exits!

        cqc_geolocated = cqc_geolocated %>% mutate(wave.entry = ifelse(date < "2014-10-01" & registry == "location.start" , 0, 
                                                                       ifelse(date >= "2014-10-01" & registry == "location.start" & date <="2015-09-30" & registry == "location.start", 1, 
                                                                              ifelse(date > "2015-09-30" & registry == "location.start", 2, NA)))) 
        
        
        
        
        cqc_geolocated = cqc_geolocated %>% mutate(wave.exit = ifelse(date < "2014-10-01" & registry == "location.end" , 0, 
                                                                       ifelse(date >= "2014-10-01" & registry == "location.end" & date <="2015-09-30" & registry == "location.end", 1, 
                                                                              ifelse(date > "2015-09-30" & registry == "location.end", 2, NA)))) 
        
        
        
        # select date of analysis October 2014 onwards 
        
        cqc = cqc_geolocated %>% filter(date >= "2014-10-01") %>% arrange(date)
        
        
        droplevels(as.factor(cqc$location.id))

# 1844 care homes

# ---------------------------------------------
# OUTCOME VARIABLE: CARE HOMES PER POPULATION #
# ---------------------------------------------


      cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc.geolocated.csv")
      
      
      cqc = cqc %>% mutate_each(funs(as.Date), date)

# entries 


        novo_entries = cqc %>% filter(registry == "location.start" & reg == "novo")
        
        novo_entries = novo_entries %>% mutate(wave = ifelse(date < "2014-10-01" , 0, 
                                                             ifelse(date >= "2014-10-01"  & date <="2015-09-30", 1, 
                                                                    ifelse(date > "2015-09-30" , 2, NA)))) 
        
        count_novo = novo_entries %>% group_by(oslaua, wave) %>% tally() %>% rename(entries = n) %>% arrange(oslaua, wave)
        
        
        # exits
        
        
        definite_exits = cqc %>% filter(registry == "location.end" & reg == "exit.def")
        
        definite_exits = definite_exits %>%mutate(wave = ifelse(date < "2014-10-01" , 0, 
                                                                ifelse(date >= "2014-10-01"  & date <="2015-09-30", 1, 
                                                                       ifelse(date > "2015-09-30" , 2, NA)))) 
        
        
        count_exits = definite_exits %>% group_by(oslaua, wave) %>% tally() %>% rename(exits = n) %>% arrange(oslaua, wave)


# all the counts
        counts = full_join(count_novo, count_exits, by = c("oslaua", "wave"))
        
        counts = counts %>% arrange(oslaua, wave)

# fill NA with 0 
        counts = counts %>% mutate_all(funs(replace(., is.na(.), 0)))
        
        counts = counts %>% mutate(net_wave = entries - exits)
        
        counts = counts %>% group_by(oslaua) %>% mutate(care_homes = cumsum(net_wave))
        
        # df with all the local authorities
        
        la = levels(as.factor(novo_entries$oslaua))
        la = rep(la, 3)
        y = rep(c(0:2), 325)
        
        df = data.frame(oslaua = la, wave = y) %>% arrange(oslaua, wave) # all the LA's and wave
        
        counts_test = left_join(df, counts, by = c("oslaua", "wave"))

# fill NA with 0 
        counts_test = counts_test %>% mutate_all(funs(replace(., is.na(.), 0)))
        
        counts_test = counts_test %>% group_by(oslaua) %>% mutate(care_homes = cumsum(net_wave), 
                                                                  entry_rate = (entries /lag(care_homes)*100),
                                                                  exit_rate = (exits/lag(care_homes)*100))
        
        counts_test = counts_test %>% filter(wave != 0)
        
        
        test3 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv")
        
        
        test4 =  test3 %>% select(-(entry:exit.rate))
        
        test4 = left_join(test4, counts_test, by = c("oslaua", "wave")) # new dataset
        
        test4 = test4 %>% select(oslaua, wave, entries:exit_rate, sum.bed:rate_inadequate)

# new care homes per population 

        test4 =  test4 %>% mutate(carehomespop = (care_homes/population)*1000, 
                                  Bad = (Inadequate + `Requires improvement`),
                                  badpop = (Bad/population)*1000)
        
        test4 = test4 %>% select(oslaua:`Requires improvement`, Bad, av.price:improvepop, badpop, quantile:rate_inadequate)
        
        write.csv(test4,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv", row.names = 
                    FALSE)

#-------------------------------------------------------------------------------------------
# -----------------------
# BEDS AND AVERAGE SIZE #
# -----------------------

    
# Newly registered beds and average size 

# - sum.bed: sum of beds for each local authority per year
# - av.size: average size of the entrant care homes


      beds = cqc_geolocated %>% group_by(oslaua, wave.entry) %>% filter(flow.entry == "novo") %>%
        mutate(sum.bed = sum(care.homes.beds), av.size = mean(care.homes.beds)) %>%
        select(oslaua, wave.entry, sum.bed, av.size) %>%  unique() %>% arrange(oslaua, wave.entry)
      
      
      df_2014 = left_join(df_2014, beds, by = c("oslaua" = "oslaua", "wave" = "wave.entry"))


      # fill the missing
      df_2014 = df_2014 %>% mutate_all(funs(replace(., is.na(.), 0)))

      # function to round all the numeric variables
      round_df <- function(df, digits) {
        nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
        
        df[,nums] <- round(df[,nums], digits = digits)
        
        (df)
      }
      
      df_2014 = round_df(df_2014, digits=1)

write.csv(df_2014,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014.csv", row.names = FALSE)

df_2014 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014.csv")


# ----------------- 
# QUALITY RATINGS #
# --------------- #


# what is the first data in the ratings

      ratings =  import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings.csv")
      
      ratings = ratings %>% mutate_each(funs(as.Date), date) %>% 
        group_by(Location.ID) %>% arrange(date, Location.ID) %>% select(-wave, category)

# create a wave variable
# ---------------------

# quality rating

      ratings = ratings %>% mutate(wave = ifelse(date <="2015-09-30", 1, 
                                            ifelse(date > "2015-09-30", 2, NA)))
      
      # how many care homes are inspected so far 
      
      ch = levels(as.factor(ratings$Location.ID))
      
      #14207 care homes are assessed


cqc_ratings = import("/Users/Personas/My Cloud/ch2/chapter2/quality/agglomeration/data/processed/geo_ratings_extended.csv")


# number of care homes per quality

    # get the oslaua variable - I link this throughout the Location.ID
    ratings_geo = cqc_ratings %>% select(Location.ID, oslaua) %>% unique()
    
    ratings = left_join(ratings, ratings_geo, by = "Location.ID") 
    
    ratings =  ratings %>% arrange(year, Location.ID)



#  note: some postcodes are duplicated
        c = ratings %>% group_by(Location.ID, date) %>% filter(n()>1)
        c_post = c %>% select(Location.ID, Location.Post.Code) %>% unique()
        c_post = c_post %>% group_by(Location.ID) %>% filter(n()>1) 
        
        # ids that have two postcodes
        corrupted_id = c_post %>% select(Location.ID) %>% unique() # those IDs that have corrupted postcodes
        
        # get the real postcodes
        tricky = cqc_geolocated %>% filter(location.id %in% corrupted_id$Location.ID) %>% 
          select(location.id, postcode) %>% 
          unique() %>%
          rename(postcode_clean = postcode) # real postcodes associated with tricky IDs

        ratings = left_join(ratings, tricky, by = c("Location.ID" = "location.id"))
        
        ratings = ratings %>% mutate(postcode_clean = ifelse(is.na(postcode_clean), Location.Post.Code, postcode_clean))

# save ratings clean
write.csv(ratings,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/ratings_located.csv", row.names = FALSE)

ratings = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/ratings_located.csv")

library(gdata)

keep(ratings, df_2014, sure = TRUE)

# count care homes by quality in each oslaua and wave 

          oslaua_ratings = ratings %>% group_by(wave,oslaua, Overall.Rating) %>% tally() %>% arrange(oslaua, wave)
          
          oslaua_ratings_wide = oslaua_ratings %>% spread(key = Overall.Rating ,value = n)
          
          # note: less number of observations is because some local authority was not inspete
          
          oslaua_ratings_wide = oslaua_ratings_wide %>% mutate_all(funs(replace(., is.na(.), 0)))

write.csv(oslaua_ratings_wide,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/ratings_located_wave.csv", row.names = FALSE)


df_2014 = left_join(df_2014, oslaua_ratings_wide, by = c("wave", "oslaua"))

# note: various local authorities were not inspected in 2014
df_2014 = df_2014 %>% mutate_all(funs(replace(., is.na(.), 0))) # fill na


write.csv(df_2014,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_ratings.csv", row.names = FALSE)

# ---------------------------------------------------------------------------------------------- #  

# ------------ #      
# HOUSE PRICES #  
# ------------ #       


      prices = import("/Users/Personas/Documents/research/other/visualizations/house prices/house_prices_england/data /raw/house_prices_geolocated.csv")

      prices2014 = prices %>% select(price:post2, oslaua) %>% 
        mutate_each(funs(as.Date), date.1) %>%
        mutate(year = year(date.1)) %>% filter(date.1 >= "2014-10-01")
      
      prices2014 = prices2014%>% arrange(date.1) %>% filter(date.1 < "2016-09-01")
      
      # mean by oslaua 
      
      prices_oslaua = prices2014 %>% group_by(oslaua) %>% filter(grepl("^E", oslaua)) %>% summarise(mean.price = mean(price))
      mean_prices_england = prices_oslaua %>% filter(grepl("^E", oslaua)) %>% as.data.frame()
      
      summary(mean_prices_england)
      

      # create the waves
      
      prices2014 = prices2014 %>% mutate(wave = ifelse(date.1 <="2015-09-30", 1, 
                                                     ifelse(date.1 > "2015-09-30", 2, NA)))
      
     
      # mean prices
      mean_prices = prices2014 %>% group_by(oslaua, wave) %>% summarise(av.price = mean(price)) %>%
      select(oslaua, wave, av.price) %>% as.data.frame()

      # clean wales
      mean_prices = mean_prices %>% unique() # this includes wales 

      mean_prices_england = mean_prices %>% filter(grepl("^E", oslaua)) %>% as.data.frame()ty
     
      # get rid of empty oslauas
       prices_eng =  mean_prices_england %>% filter(oslaua != "")
      
      # common local authorities - sample and price paid data
     oslauas = intersect(test3$oslaua, prices_eng$oslaua)
    
      prices_sample = prices_eng %>% filter(oslaua %in% la)
    

write.csv(mean_prices_england,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/mean_prices_england_oslaua_wave.csv", row.names = FALSE)

test = left_join(df_2014, mean_prices_england, by = c("oslaua", "wave"))


write.csv(test,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_ratings_prices.csv", row.names = FALSE)


# lagged prices (2 years lag)
# ---------------------------

# note: prices already is set for having the data on 2012 -  2014

      prices2012 =  prices %>% filter(date.1 < "2014-10-01")

      # create the waves
      
      prices2012 = prices2012 %>% mutate(wave = ifelse(date.1 <="2013-09-30", 1, 
                                                       ifelse(date.1 > "2013-09-30", 2, NA)))
      
      
      mean_prices = prices2012 %>% group_by(oslaua, wave) %>% mutate(av.price = mean(price)) %>% select(oslaua, wave, av.price)
      
      
      mean_prices = mean_prices %>% unique() # this includes wales 
      
      mean_prices_england_lag = mean_prices %>% filter(grepl("^E", oslaua))
      
      test1 = left_join(test, mean_prices_england_lag, by = c("oslaua", "wave")) %>% 
        rename(av.price = av.price.x, lag.av.price = av.price.y) %>%
        mutate(logprice = log(av.price), loglagprice = log(lag.av.price))


      write.csv(test1,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_ratings_prices.csv", row.names = FALSE)
      



# ------------------------------------------------------------------------------------------------ #

# ----------- #
# INSTRUMENTS #
# ----------- #

      
   # import data yearly basis 
      
      yearly_sample = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_complete.csv")
      
      vars =  yearly_sample %>% select(oslaua, lpa, location.region, year, population, delchange_maj1:pop_density_1911_imp,av.share15 )
      
      # select years 2015 and 2016 and recode into wave 1 and 2 
      
      anhos = c(2015, 2016)
      vars = vars %>% filter(year %in% anhos) %>% mutate(wave = ifelse(year == 2015, 1, ifelse(year == 2016, 2, 0)))
      
      
      test2 =  left_join(test1, vars, by = c("oslaua", "wave")) # complete sample with new oslauas - after 2008 (these are missing)
      
      test3 = test2 %>% filter(!is.na(labourvotes1983)) # sample of analysis
  
      
      # care homes variables of interest
      test3 = test3 %>% group_by(wave, oslaua) %>% 
        mutate(carehomespop = (cum.care.homes/population)*1000,
               goodpop = (Good/population)* 1000,
               inadequatepop = (Inadequate/population)*1000,
               outstandingpop = (Outstanding/population)*1000,
               improvepop = (`Requires improvement`/population)*1000,
               rate_outstanding = (Outstanding/cum.care.homes)*100,
               rate_inadequate = (Inadequate/cum.care.homes)*100)
      
    test3 = test3 %>% mutate_all(funs(replace(., is.na(.), 0))) # fill na
      
      write.csv(test3,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv", row.names = FALSE)
      
      write.csv(test2,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_missinglas.csv", row.names = FALSE) 
      
# ------------------------------
# Link house prices to sample 
# ------------------------------
      
      
      test5 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv")
      
      
      sum_prices_waves = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/sum_prices_waves_oslaua.csv")
      
      
      # current prices
      
      prices_waves = sum_prices_waves %>% filter(wave %in% c(3,4)) %>% select(wave, oslaua, geoavprice) %>% mutate(wave = ifelse(wave == 3, 1, ifelse(wave == 4, 2, NA)))
      
      test5 = left_join(test5, prices_waves, by = c("oslaua", "wave"))
      
      # lagged prices
      
      prices_waves = sum_prices_waves %>% filter(wave %in% c(1,2)) %>% select(wave, oslaua, geoavprice) %>% rename(lag_geoavprice = geoavprice)
      
      test5 = left_join(test5, prices_waves, by = c("oslaua", "wave"))
      
      
      # create logs
      
      test5 = test5 %>% mutate(log_geoaverage = log(geoavprice), lag_loggeoaverage = log(lag_geoavprice)) %>% select(-loggeomaverage)
      
      
      write.csv(test5, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv", row.names = FALSE)
      
      
      
      
      
         
# ----------------- #
# ROBUST SAMPLES 
# ----------------- #

# No London 
# --------

          la = cqc_geolocated %>% select(location.region, oslaua) %>% unique()
          
          common = intersect(la$oslaua, test2$oslaua)
          
          la =  la %>% filter(oslaua %in% common)
          
          la = la %>% filter(location.region != "Unspecified")
          
          test3 = left_join(test2, la, by = c("oslaua")) 
          
          no_london = test3 %>% filter(location.region != "London")

write.csv(no_london,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_nolondon_wave.csv", row.names = FALSE)

# 5% top bottom
# -------------


test3 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv")
              

test3 = test3 %>%
                mutate(quantile = ntile(carehomespop, 100))
              
              centiles = c(1,2,3,5,95,96,97,98,99, 100)
              
              
              test5_bottom = test3 %>% filter(!(quantile %in% centiles))

write.csv(test5_bottom,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_wave_bottom.csv", row.names = FALSE)

test3 =  test3 %>% mutate(entry.rate = entry.rate*100, exit.rate = exit.rate*100)


# -------------------------------
# RESTRICTIVE AND NO RESTRICTIVE
# -------------------------------



care_homes_norestrictive = test3 %>% filter(delchange_maj1 > 0)
care_homes_restrictive =  test3 %>% filter(delchange_maj1 <= 0)




write.csv(care_homes_norestrictive,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_wave_norestrictive.csv", row.names = FALSE)
write.csv(care_homes_restrictive,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_2014_wave_restrictive.csv", row.names = FALSE)

# STATS # 
# -----

# Summary statistics 


# Summary statistics 


library(fBasics)

        vars = test4 %>% select(carehomespop, entry_rate, sum.bed,
                                av.size, outstandingpop, inadequatepop, badpop, goodpop,
                                av.price, delchange_maj1:av.share15)
        
        
        
        stats_vars = basicStats(vars)
        
        stats_vars = data.frame(t(stats_vars)) 
        
        stats_vars = stats_vars %>% select(nobs, Mean, Minimum, Maximum, Stdev)
        
        stats_vars = round(stats_vars, 4)

write.csv(stats_vars,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/stats.csv", row.names = TRUE)
