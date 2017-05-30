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



cqc_geolocated = cqc_geolocated %>% mutate(year.exit = ifelse(registry == "location.end" & flow.entry == "exit.def", year(date), NA))

c = cqc_geolocated %>% select(date, registry, year.entry, year.exit,  flow.entry)

# what is the first data in the ratings

ratings =  import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings.csv")

ratings = ratings %>% group_by(Location.ID) %>% arrange(date, Location.ID)

# create a year variable
# ---------------------

# quality rating
      ratings = ratings %>% mutate(year = year(date)) %>% select(-wave, -category)


# calculate variables of interest 
      cqc = cqc_geolocated %>% filter(registry == "location.start" & date >= "2014-10-10") %>% arrange(date)
      
      
# ---------------- #     
# OUTCOME VARIABLES
# ---------------- #
      
      
#   care homes per 1000 population 
      
        entries = cqc_geolocated %>% group_by(oslaua, year.entry) %>% 
            filter(registry == "location.start" & flow.entry == "novo") %>% tally() %>% rename(entry = n)
      
      entries$year.entry = as.integer(entries$year.entry)
      
      
          
        
          
          exits = cqc_geolocated %>% group_by(oslaua, year.exit) %>% 
          filter(registry == "location.end" & flow.entry == "exit.def") %>% tally() %>% rename(exit = n)
      
# data frame with all the local authorities and years

          la = levels(as.factor(entries$oslaua))
          
          la = rep(la, 7)
          y = rep(c(2010:2016), 325)
          
          df = data.frame(oslaua = la, year = y)

# link exists and entries
  
        df = left_join(df, entries, by = c("oslaua" = "oslaua", "year" = "year.entry"))
          
        df = left_join(df, exits, by = c("oslaua" = "oslaua", "year" = "year.exit")) %>% arrange(oslaua, year)
  

  
        df = df %>% mutate_all(funs(replace(., is.na(.), 0)))   # fill na
        
        df = df %>% mutate(net = entry - exit)
        
        df = df %>% mutate(care.homes = cumsum(net))
        
        df = df %>% group_by(oslaua) %>% mutate(entry.rate = entry / lag(care.homes), exit.rate = exit/ lag(care.homes))
      
        
        write.csv(df,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/net_care_homes_yearly.csv", row.names = FALSE)
        
        df_2014 =  df %>% filter(year >= 2014) %>% select(oslaua, year, care.homes, entry.rate, exit.rate)
  
        write.csv(df_2014,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/net_care_homes_2014.csv", row.names = FALSE)
  
  
  
  
  # beds 
  
  # Newly registered beds and average size 
        
        # - sum.bed: sum of beds for each local authority per year
        # - av.size: average size of the entrant care homes
  
  
        beds = cqc %>% group_by(oslaua, year.entry) %>% filter(flow.entry == "novo") %>%
                mutate(sum.bed = sum(care.homes.beds), av.size = mean(care.homes.beds)) %>%
                select(oslaua, year.entry, care.homes.beds, sum.bed, av.size) %>% arrange(oslaua, year.entry)
        
      
              
        beds = beds %>% select(oslaua, year.entry, sum.bed, av.size) %>% unique()
        
        beds$year.entry = as.integer(beds$year.entry)
        
        
        
        df_2014 = left_join(df_2014, beds, by = c("oslaua" = "oslaua", "year" = "year.entry"))
        df_2014 = df_2014 %>% mutate_all(funs(replace(., is.na(.), 0)))
        
        # function to round all the numeric variables
        round_df <- function(df, digits) {
          nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
          
          df[,nums] <- round(df[,nums], digits = digits)
          
          (df)
        }
      
        df_2014 = round_df(df_2014, digits=1)

# ----------------- 
# QUALITY RATINGS #
# --------------- #
        
        cqc_ratings = import("/Users/Personas/My Cloud/ch2/chapter2/quality/agglomeration/data/processed/geo_ratings_extended.csv")
        
        
        # number of care homes per quality
        
        ratings_geo = cqc_ratings %>% select(Location.ID, oslaua) %>% unique()
        
        ratings = left_join(ratings, ratings_geo, by = "Location.ID") 
        
        ratings =  ratings %>% arrange(year, Location.ID)
        
  
        
        #  note: some postcodes are duplicated
        c = ratings %>% group_by(Location.ID, date) %>% filter(n()>1)
        c_post = c %>% select(Location.ID, Location.Post.Code) %>% unique()
        c_post = c_post %>% group_by(Location.ID) %>% filter(n()>1) 
        
        corrupted_id = c_post %>% select(Location.ID) %>% unique() # those IDs that have corrupted postcodes
        
        tricky = cqc_geolocated %>% filter(location.id %in% corrupted_id$Location.ID) %>% 
          select(location.id, postcode) %>% 
          unique() %>%
          rename(postcode_clean = postcode) # real postcodes associated with tricky IDs
        
        ratings = left_join(ratings, tricky, by = c("Location.ID" = "location.id"))
        
        ratings = ratings %>% mutate(postcode_clean = ifelse(is.na(postcode_clean), Location.Post.Code, postcode_clean))
        
  
        
        write.csv(ratings,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/ratings_located.csv", row.names = FALSE)
        
        ratings = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/ratings_located.csv")
        
        library(gdata)

        keep(ratings, df_2014, sure = TRUE)
        
      # count care homes by quality in each oslaua and year 
        
        oslaua_ratings = ratings %>% group_by(year,oslaua, Overall.Rating) %>% tally() %>% arrange(oslaua, year)
        
        oslaua_ratings_wide = oslaua_ratings %>% spread(key = Overall.Rating ,value = n)
        
        oslaua_ratings_wide = oslaua_ratings_wide %>% mutate_all(funs(replace(., is.na(.), 0)))
        
        write.csv(oslaua_ratings_wide,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/ratings_located_year.csv", row.names = FALSE)
        
        
      df_2014 = left_join(df_2014, oslaua_ratings_wide, by = c("year", "oslaua"))
      
     # note: various local authorities were not inspected in 2014
       df_2014 = df_2014 %>% mutate_all(funs(replace(., is.na(.), 0))) # fill na
       
    
       write.csv(df_2014,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_ratings.csv", row.names = FALSE)
  
# ---------------------------------------------------------------------------------------------- #  
 
 # ------------ #      
 # HOUSE PRICES #  
 # ------------ #       
       
       cqc_ratings =  import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_ratings.csv")
       prices = import("/Users/Personas/Documents/research/other/visualizations/house prices/house_prices_england/data /raw/house_prices_geolocated.csv")
      
       prices = prices %>% select(price:post2, oslaua) %>% mutate_each(funs(as.Date), date.1) %>% mutate(year = year(date.1)) %>% filter(date.1 >= "2014-01-01")
       
       mean_prices = prices %>% group_by(oslaua, year) %>% mutate(av.price = mean(price)) %>% select(oslaua, year, av.price)
       mean_prices = mean_prices %>% unique() # this includes wales 
       
       mean_prices_england = mean_prices %>% filter(!grepl("^W", oslaua))
       
       mean_prices_england = mean_prices_england[-981, ]
       
       write.csv(mean_prices_england,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/mean_prices_england_oslaua_yearly.csv", row.names = FALSE)
       
      test = left_join(cqc_ratings, mean_prices_england, by = c("oslaua", "year"))
       

  # lagged prices (2 years lag)
  # ---------------------------
      
      prices = prices %>% select(price:post2, oslaua) %>% mutate_each(funs(as.Date), date.1) %>% mutate(year = year(date.1)) %>% filter(date.1 >= "2012-01-01" & date.1 <="2014-12-31")
      
      mean_prices = prices %>% group_by(oslaua, year) %>% mutate(av.price = mean(price)) %>% select(oslaua, year, av.price)
      mean_prices = mean_prices %>% unique() # this includes wales 
      
      mean_prices_england = mean_prices %>% filter(!grepl("^W", oslaua))
      
      mean_prices_england = mean_prices_england %>% mutate(year_new = year + 2)  %>% 
        ungroup() %>%  select(-year)
      
     mean_prices_england = mean_prices_england %>% rename(lagprice = av.price) 

  # Import complete data
     
     test3 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_complete.csv")
      
     
  # link data 
     
     test4 =  left_join(test3, mean_prices_england, by = c("oslaua" = "oslaua", "year" = "year_new"))
     
     test4 = test4 %>% mutate(loglagprice = log(lagprice)) 
      
# ------------------------------------------------------------------------------------------------ #
      

 # ----------- #
 # INSTRUMENTS #
 # ----------- #
       
      instruments = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc.geolocated.csv")
      
      codes = instruments %>% select(oslaua, dclg.code:la) %>% unique()
      
      cqc_ratings = left_join(cqc_ratings, codes, by = "oslaua")
      
        raw_instrument = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/instr_cqc.csv")
        
        raw_instrument = raw_instrument %>% select(refusal_maj_7908, delchange_maj1, labourvotes1983, pop_density_1911_imp, lpa_code) %>% unique()
        
        test  = left_join(cqc_ratings, raw_instrument, by =  c("old.la" = "lpa_code")) %>% filter(!is.na(labourvotes1983)) # remove those la that do not have historical information

# ---------- # 
# POPULATION #
# -----------#

        old_population = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/pop_65.csv")
        
        conflict = old_population %>% filter(!grepl("^E", oslaua)) %>% rename(oslaua = y2014, y2014 = y2015, y2015 = y2016, y2016 = V6)
        conflict = conflict[, -2]
        
        la_conflicts = unique(conflict$lpa)
        
        old_population_clean = old_population %>% filter(!(lpa %in% la_conflicts)) %>% select(-V6)
        
        old_population_new = rbind(old_population_clean, conflict) 
        
        old_population = old_population_new %>% gather(year, population, y2014:y2016)
        
        old_population = old_population %>% mutate(year = gsub("y", "", year))
        
        write.csv(old_population,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/old_population.csv", row.names = FALSE)
        
        old_population$year = as.numeric(old_population$year)

# link information regarding the population 

        test1 = left_join(test, old_population, by = c("oslaua", "year"))
        
        test1$population = as.numeric(test1$population)
        
        prices = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/mean_prices_england_oslaua_yearly.csv")
        
        test1 = left_join(test1, prices, by = c("oslaua", "year"))

test1 = test1 %>% group_by(year, oslaua) %>% 
          mutate(carehomespop = (care.homes/population)*1000,
                 goodpop = (Good/population)* 1000,
                 inadequatepop = (Inadequate/population)*1000,
                 outstandingpop = (Outstanding/population)*1000,
                 improvepop = (`Requires improvement`/population)*1000,
                 logprice = log(av.price))

write.csv(test1,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_complete.csv", row.names = FALSE)

# ----------- #
# ENTRY RATES # 
# ----------- #

df = df %>% group_by(oslaua) %>% mutate(entry.rate = entry / lag(care.homes), exit.rate = exit/ lag(care.homes))

df_2014 =  df %>% filter(year >= 2014) %>% select(oslaua, year, entry.rate, exit.rate)

test1 = left_join(test1, df_2014,  by = c("oslaua", "year"))

# ----------------- #
# CONTEMPORARY VOTE
# ----------------- #

test5 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/test5.csv")
test3 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_complete.csv")

contemporaneous = test5 %>% select(oslaua, av.share15) %>% unique()


test2 = left_join(test1, contemporaneous, by = c("oslaua"))

test2 = test2 %>% mutate(av.share15 = ifelse(is.na(av.share15),  0.2000723, av.share15))

write.csv(test4,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_complete.csv", row.names = FALSE)

# ----------------- #
# ROBUST SAMPLES 
# ----------------- #

# No London 

      la = cqc_geolocated %>% select(location.region, oslaua) %>% unique()
      
      common = intersect(la$oslaua, test2$oslaua)
      
      la =  la %>% filter(oslaua %in% common)
      
      la = la %>% filter(location.region != "Unspecified")

      test3 = left_join(test2, la, by = c("oslaua")) 
      
      no_london = test3 %>% filter(location.region != "London")

write.csv(no_london,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_nolondon.csv", row.names = FALSE)

# 5% top bottom

test3 = test3 %>%
  mutate(quantile = ntile(carehomespop, 100))

centiles = c(1,2,3,5,95,96,97,98,99, 100)


test5_bottom = test3 %>% filter(!(quantile %in% centiles))

write.csv(test5_bottom,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_bottom.csv", row.names = FALSE)

test3 =  test3 %>% mutate(entry.rate = entry.rate*100, exit.rate = exit.rate*100)


# -------------------------------
# RESTRICTIVE AND NO RESTRICTIVE
# -------------------------------



care_homes_norestrictive = test4 %>% filter(delchange_maj1 > 0)
care_homes_restrictive =  test4 %>% filter(delchange_maj1 <= 0)


write.csv(care_homes_norestrictive,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_norestrictive.csv", row.names = FALSE)
write.csv(care_homes_restrictive,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/care_homes_2014_restrictive.csv", row.names = FALSE)

