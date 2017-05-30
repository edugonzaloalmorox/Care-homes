################################################################
#
# Create sample with all the local authorities
# 30-05-2017
# Unit of data - district level (oslaua). 
# Time periods: October 2014-  September 2015 (wave 1); October 2015 - September 2016 (wave 2)
# Edu Gonzalo Almorox, e.gonzalo-almorox@newcastle.ac.uk.
#################################################################

library(dplyr)
library(tidyverse)
library(rio)
library(lubridate)
library(purrr)



######################################

# #####################
# Entry and exit counts
#######################

# load data
  cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc.geolocated.csv")

# transform date into date format 
  cqc = cqc %>% mutate_each(funs(as.Date), date)

# entries #
# ------- #

# select novo entries 
  novo_entries = cqc %>% filter(registry == "location.start" & reg == "novo")

# create waves considering the date of entry
  novo_entries = novo_entries %>% mutate(wave = ifelse(date < "2014-10-01" , 0, 
                                                     ifelse(date >= "2014-10-01"  & date <="2015-09-30", 1, 
                                                            ifelse(date > "2015-09-30" , 2, NA)))) 
# count novo entries
  count_novo = novo_entries %>% group_by(oslaua, wave) %>% 
    tally() %>% 
    rename(entries = n) %>% 
    arrange(oslaua, wave)


# exits #
# ----- #

# select definite exits
  definite_exits = cqc %>% filter(registry == "location.end" & reg == "exit.def")

# create waves considering the market exit
  definite_exits = definite_exits %>%mutate(wave = ifelse(date < "2014-10-01" , 0, 
                                                        ifelse(date >= "2014-10-01"  & date <="2015-09-30", 1, 
                                                               ifelse(date > "2015-09-30" , 2, NA)))) 

# count definite exits
  count_exits = definite_exits %>% group_by(oslaua, wave) %>% 
    tally() %>% rename(exits = n) %>% 
    arrange(oslaua, wave)

# net flow of entries and exits #
# ------------------------------

# merge entries and exits 
  counts = full_join(count_novo, count_exits, by = c("oslaua", "wave"))

  counts = counts %>% arrange(oslaua, wave)

# fill NA with 0 (if there nothing happened - then is 0)
  counts = counts %>% mutate_all(funs(replace(., is.na(.), 0)))

# calculate net flow 
  counts = counts %>% mutate(net_wave = entries - exits)

# calculate the accumulated care homes per local authority for each wave
  counts = counts %>% group_by(oslaua) %>% mutate(care_homes = cumsum(net_wave))

# df with all the local authorities

  la = levels(as.factor(novo_entries$oslaua))
  la = rep(la, 3)
  y = rep(c(0:2), 325)

  df = data.frame(oslaua = la, wave = y) %>% arrange(oslaua, wave) # all the LA's and wave

  # all the counts for all the waves
  counts_test = left_join(df, counts, by = c("oslaua", "wave"))


# fill NA with 0 
  counts_test = counts_test %>% mutate_all(funs(replace(., is.na(.), 0)))
  
  counts_test = counts_test %>% group_by(oslaua) %>% mutate(care_homes = cumsum(net_wave), 
                                                            entry_rate = (entries /lag(care_homes)*100),
                                                            exit_rate = (exits/lag(care_homes)*100))
  
  counts_test = counts_test %>% filter(wave != 0)

# save data on counts
write.csv(counts_test, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/count_entries_exits_wave.csv", row.names = FALSE)

####################################################################


######################################################################

# ######################
# Beds and average size #
# ##################### #


# Newly registered beds and average size 

# - sum.bed: sum of beds for each local authority per year
# - av.size: average size of the entrant care homes

# note: reload cqc and get novo entries 

novo_entries2014 =  novo_entries %>% filter(wave != 0)

# obtain newly registered beds for the period of analysis
  beds = novo_entries2014 %>% group_by(oslaua, wave) %>% 
      filter(reg == "novo") %>%
      mutate(sum.bed = sum(care.homes.beds), av.size = mean(care.homes.beds))%>% 
      select(oslaua, wave, sum.bed, av.size) %>% 
      unique() %>% 
      arrange(oslaua, wave)

  # create a data frame with all the local authorities and waves
  la = levels(as.factor(novo_entries$oslaua))
  la = rep(la, 2)
  y = rep(c(1:2), 325)

 
  df = data.frame(oslaua = la, wave = y) %>% arrange(oslaua, wave) # all the LA's and wave


# link all the data local authorites and waves with the new registered beds
  df_beds = left_join(df, beds, by = c("oslaua" ,  "wave"))


# fill the missing
  df_beds = df_beds %>% mutate_all(funs(replace(., is.na(.), 0)))

# function to round all the numeric variables
  round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
  }

df_beds = round_df(df_beds, digits=1)

write.csv(df_beds,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/beds_waves_oslaua.csv", row.names = FALSE)


####################################################################


######################################################################

# ####### #
# Ratings #
# ####### #


  ratings = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/ratings_located_wave.csv")

  df_ratings = left_join(df, ratings, by = c("oslaua", "wave"))

# note: various local authorities were not inspected in 2014
  df_ratings = df_ratings %>% mutate_all(funs(replace(., is.na(.), 0))) # fill na


write.csv(df_ratings,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/ratings_waves_oslaua.csv", row.names = FALSE)


####################################################################


######################################################################

# ########## #
# Population #
# ########## #


  population = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/four/old_population.csv")

# Select waves for 2015, 2016. 
# Assumption 2015 ~ wave 1; 2016 ~ wave 2

  pop_waves =  population %>% filter(year != "2014")
  pop_waves = pop_waves %>% mutate(wave = ifelse(year == "2015", 1, ifelse(year == "2016", 2, NA)))
  
  df_pop = left_join(df, pop_waves, by = c("oslaua", "wave")) %>% select(oslaua, wave, population)
  
  
  write.csv(df_pop,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/population_waves_oslaua.csv", row.names = FALSE)
  

  ####################################################################
  
  
  ######################################################################
  
  # ############ #
  # House prices #
  # ############ #
 
  # note: sum prices are created in "prices_LR.R"
  
  sum_prices_waves = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/sum_prices_waves_oslaua.csv")
  
  
  # current prices
  
  prices_waves = sum_prices_waves %>% filter(wave %in% c(3,4)) %>% select(wave, oslaua, geoavprice) %>% mutate(wave = ifelse(wave == 3, 1, ifelse(wave == 4, 2, NA)))
  
  test5 = left_join(test5, prices_waves, by = c("oslaua", "wave"))
  
  test5 = test5 %>% mutate(log_geoaverage = log(geoavprice))
  
  
  # lagged prices
  
  prices_waves = sum_prices_waves %>% filter(wave %in% c(1,2)) %>% select(wave, oslaua, geoavprice) %>% rename(lag_geoavprice = geoavprice)
  
  test5 = left_join(test5, prices_waves, by = c("oslaua", "wave"))
  
  
  # create logs
  
  test5 = test5 %>% mutate(lag_loggeoaverage = log(lag_geoavprice))
  
  write.csv(test5, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv", row.names = FALSE)
  
  
