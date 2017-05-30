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
  
  ######################################################################
  
  
  ######################################################################
  
  # ############ #
  # Instruments  #
  # ############ # 
  
# raw data from  replication materials Hilber and Vermeulen (2016)
 instr = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/data_LPA.dta")

# select relevant information 
  
  info_inst = instr %>% select(lpa_code:county_pre_96, delchange_maj1, labourvotes1983, pop_density_1911_imp)
  
# data referred to each local authority
  info_inst = unique(info_inst)
  
# select codes associated with the cqc
  cqc_codes = cqc_geolocated %>% select(oslaua:la) %>% unique()
  
# link old and new information of local authorities  
  instr_linked = left_join(cqc_codes, info_inst, by = c("old.la" = "lpa_code"))

# counties that are reformed
  instr_missing =  instr_linked %>% filter(is.na(delchange_maj1))
  
# select information for old counties
# note: drop information corresponding to Isles of Scilly - there is not information available
  
  counties = c("Durham", "Cornwall", "Northumberland", "Shropshire", "Wiltshire")
  
  instr_counties = info_inst %>% filter(county_pre_96 %in% counties) %>% filter(lpa_name_2005on != "Isles of Scilly")
  
  counties_mean = instr_counties %>% group_by(county_pre_96) %>% 
    mutate(av.delchange = mean(delchange_maj1),
           av.labourvotes = mean(labourvotes1983),
           av.pop_density = mean(pop_density_1911_imp)) %>% 
    select(county_pre_96, av.delchange, av.labourvotes, av.pop_density) %>%
           unique()
  
# create a "la" variable to merge with linked information based on instruments 
  counties_mean = counties_mean %>% mutate(la = paste(county_pre_96, "UA", sep = " "))
  counties_mean$la[counties_mean$la == "Durham UA"] <- "County Durham UA"
  
# merge linked information of the instruments and the missing
  
  instr_short = instr_linked %>% select(oslaua, la, delchange_maj1:pop_density_1911_imp)
  
  instr_test = left_join(instr_short, counties_mean, by = "la")
  
  # recode information on instruments and clean up 
  
  instr_test = instr_test %>% mutate(delchange_maj1 = ifelse(is.na(delchange_maj1), av.delchange, delchange_maj1),
                                      labourvotes1983 = ifelse(is.na(labourvotes1983), av.labourvotes, labourvotes1983), 
                                     pop_density_1911_imp = ifelse(is.na(pop_density_1911_imp), av.pop_density, pop_density_1911_imp)) %>%
    select(oslaua:pop_density_1911_imp)
                                     
  instr_sample = instr_test %>% filter(!is.na(delchange_maj1))

  # note: There are LAs that are not merged -  Cheshire East, Cheshire West, Central Bedforshire, Bedford and Isles of Scilly
  
  write.csv(instr_sample, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/instr_oslaua.csv", row.names = FALSE)
  
  
  
  
