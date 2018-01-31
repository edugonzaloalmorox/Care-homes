###########################################
# New wave (8) 
# January 2018
# Sample period: March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Link data for different samples
###########################################


library(tidyverse)
library(rio)
library(lubridate)
library(purrr)
library(dummies)
library(janitor)
library(haven)
library(readxl)

setwd("data/waves/eight")




temp = list.files(pattern="*.csv")

list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

# -----------------------------
# CARE HOMES AND ENTRY RATES
# ------------------------------


# Sample 2011-2017

# create a dataset with all possible cases

    # select the districts that have information on the instrumets 
    care_homes_oslaua = unique(instruments_controls$oslaua)
    
    oslauas = rep(care_homes_oslaua, each = 7)
    years = rep((2011:2017), 315)
    

df_complete = data.frame(oslauas, years)

# calculate the flows

care_homes_base =  care_homes_entries_exits %>% 
  mutate(net = entries - exits) %>%
  group_by(oslaua) %>%
  mutate(care_homes_incumbent =  cumsum(net)) %>%
  select(-incumbent_ch, -incumbent_ch_net, oslaua,
         year_reg, entries, exits, net, care_homes_incumbent) %>%
mutate(entry_rates = entries/lag(care_homes_incumbent)) %>%
  filter(year_reg != 2010)
  


 
# link data to obtain all the cases; when there is NA then recode to 0
      df_complete = left_join(df_complete, care_homes_base, 
                              by = c("oslauas"  = "oslaua", "years" = "year_reg"))
      
      
      df_complete_test = df_complete %>% mutate(exits = ifelse(is.na(exits), 0, exits),
                                                entries = ifelse(is.na(entries), 0, entries),
                                                net = ifelse(is.na(net), 0, net),
                                                care_homes_incumbent = ifelse(is.na(care_homes_incumbent), lag(care_homes_incumbent), care_homes_incumbent))
      

# recode entry rates


    df_complete_test = df_complete_test %>%
      group_by(oslauas) %>%
      mutate(entry_rates = ifelse(is.na(entry_rates), 0, entry_rates))
    
    df_complete_test = df_complete_test %>% group_by(oslauas) %>% 
      fill(care_homes_incumbent, .direction = "down")
    
    df_complete_test = df_complete_test %>% 
      group_by(oslauas) %>%
      mutate(care_homes_incumbent = ifelse(is.na(care_homes_incumbent), 0, care_homes_incumbent))
    
    
    head(df_complete_test, 15)

#-----------
# CAPACITY
#-----------

# beds
     df_complete_test = left_join(df_complete_test, registered_beds, by = c("oslauas" = "oslaua",
                                                                            "years" = "year_reg"))
    
    df_complete_test =  df_complete_test %>%
                         group_by(oslauas) %>%
      mutate(sum_beds = ifelse(is.na(sum_beds), 0, sum_beds))

# average size 

    df_complete_test = left_join(df_complete_test, registered_size, by = c("oslauas" = "oslaua",
                                                                           "years" = "year_reg"))
    
    
    df_complete_test =  df_complete_test %>%
      group_by(oslauas) %>%
      mutate(av_size = ifelse(is.na(av_size), 0, av_size))



# -------------
# HOUSE PRICES
# -------------

    house_prices = house_prices_oslaua_year %>%
      filter(! year_transaction %in% c(2005, 2006, 2007, 2008, 2009, 2010))
    
    
    df_complete_test = left_join(df_complete_test, house_prices, by = c("oslauas" = "oslaua",
                                                                          "years" = "year_transaction"))
# -------------------------
# CONTROLS AND INSTRUMENTS
# -------------------------

    df_complete_test = left_join(df_complete_test, instruments_controls, by = c("oslauas" = "oslaua"))


# ----------
# POPULATION 
# -----------

    pop65  = import("/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/pop65.csv")
    
    pop65$year = as.integer(pop65$year)
    
    
    df_complete_test = left_join(df_complete_test, pop65, by = c("oslauas" = "code",
                                                                 "years" = "year"))

    # fill missing information for 2017
      df_complete_test = df_complete_test %>%
        group_by(oslauas) %>%
        fill(population_65over, .direction = "down")
      
      df_complete_test = df_complete_test %>%
        group_by(oslauas) %>%
        fill(share_pop65_over, .direction = "down")
      
      df_complete_test = df_complete_test %>%
        group_by(oslauas) %>%
        fill(local_authority, .direction = "down")

# --------------------------------------
# CARE HOMES PER 1000 population over 65
# --------------------------------------
      
      df_complete_test = df_complete_test %>%
        mutate(care_home_pop = (care_homes_incumbent/population_65over)*1000)
      
      df_complete_test = df_complete_test %>% 
        select(local_authority, oslauas:care_homes_incumbent,care_home_pop, entry_rates, population_65over, share_pop65_over, sum_beds, everything())
      
      
      
write.csv(df_complete_test, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/care_homes_complete_sample.csv", row.names = FALSE )



