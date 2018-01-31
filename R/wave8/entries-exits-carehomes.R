#################################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Entries, exits and total number of care homes
#################################################

library(tidyverse)
library(rio)
library(lubridate)
library(purrr)
library(dummies)
library(janitor)
library(haven)
library(readxl)

care_homes = import("/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/care_homes.csv")

# -------
# ENTRIES 
# --------

# registrations after march 2011 
    care_homes_entries = care_homes %>% 
      group_by(postcode, location_name, location_id) %>%
      mutate_at(vars(initial_reg_date), funs(as.Date)) %>% 
      arrange(initial_reg_date)  %>%
      filter(initial_reg_date >= "2011-03-01")
    
    
    care_homes_entries = care_homes_entries %>%
      mutate(year_reg = year(initial_reg_date)) %>%
      group_by(oslaua, year_reg) %>%
      tally()  %>% 
      rename(entries= n)

# incumbent 

    incumbent_care_homes = care_homes %>% 
      filter(initial_reg_date < "2011-03-01") %>%
      group_by(oslaua) %>% 
      tally() %>% 
      rename(incumbent_ch = n) %>%
      mutate(year_reg = 2010)
    
    oslaua_care_homes = full_join(care_homes_entries, incumbent_care_homes, by = c("oslaua", "year_reg")) %>%
      arrange(oslaua, year_reg)
    
    oslaua_care_homes = oslaua_care_homes %>%
      group_by(oslaua) %>% 
      mutate(entries = ifelse(is.na(entries), incumbent_ch, entries), 
             incumbent_ch = cumsum(entries))
    
    oslaua_care_homes

# ------
# EXITS
# ------
    
    care_homes_exits = care_homes %>% 
      group_by(postcode, location_name, location_id) %>%
      mutate_at(vars(location_hsca_end_date), funs(as.Date)) %>% 
      arrange(initial_reg_date)  %>%
      filter(!is.na(location_hsca_end_date)) %>%
      mutate(year_reg = year(location_hsca_end_date)) %>%
      group_by(oslaua, year_reg) %>%
      tally()  %>% 
      rename(exits = n)
    
    oslaua_care_homes = left_join(oslaua_care_homes, care_homes_exits, by = c("oslaua", "year_reg")) %>%
      select(oslaua, year_reg, entries, exits, incumbent_ch)
    
    oslaua_care_homes = oslaua_care_homes %>% 
      group_by(oslaua) %>%
      mutate(exits = ifelse(is.na(exits), 0, exits),
             incumbent_ch_net = incumbent_ch - exits)
      
    write.csv(oslaua_care_homes, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/care_homes_entries_exits.csv", row.names = FALSE)
    
    

