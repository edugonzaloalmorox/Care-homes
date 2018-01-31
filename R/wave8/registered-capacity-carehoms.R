#########################################################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Variables referrred to capacity: new registered beds and registered average size
########################################################################

library(tidyverse)
library(rio)
library(lubridate)
library(purrr)
library(dummies)
library(janitor)
library(haven)
library(readxl)


care_homes = import("/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/care_homes.csv")

# ---------
# NEW BEDS 
# ----------

# bed registrations after march 2011 
    care_homes_entries = care_homes %>% 
      group_by(postcode, location_name, location_id) %>%
      mutate_at(vars(initial_reg_date), funs(as.Date)) %>% 
      arrange(initial_reg_date)  %>%
      filter(initial_reg_date >= "2011-03-01")
    
    
    care_homes_beds = care_homes_entries %>%
      mutate(year_reg = year(initial_reg_date)) %>%
      select(oslaua, year_reg, care_homes_beds) %>% 
      group_by(oslaua, year_reg) %>% 
      summarise(sum_beds = sum(care_homes_beds))

# incumbent beds before march 2011

      incumbent_beds = care_homes %>% 
        filter(initial_reg_date < "2011-03-01") %>%
        mutate(year_reg = year(initial_reg_date)) %>%
        select(oslaua,care_homes_beds) %>% 
        group_by(oslaua) %>%
        summarise(sum_beds = sum(care_homes_beds)) %>% 
        mutate(year_reg = 2010)
      
# link data regarding new and incumbent beds 
      
      oslaua_beds = full_join(care_homes_beds, incumbent_beds, by = c("oslaua", "year_reg", "sum_beds")) %>%
        arrange(oslaua, year_reg) %>%
        mutate(sum_beds = ifelse(is.na(sum_beds), 0, sum_beds))
      
     
      
      write.csv(oslaua_beds, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/registered_beds.csv", row.names = FALSE)
      
      
# -------------
# AVERAGE SIZE
# -------------
      
      oslaua_size = care_homes_entries %>%
        mutate(year_reg = year(initial_reg_date)) %>%
        group_by(oslaua, year_reg) %>%
        summarise(av_size = mean(care_homes_beds))
      
      # recode 
      oslaua_size= oslaua_size %>% 
        group_by(oslaua) %>% 
        mutate(av_size = ifelse(is.na(av_size), lead(av_size), av_size))
      
      write.csv(oslaua_size, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/registered_size.csv", row.names = FALSE)
      

