# Calculation of HHI 

# created: 19/10/2016
# modified: 19/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Calculate the HHI for each local authority considering the number of beds of each provideer per wave.
#       It´s important to consider the cumulative number of beds of each provider in each local authority in each wave 
#       in addition to the beds that new care homes bring to the market. 
#       Note:  These calculations may change depending on the time frames that are considered 
#       
#             Wave: 
#             0 from 2010 to march 2011
#             1 from march 2011 to march 2014
#             2 form march 2014 onwards (sept 2016)
#-----------------------------------------------------------------------------------------------------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(rio)
library(dplyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(tidyr)
library(plyr)
library(magrittr)



data = import("data.csv")

# -----------------
# Prepare the data
# -----------------

      df = data %>% mutate_each(funs(as.Date), hsca.start.date, hsca.end.date) %>%
          mutate(year.entry = format(hsca.start.date, "%Y"),
                 year.exit = format(hsca.end.date, "%Y")) %>%
          select(location.status, provider.id, local.authority, year.entry, year.exit, care.homes.beds, hsca.start.date, hsca.end.date) %>% arrange(hsca.start.date)

# define the waves 

      df  = df %>% mutate(wave.entry = ifelse(year.entry == 2010, 0,
                                              ifelse(hsca.start.date >= "2011-01-01" & hsca.start.date < "2011-03-01", 0,
                                                     ifelse(hsca.start.date >= "2011-03-01" & hsca.start.date < "2014-03-01", 1, 
                                                            ifelse(hsca.start.date >= "2014-03-01", 2, NA)))))
      
      
      df  = df %>% mutate(wave.exit = ifelse(year.exit == 2010, 0,
                                             ifelse(hsca.end.date >= "2011-01-01" & hsca.end.date < "2011-03-01", 0,
                                                    ifelse(hsca.end.date >= "2011-03-01" & hsca.end.date < "2014-03-01", 1, 
                                                           ifelse(hsca.end.date >= "2014-03-01", 2, NA)))))
      
      
      df = df %>% mutate(wave.exit = ifelse(is.na(hsca.end.date), 2,wave.exit))


# expand the data 

      y = df %>%
          rowwise() %>%
          do(data.frame(provider.id = .$provider.id, 
                        local.authority = .$local.authority,
                        beds = .$care.homes.beds, 
                        year = seq(.$wave.entry, .$wave.exit, by = 1),
                        status = .$location.status))
        
        
      y = y %>% mutate(year = as.factor(year)) %>% arrange(local.authority, provider.id)
      y = unique(y) # remove  duplicates 


# - Select the last observation of each care provider for each local authority 
# Get drop of those observations where the care home is inactive 
#   -  assumption: if that wave the care home gets unregistered then is not active that year 

    last = y %>% group_by(local.authority, provider.id, beds) %>% 
      filter(row_number() == n()) %>%
      mutate(toy = 0) %>%
      arrange(provider.id, local.authority, year)

      # - Link the last observations 
            y_linked = left_join(y, last, by = c("provider.id","local.authority", "beds", "year", "status"))
      
      # - Arrange
            y_linked = y_linked %>% mutate_each(funs(as.factor), provider.id, local.authority, status)
      
      
      # - Recode the dummy - where the last is active is a NA and therefore not removable 
            y_linked = y_linked %>% mutate(toy = ifelse(status == "Active", NA, toy)) %>% as.data.frame()

      # - Get rid of the '0' (unactive years)
            y_clean = y_linked %>% filter(is.na(toy)) %>% select(-toy, status)
      
      
      write.csv(y_clean, "hhi.sample2.csv", row.names = FALSE) # hhi sample wave2 (2 waves of three years)


# ---------------
# Count the beds
# ---------------

        y_clean = import("hhi.sample2.csv")

# Note: year represents the wave (0: before march 2011, 1: between march 2011 - march 2014, 2: march 2014 - sept2016)

        y_clean = y_clean %>% mutate_each(funs(as.numeric), beds)
        
        prueba =  y_clean %>%
          dplyr::group_by(local.authority, provider.id, year) %>%
          dplyr::mutate(beds.provider.la = sum(beds, na.rm = TRUE))
        
        
        prueba = prueba  %>%
          dplyr:: group_by(local.authority,year) %>%
          dplyr:: mutate(beds.la = sum(beds, na.rm = TRUE))
        
        prueba = prueba %>% arrange(local.authority, year) %>% select(-status)

# Get an observation per provider - duplicated rows of shares won´t sum 1 otherwise           

        prueba = prueba %>% group_by(local.authority, year) %>% 
          dplyr::mutate(share = (beds.provider.la/beds.la),
                        check = sum(share))
        
        prueba_unique  = prueba %>% group_by(local.authority, provider.id,year) %>% distinct(beds.provider.la, .keep_all = TRUE) %>% 
          as.data.frame()

        # check that the shares are right - should be 1 
        # -----------------------------------------------------------
        check =prueba_unique %>% group_by(local.authority, year) %>% 
          dplyr::mutate(share = (beds.provider.la/beds.la),
                        check = sum(share))
        
        rm(check)
        # ----------------------------------------------------

        prueba_unique = prueba_unique %>% group_by(local.authority, year) %>% 
          dplyr::mutate(share = (beds.provider.la/beds.la),
                        share2 = share^2,
                        hhi = sum(share2))

# Save data 
write.csv(prueba_unique, "data.hhi.lsa.wave2.csv", row.names = FALSE) # data referred to the hhi per local authority in wave2


