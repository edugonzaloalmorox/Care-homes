# Calculation of HHI 

# created: 13/10/2016
# modified: 19/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Calculate the HHI for each local authority considering the number of beds of each provider per year.
#       It´s important to consider the cumulative number of beds of each provider in each local authority in each year 
#       in addition to the beds that new care homes bring to the market. 
#       Caveat:These calculations may change depending on the time frame that are considered 
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

cqc  = import("cqc.prices.pop.csv")
data = import("data.csv")

# -----------------
# Prepare the data
# -----------------

      df = data %>% mutate_each(funs(as.Date), hsca.start.date, hsca.end.date) %>%
        mutate(year.entry = format(hsca.start.date, "%Y"),
               year.exit = format(hsca.end.date, "%Y")) %>%
        select(location.status, provider.id, local.authority, year.entry, year.exit, care.homes.beds, hsca.start.date) 
      
      df = df %>% mutate(year.exit = ifelse(is.na(year.exit), "2017", year.exit)) %>% 
        mutate_each(funs(as.numeric), year.entry, year.exit,care.homes.beds)

# Long structure; expand entries and consider exits


          # dplyr solution
      
          y = df %>%
              rowwise() %>%
              do(data.frame(provider.id = .$provider.id, 
                            local.authority = .$local.authority,
                            beds = .$care.homes.beds, 
                            year = seq(.$year.entry, .$year.exit, by = 1),
                            status = .$location.status))


            y = y %>% arrange(local.authority, provider.id, year)
            y = unique(y) # remove  duplicates 


# Get drop of those observations where the care home is inactive -  assumption: if that year the care home gets unregistered then is not active that year 

    # Create a dummy variable to reflect the observation where the provider is inactive
    # ...................................................................................
            
            
            # - Select the last observation of each care provider for each local authority 
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
            
             
             
             y_clean = as.data.frame(y_clean)
             
           write.csv(y_clean, "hhi.sample.csv", row.names = FALSE)

# ---------------
# Count the beds
# ---------------

          y_clean = import("hhi.sample.csv")
                           
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
          
          prueba_unique = prueba_unique %>% group_by(local.authority, year) %>% 
            dplyr::mutate(share = (beds.provider.la/beds.la),
                          share2 = share^2,
                          hhi = sum(share2))
  
  write.csv(prueba_unique, "data.hhi.lsa.csv", row.names = FALSE) # data referred to the hhi per local authority

 

  

 
  