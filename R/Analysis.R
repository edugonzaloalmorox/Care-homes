# Analysis of the data 

# created: 10/10/2016
# modified: 11/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Analyse the regulatory data with regards to the instruments
#
#-----------------------------------------------------------------------------------------------------------------------



setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(rio)
library(dplyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)

cqc  = import("cqc.prices.pop.csv")





# ---------------------------------------
# Load regulatory data - for instruments 
# ---------------------------------------
          cou = import("data_COU.dta")
          fur = import("data_FUR.dta")
          ttwa = import("data_TTWA.dta")
          lpa = import("data_LPA.dta")


          
# --------------------------------          
# Reconsider the type of variables 
# --------------------------------

          cqc = cqc %>% mutate_each(funs(as.factor), location.name, location.id, provider.name,
                                  provider.id, location.status, provider.status, location.type,
                                    city, local.authority, county, location.region,
                                    provider.local.authority, registry, reg, postcode, postcode2,
                                    oa11, lsoa11, msoa11, imd, n.transactions, lsoa.name) %>% mutate_each(funs(as.Date), date) 
            
            
            cqc = cqc %>% mutate(month.year.entry = format(date, "%Y-%m")) %>% 
              select(location.id:year.entry, month.year.entry, reg:old.people)
            
# ----------------------------- 
# Fill the missing observations
# -----------------------------
  
          # remove duplicates       
          cqc =  cqc %>% group_by(location.id) %>% distinct(registry, .keep_all = TRUE) %>% 
            as.data.frame()
        
         # new variables with the filled variables
          prueba = cqc %>% group_by(location.id) %>%
            mutate(mean.price=na.omit(mean_price)[1],
                   max.price = na.omit(max_price)[1],
                   min.price = na.omit(min_price)[1],
                   house.transactions = na.omit(n.transactions)[1],
                   name.lsoa=na.omit(lsoa.name)[1], 
                   pension.credit = na.omit(pension.credit)[1],
                   income.support = na.omit(income.support)[1],
                   jsa.fem = na.omit(jsa.fem)[1],
                   people = na.omit(persons)[1],
                   old = na.omit(old.people)[1]) %>%
                   select(-mean_price, -lsoa.name, - persons, -old.people, -max_price, -min_price, -n.transactions)

   
          cqc_clean = prueba  
          
          lsoas = with(cqc_clean, unique(lsoa11))
          msoas = with(cqc_clean, unique(msoa11))
          provs = with(cqc_clean, unique(provider.id))
          locations = with(cqc_clean, unique(location.id))
          
# -----------------------------                
# Design the sample of analysis
# -----------------------------
 

      # 1.  Select the sample of analysis (march 2011 -  onwards )
            # ----------------------------------------------------
      
                # postcodes and id´s where the entry was in march 2011 or after 
                muestra = cqc_clean %>% filter(registry == "location.start" & date >= "2011-03-01") %>% arrange(date)
                
                # number of care homes in the sample 
                muestra_post = with(muestra, unique(location.id)) 
                
                
                # select those locations within the cqc that are in 'muestra_post'
                muestra = cqc_clean %>% filter(location.id %in% muestra_post) # this data sample contains those entries from march 2011 onwards
      
      # 2.  Select those observations that I want to consider 
      #     -------------------------------------------------
                
                # HHI 
                
                
                
                
                
                hhi <- function(x) {
                  # calculate sum
                  total <- sum(x)
                  
                  # calculate share
                  share <- x*100/total
                  
                  # add
                  return(sum(share^2))
                  
                }
                
                
                
              test = muestra %>% filter(registry == "location.start") %>%
                group_by(provider.id, year.entry, local.authority) %>% 
                mutate(beds.provider = sum(care.homes.beds, na.rm = TRUE)) %>%
                ungroup() %>%
                group_by(local.authority, year.entry) %>% 
                mutate(beds.local.authority = sum(care.homes.beds, na.rm = TRUE), 
                 hhi = ((beds.provider/beds.local.authority)^2))
              
              
                
                
                                                                 
                                                                 check =  test %>% group_by(provider.id) %>% filter(n()>=2) %>%
                  select(location.id, location.name, provider.id, provider.name,
                                        care.homes.beds, beds.provider, beds.local.authority, local.authority, year.entry) %>%
                  arrange(provider.id)
                
                                                          
                        
