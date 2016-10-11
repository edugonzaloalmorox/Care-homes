# Regulatory data

# created: 10/10/2016
# modified: 10/10/2016
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







cou = import("data_COU.dta")
fur = import("data_FUR.dta")
ttwa = import("data_TTWA.dta")
lpa = import("data_LPA.dta")


cqc = cqc %>% mutate_each(funs(as.factor), location.name, location.id, provider.name,
                        provider.id, location.status, provider.status, location.type,
                          city, local.authority, county, location.region,
                          provider.local.authority, registry, reg, postcode, postcode2,
                          oa11, lsoa11, msoa11, imd, n.transactions, lsoa.name) %>% mutate_each(funs(as.Date), date) 
  
  
  cqc = cqc %>% mutate(month.year.entry = format(date, "%Y-%m")) %>% 
    select(location.id:year.entry, month.year.entry, reg:old.people)


# 1.  Select the sample of analysis (march 2011 -  onwards )
      # ----------------------------------------------------

          # postcodes and id´s where the entry was in march 2011 or after 
          muestra = cqc %>% filter(registry == "location.start" & date >= "2011-03-01") %>% arrange(date)
          
          # number of care homes in the sample 
          muestra_post = with(muestra, unique(location.id)) 
          
          # select those locations within the cqc that are in 'muestra_post'
          muestra = cqc %>% filter(location.id %in% muestra_post) # this data sample contains those entries from march 2011 onwards

# 2.  Select those observations that I want to consider 
#     -------------------------------------------------
          
          # Inon
   
                        
