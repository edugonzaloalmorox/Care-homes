# Calculation of HHI 

# created: 13/10/2016
# modified: 13/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Calculate the HHI for each local authority considering the number of beds of each provideer per year.
#       It´s important to consider the cumulative number of beds of each provider in each local authority in each year 
#       in addition to the beds that new care homes bring to the market. 
#-----------------------------------------------------------------------------------------------------------------------


setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(rio)
library(dplyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)

cqc  = import("cqc.prices.pop.csv")

cqc = cqc %>% mutate_each(funs(as.factor), location.name, location.id, provider.name,
                          provider.id, location.status, provider.status, location.type,
                          city, local.authority, county, location.region,
                          provider.local.authority, registry, reg, postcode, postcode2,
                          oa11, lsoa11, msoa11, imd) %>% mutate_each(funs(as.Date), date) 


 
locales = c("Birmingham", "Hertfordshire") 
registro = c("location.start", "location.end")

x = cqc %>% filter(local.authority %in% locales & registry %in% registro) %>% 
  select(location.id:care.homes.beds, local.authority, registry, date, year.entry) %>%
  mutate(year.exit = ifelse(registry == "location.end" & !is.na(date), format(date, "%Y"), NA))


beds = x %>% 



