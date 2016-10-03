# Link geographical information to the sample of care homes 

# created: 30/09/2016
# modified: 2/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: geolocate each care home 
#       link: care home postcodes to various geographical information  
# I get coordinates and LSOA and MSOA (these will be useful for linking other variables - e.g. prices, market variables)
# I create two different datasets: 
#       - 1. A dataset with all the geographical information linked to the postcode
#       - 2. A dataset with the information relative to the care homes and some geographical information
# -------------------------------------------------------------------------------------------------------------------------



setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(rio)
library(dplyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)

cqc  = import("cqc.entries.csv")
coords = import("post.directory.august2016.csv")


# Get geographical information 
# -----------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw")

postcode.directory = read.csv("http://geoportal.statistics.gov.uk/datasets/8f7f8bc2bd7c4960b44261ed36da185b_0.csv", sep = ",", header = TRUE)

# save the dataset with geographical information 
write.csv(postcode.directory, "post.directory.august2016.csv", row.names = FALSE)

# 1. Geographical dataset associated with the postcodes of the care.homes 

# filter the geographical information by postcodes of the care homes 

cqc = cqc %>% mutate_each(funs(as.factor), postal.code, location.id, location.name, provider.name,
                          provider.id, location.status, provider.status, location.type, city, local.authority, 
                          county, location.region, provider.local.authority, registry, reg)

cqc = cqc %>% mutate(postcode = str_replace_all(postal.code,  pattern = "[[:punct:]]", " ")) %>% select(-postal.code)
# -----------------     
# Data preparation
# -----------------

# Collapse two outward and inward postcode in both datasets 

# CQC data 
cqc$postcode2 = with(cqc, str_replace_all(postcode, "[[:blank:]]", ""))

# Geographical data    
coords$post2 = with(coords, str_replace_all(pcd, "[[:blank:]]", ""))

# Select the postcodes in CQC data
cqc_geo = cqc %>% select(postal.code, postcode2) 

# get unique postcodes 
cqc_geo = unique(cqc_geo) 
cqc_geo = cqc_geo %>% mutate(postcode2 = str_replace_all(postcode2, "[[:blank:]]", ""), postcode2 = stri_trim(postcode2))


cqc_geo %>% group_by(postcode2) %>% filter(n()>1)
# note: there are duplicated postcodes in CQC - cqc_geo.post gets the unique ones 

# get cleaned
# there are duplicated postcodes; get unique without blank spaces and wird things.

cqc_geo.post = unique(cqc_geo$postcode)
cqc_geo.post2 = unique(cqc_geo$postcode2)

# -------------------                
#Geolocate postcodes 
# -------------------

# link postcodes to coordinates for creating geographical information associated to each postcode
ch.geo.cqc = coords %>% filter(post2 %in% cqc_geo.post2)

write.csv(ch.geo.cqc, "geo.ch.postcodes.csv", row.names = FALSE) # uniquely postcodes are geolocated 

geo_cqc = import("geo.ch.postcodes.csv")
cqc = import("cqc.entries.csv")


# link to cqc information

cqc.geolocat = inner_join(cqc, geo_cqc, by = c("postcode2" = "post2"))


# checks - compare postcodes in the linked data and the cqc dataset 
geo.postcodes = with(cqc.geolocat, postcode2)
cqc.postcodes = with(cqc, postcode2) 
setdiff(cqc.postcodes, geo.postcodes)
# -------------------------------



write.csv(cqc.geolocat, "cqc.geolocated.csv", row.names = FALSE) # cqc dataset with geographic information







