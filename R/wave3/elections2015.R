# British elections 2015 data 

library(rio)
library(tidyverse)
library(stringr)

elections = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/hocl-ge2015-results-full.csv", sep = ",", header = TRUE)


en_election = elections %>% filter( grepl("^E", ons_id) ) # select those observations that start with E -English observations
en_election = en_elections %>% filter(!(region_name %in% england)) %>% select(ons_id, ons_region_id, 
                                                                           party_abbreviation, votes, share, change)



write.csv(en_election, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/en_election15.csv", row.names = FALSE)

en_election15 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/en_election15.csv")


# select those variables

vacias = en_election %>% filter(ons_region_id == "")

# analysis of empty ones 

# Idea: split the data into meaningful pieces and create a cleaner dataset that contains: geographical codes, party information, the share and change. 
# To less extent: focus on Labour and Conservative.

# create test 3 and select the codes - this is the first bit of our data and contains the codes 
# ---------------------------------------------------------------------------------------------

test3 = gsub("[[:punct:]`.]", " ", vacias$ons_id) 

codes = str_split_fixed(test3," ", 3) %>% as.data.frame() %>% select(V1, V2) 


# create borough object - drops information before "Borough" and purpose is to get information about the parties

borough =  gsub(".*(England)"," ", vacias$ons_id)

b = str_split_fixed(borough,",", 11)

b = as.data.frame(b) %>% select(V3, V4, V10, V11)

codes = codes %>% select(ons_id  = V1, region_id = V2)

# link 

codes = cbind(codes, b)

# clean problematic parties: christian party and beer , baccy and scratinghs 

clean =  codes %>% filter(V10 != "No")

share = str_split_fixed(clean$V11,",", 2) %>% as.data.frame() %>% select(proportion = V1, change = V2)

# link all the bits of information
# ------------------------------

        clean = clean %>% select(-V11)
        
        clean = cbind(clean, share)
# clean up "clean" and attach to the complete dataset en_election 

        clean = clean %>% select(ons_id, ons_region_id = region_id, party_abbreviation = V4, votes = V10, share = proportion, change)


# filter in the complete dataset those observations that were not complete

        llenas = en_election %>% filter(ons_region_id != "")
        
        england1 = rbind(llenas, clean) # it has 7 observations less than llenas +  vacias that correspondi to the removal of weird partiess


        write.csv(england1, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/en_election15_clean.csv", row.names = FALSE)


# add some geographical variables to link with the data
        
      # the data that is common are the constituencies


        geo_vars= geo_ratings %>% select(oslaua, oscty, pcon)
        
        geo_elections = left_join(england1, geo_vars, by = c("ons_id" = "pcon"))
        geo_elections = unique(geo_elections)

# select Labour shares for 2015
# -----------------------------

        labour2015 = geo_elections %>% filter(party_abbreviation == "Lab")
        
        
        partidos = c("Lab", "Con", "UKIP")
        labour_conservative_UKIP2015 = geo_elections %>% filter(party_abbreviation %in% partidos)
        
        write.csv(labour2015, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/labour2015.csv", row.names = FALSE)
        write.csv(labour_conservative_UKIP2015, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/labour_conservative_UKIP2015.csv", row.names = FALSE)
        

