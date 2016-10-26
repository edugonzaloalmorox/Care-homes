# Instruments - regulatory information 
# Created: 19/10/2016
# Modified: 25/10/2016


# Regulatory information regarding planning decisions 
# Source: Hilbert and Vermeulen (2016)



# ---------------------------------------
# Load regulatory data - for instruments 
# ---------------------------------------


#-------------------------------
library(rio)
library(dplyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(tidyr)
library(plyr)
library(magrittr)
library(ggplot2)
library(pglm)
library(stargazer)
library(lme4)
#-------------------------------



setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw")

cou = import("data_COU.dta")
fur = import("data_FUR.dta")
ttwa = import("data_TTWA.dta")
lpa = import("data_LPA.dta")
la_codes = read.csv("la_codes_lsoa.csv", sep = ";", header = TRUE)

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

# Link instruments variable to CQC data - by the LPA code 

    # I need to attach the local planning authority information to the lsoa codes 
    # I link information of lpa and cqc - key variable is the LA code 

            # Step 1: Attach LA code to geocoded CQC data 
            # Step 2: Attach instrument information to geocode CQC - through the old LA code 
            # Step 3: Attach the latter to the CQC with other information


# Step 1
# ------

     cqc_geo = import("cqc.geolocated.csv")
     
    # Attach local planning information  
   
           # clean up names 
           names(la_codes) = tolower(colnames(la_codes))
           
        # Link old local authorities codes and geocoded cqc information - key: la district code ('oslaua' in cqc geocoded)
           
           # select lsoas in cqc 
           
           lsoas_cqc =  cqc_geo %>% select(postcode, lsoa11, oslaua) 
           lsoas_cqc =  unique(lsoas_cqc)
         
          # link old district codes 
           
          cqc_geo_linked = left_join(lsoas_cqc, la_codes, by = c("oslaua" ="current.la"))
          
          prueba = left_join(cqc_geo, cqc_geo_linked, by = c("postcode","lsoa11","oslaua"))
          
          write.csv(prueba, "cqc.geolocated.csv", row.names = FALSE)
          
# Step 2
# ------
          
    # Link instruments to CQC postcodes 
          
          cqc_instrument = cqc_geo %>% select(postcode, postcode2, local.authority, la, old.la)
          
          cqc_instrument = cqc_geo %>% distinct(local.authority, la, old.la) # select unique
          
           
        # select unique la codes in LPA and CQC 
          
          codes_lpa = lpa %>% distinct(lpa_code) %>% as.data.frame()   # (329 different old codes in lpa)
          
          codes_cqc = cqc_geo %>% distinct(old.la) %>% as.data.frame() # (325 different old codes in cqc)
          
          # common and differen old la codes in cqc data and LPA -filter in LPA by the codes contained in CQC  
          
           instr.of.cqc = lpa %>% filter(lpa_code %in% lev_cqc)
                   inex = setdiff(lev_cqc, lev_lpa) # those old that are in CQC but not in LPA
                   
                   inex_names = la_codes %>% filter(old.la %in% inex)
                   
          
          common = intersect(codes_cqc$old.la.x, codes_lpa$lpa_code)
           different = set(codes_lpa$lpa_code, codes_cqc$old.la)
           
           
          
                # select those codes in the instruments LPA 
           
          
          instr = lpa %>% filter(lpa_code %in% common)
          
          instr_cqc = left_join(instr, cqc_instrument, by = c("lpa_code" = "old.la.x"))
          
          # reorder variables 
          instr_cqc = instr_cqc %>% select(year, lpa_code, lpa_num, local.authority, lpa_name:trafsqtrend, -total) 
          
          
          
          # .................
          # Note: local.authority refers to the local authority that is in charge of the social care decisions
          # .................
          
          
          write.csv(instr_cqc, "instr_cqc.csv", row.names = FALSE)  # iv information based on cqc postcodes 
         
         
      
          
        

          
  
