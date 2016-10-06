# ONS - population

# created: 5/10/2016
# modified: 6/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Include Census Information associated with different population
#                                       - it´s considered the lower layer LSOA from 2001
#       Neighbour statistics are extracted from the ONS official labour market statistics (NOMIS - DWP)
#       Present data in long format 
# -------------------------------------------------------------------------------------------------------------------------


setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw")

library(rio)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(lubridate)

# --------------
# Old population 
# --------------
        
        old.pop = import("nomis_old_pop.csv")
        n.old = c("soa", "code", "total", "old.85")
        names(old.pop) <- n.old
        
        write.csv(old.pop, "old.population.csv", row.names = FALSE)
    
    # Note: This can be obtained from the 'population' dataset 

# -------------
# Informal care 
# -------------
        informal = import("nomis_unpaid_care.csv")
        setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")
      

# ----------
# Population 
# ----------
          population = read_excel("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/POP_Table_PP04__LSOA_MSOA_england_wales.xls", sheet = 1, col_names = TRUE, col_types = NULL)
          
          name.population = c("region.code", "region.name", "loc.auth.code",
                            "local.authority", "msoa.code", "msoa",
                            "lsoa.code", "lsoa", "persons", "old.85",
                            "old.90", "total.old")
        
          colnames(population) <- name.population
          
          write.csv(population, "total.population.csv", row.names = FALSE)
          
                    
                  
# ---------------------------------------
# Link with benefits and population data
# ---------------------------------------
          
      # Note: Benefits data are collected considering 2001 LSOA codes from Nomis web
      #       Population data are collected considering 2011 codes from the ONS Census
          
      # It necessary to obtain both codes in a single datasets to merge them correctly 
      # ---------------------------------------------------------------------------------

          benefits = import("benefits.csv") # (note: the OAS are referred to 2001)
          old.lsoa = population %>% select(`lsoa.code`, `lsoa`,`persons`, total.old) # (note: the OAS are referred to 2011)
          
        
        
    # Geographical information
    #      Idea: get both codes (01 and 11) to link benefits and popuation 
    #      Three steps to carry out this   
        
        # Step 1: Get geographical information and create a dataset that associates LSOA codes for 2001 and 2011 
        # ......................................................................................................
          
          geo = read.csv("http://geoportal.statistics.gov.uk/datasets/8f7f8bc2bd7c4960b44261ed36da185b_0.csv", sep = ",", header = TRUE)

              # data that links postcodes from 01 and 11 
              geo.codes = geo %>% select(lsoa01, msoa01, lsoa11, msoa11) %>% 
                mutate(equal = ifelse(lsoa01 == lsoa11, TRUE, FALSE))

                write.csv(geo.codes, "oas_codes.csv", row.names = FALSE)
        
        
        # Subset the geo dataset with the codes associated with the benefits (it´s easier to merge than 2M)
            # Get the codes associated with benefits 
                benefit.codes = with(benefits, unique(code))
              
                geo.code.benefit = geo.codes %>% filter(lsoa01 %in% benefit.codes)
              
                g = unique(geo.code.benefit)
                  
                g = g %>% select(lsoa01, lsoa11, msoa01, msoa11)
                  
        
          # Step 2: Link codes to benefits 
          # ..............................
                
                #Therefore I have a benefits dataset where I have the codes from 2011 and 2001
                
                benefits = left_join(benefits, g, by = c("code"="lsoa01")) 
              
                benefits =unique(benefits) # drop duplicates 
              
          # Step 3 : Link benefits and population variables 
          # ................................................
                
          old.lsoa = read.csv("old_pop_lsoa.csv", sep = ";", header = TRUE)
          
          old.lsoa = old.lsoa %>% select(LSOA.Code:Persons, Old.people)
            names.old.lsoa = tolower(colnames(old.lsoa))   
            colnames(old.lsoa) = tolower(colnames(old.lsoa))  
            
            old.lsoa = old.lsoa 
      
                # benefits, old population and  whole population
            benefits.pop = left_join(benefits, old.lsoa, by = c("lsoa11" = "lsoa.code"))
           
            
           
            # Checks -----------------------------------------------------------
            check = benefits.pop.clean %>% filter(is.na(persons)) 
            benefits.pop.clean = benefits.pop %>% filter(soa != "Column Total")
            # ------------------------------------------------------------------
            
            
          # benefits and population: this dataset contains:
          #                   -  Number of people getting benefits
          #                   -  Total people 
          #                   -  People older than 85 
            
            setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")
            write.csv(benefits.pop.clean, "benefits.pop.csv", row.names = FALSE)  # I have to use 'lsoa.name' - contains the denomination for lsoa 2011
            
            
# ---------------------------
# Link benefits with CQC data         
# ----------------------------

            benefits.pop = import("benefits.pop.csv")
            cqc = import("cqc.prices.lsoa.csv")

 # clean benefits 
 benefits = benefits.pop %>% select(lsoa.name, lsoa11, year, pension.credit:jsa.fem, persons, old.people) %>% mutate()

 benefits = benefits %>% mutate(year.entry = gsub("feb.", "", year)) 




cqc.test = left_join(cqc, benefits, by = c("lsoa11", "year.entry"))

cqc.test = cqc.test %>% group_by(postcode2, date, location.id) %>% arrange(postcode2, location.id, date) %>% select(-year)

write.csv(cqc.test, "cqc.prices.pop.csv", row.names = FALSE)




