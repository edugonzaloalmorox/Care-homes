# Neighbour statistics

# created: 4/10/2016
# modified: 5/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Include socio-economic information corresponding to the each superoutput area 
#                                       - it´s considered the lower layer LSOA from 2001
#       Neighbour statistics are extracted from the ONS official labour market statistics (NOMIS - DWP)
#       Present data in long format 
# -------------------------------------------------------------------------------------------------------------------------


setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(lubridate)

# --------------------------
# Load and rename variables
# --------------------------

pension = import("nomis_pension_credits.csv")
income = import("nomis_income_support.csv")
jsa.females = import("nomis_jsa_females.csv")



# Income support
        # Rename variables 
        n.income = c("soa","code","feb.2010","feb.2011","feb.2012","feb.2013","feb.2014","feb.2015","feb.2016")
        names(income) <- n.income 
        
      
# Pension credits
        # Break the first variable into two 
        # Rename and reorder variables
       
       pension = pension %>% mutate(code = gsub("(.*)\\:.*", "\\1", V1),
                                         soa = gsub(".*:","",V1),
                                    code = stri_trim(code, "both"),
                                    soa = stri_trim(soa, "both")) %>%
         select(soa,code,feb.2010 = V2,
                feb.2011 = V3,
                feb.2012 = V4,
                feb.2013 = V5,
                feb.2014 = V6,
                feb.2015 = V7, 
                feb.2016 = V8)
         
                                       
# Job Support Allowance 
       n.jsa = n.income
       names(jsa.females) = n.jsa
       
       # checks ------
        # SOA
             soa.jsa = jsa.females$soa
             soa.income = income$soa
             
             setdiff(soa.jsa, soa.income)
        # CODE
             code.jsa = jsa.females$code
             code.income = income$code
             
             setdiff(code.jsa, code.income)
       # ----------------------------------

# --------------------------------
# Change the structure of the data 
# --------------------------------
          
           
             pension_long = pension %>% gather(year, pension.credit, feb.2010:feb.2016)
             income_long = income %>% gather(year, income.support, feb.2010:feb.2016)
             jsa_long = jsa.females %>% gather(year, jsa.fem, feb.2010:feb.2016)
             
          
    
                # Link datasets 
              
                benefits = left_join(pension_long, income_long, by = c("soa", "code", "year")) 
                benefits = left_join(benefits, jsa_long, by = c("soa", "code", "year"))
                   
                # saves 
               write.csv(benefits, "benefits.csv", row.names = FALSE)
               write.csv(pension_long, "pension.long.csv", row.names = FALSE)
                write.csv(income_long, "income.long.csv", row.names = FALSE)
                write.csv(jsa_long, "jsa.long.csv", row.names = FALSE)
    
      # -----      
       
