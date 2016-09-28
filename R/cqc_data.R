# importing CQC data and cleaning some variables 

# created: 27/09/2016
# modified: 27/09/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# This script contains the information associated with the registry of care homes.
# It contains the active and inactive care homes

# Registry that enables to get the entries and exits of care homes.
# Create variables that reflect the entries and the exits. 
# Care homes are identified by postal codes. 
#Some cases have duplicated postal codes - (re) registrations that may reflect different things

# Steps for the analysis:

#     - 1. Identify duplicated observations 
#     - 2. Classify entries depending on _ a) de novo entries b) spurious entries (re -registrations)
#               Note: spurious entries occur for different reasons -  mostly change of the provider
#     - 3. Create a variable that reflects the flow: type of enty and type of exit
#     - 4. Data are transformed into long format

# ---------------
# Data cleaning # 
# ---------------



# load data corresponding to CQC registry and do some cleaning
# -------------------------------------------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/R")



library(readxl)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(rio)
library(magrittr)




data = read_excel("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/20160927_Active and inactive care homes.xlsx", sheet = 2, col_names = TRUE, col_types = NULL)


# load function for renaming variables of teh datset
      rename.var <- function(dta){
        
        x = tolower(colnames(dta))
        x = gsub(" ", ".", x)
        x = gsub("\\s*\\([^\\)]+\\)","",as.character(x))
        x = gsub("-", "", as.character(x))
      
        return(x)
      }
  
# change the name of the care homes 
      
      # makes easier the management of variables 
      # it helps to compare other variables from alternative datasets
      
  cqc = rename.var(data)
  colnames(data) = cqc

# `region` is duplicated 
  
  names(data)[16] <- "location.region" 
  
  setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")
  
  # raw data in csv and cleaned names
  write.csv(data, "data.csv", row.names = F)
  
