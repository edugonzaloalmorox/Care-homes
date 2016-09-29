# Management L&B information - cleaning data 

# created: 27/09/2016
# modified: 28/09/2016
# author: Eduardo Gonzalo Almorox (Newcastle University)
# -------------------------------------------------------

# This dataset contains information of L&B registry from 2012/14
# Prepare variables for linkage with data from the CQC
# Some variables are factors that have the same level with different codes; I have to recode variables
# Data are referred to England, Scotland, Wales, Northern Ireland and Isle of Wight


library(dplyr)
library(rio)
library(magrittr)
library(stringr)


setwd("/Users/Personas/My Cloud/PhD_september_1/data /projects /laing buisson/care_homes")



# ---------------------------------------
# Prepare data for linking both datasets  
# ---------------------------------------

ch.sample = import("ch.sample.csv") 

      # - clean variables that have different levels in the factors 

          ch.sample$purpose.built %<>% factor
          ch.sample$regist.code %<>% factor

      # clean some crap and recode some variables

      # some variables present different levels that are referred to the same thing :: recode

          ch.sample$purpose.built = plyr:: revalue(ch.sample$purpose.built, c("No"="N", "Yes"="Y"))
          ch.sample$purpose.built[ch.sample$purpose.built == ""] <- NA
          
          ch.sample$regist.code[ch.sample$regist.code == "PC"] <- "R"



write.csv(ch.sample, "ch.sample.csv", row.names = FALSE)                    

      # - select variables that are used for the analysis
      # - select only unique variables - the information is repeated over the years
      # - identify those that are duplicates and select only one observation 
    
      ch.sample = import("ch.sample.csv") 
       
      
# remove regions other than england
# ---------------------------------
      
        apestados = c("Wales", "Scotland", "Northern Ireland & Isle of Man")
        
        ch.sample= ch.sample %>% group_by(postcode, care.home.name) %>%
          filter(!(region %in% apestados)) %>% mutate_each(funs(as.factor), care.home.name:purpose.built) 
          
          
          ch.sample$date.registration = strptime(ch.sample$first.reg.date, format ="%d/%m/%Y")
          ch.sample$date.registration = as.Date(ch.sample$date.registration)
          
          ch.sample = as.data.frame(ch.sample)
          
          str(ch.sample)
              
          
          # change some variables 
          ch.sample$single.rooms %<>% factor
          ch.sample$ensuite.rooms %<>% factor
          
          
          ch.sample$single.rooms[ch.sample$single.rooms == ""] <- NA
          ch.sample$ensuite.rooms[ch.sample$ensuite.rooms == ""] <- NA
          
          
          

# fill the information of observations that is missing 
# -------------------------------------------------
      
    # variables that are unlikely to change over time
              
              # rooms 
              # purpose built 
                
          ch.sample = ch.sample %>%
            group_by(postcode, care.home.name) %>%
            mutate(single.rooms=na.omit(single.rooms)[1])
          
          ch.sample = ch.sample %>%
            group_by(postcode, care.home.name) %>%
            mutate(ensuite.rooms=na.omit(ensuite.rooms)[1])
          
          ch.sample = ch.sample %>%
            group_by(postcode, care.home.name) %>%
            mutate(purpose.built=na.omit(purpose.built)[1])
          
        
          
  # select unique rows based on several variables 
          
          # select variables 
      
          ch.sample = ch.sample%>% 
            select(postcode, care.home.name, year, date.registration,
                   purpose.built, regist.code, regist.type, primary.client.code,
                   sector.code, region, single.rooms, ensuite.rooms, care.standards.office,
                   care.contracting.unit, res.fees.sing.min.clean:nurs.fees.shar.max.clean)
          
          write.csv(ch.sample, "ch.sample_clean.csv", row.names = FALSE) 
        
        
          
            
       