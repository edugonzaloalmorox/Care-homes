# Analysis of the data 

# created: 10/10/2016
# modified: 18/10/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Analyse the regulatory data with regards to the instruments
#
#-----------------------------------------------------------------------------------------------------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

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
                                    oa11, lsoa11, msoa11, imd) %>% mutate_each(funs(as.Date), date) 
            
            
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

   
          write.csv(cqc_clean, "cqc.prices.pop.csv", row.names = FALSE)
          
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
                muestra = cqc %>% filter(registry == "location.start" & date >= "2011-03-01") %>% arrange(postcode, provider.id)
                
                # number of care homes in the sample 
                muestra_post = with(muestra, unique(location.id)) 
                
                
                # select those locations within the cqc that are in 'muestra_post'
                muestra = cqc_clean %>% filter(location.id %in% muestra_post) # this data sample contains those entries from march 2011 onwards
      
      # 2.  Select those observations that I want to consider 
      #-------------------------------------------------------
      

         # Link information associated with the HHI
                
                hhi = import("data.hhi.lsa.csv")
                                                    
             # clean up hhi   
          
                hhi = hhi %>% mutate_each(funs(as.character), year) %>% select( -beds, -check)    
                
    prueba = left_join(cqc, hhi, by =c("provider.id" = "provider.id", "local.authority" = "local.authority", "year.entry" = "year"))
      
    write.csv(prueba, "cqc.prices.pop_v1.csv", row.names = F)
    
    # Build the sample of analysis
    # ----------------------------
    
    # Note: Depending on the unit of analysis the datasets have a particular structure
    # .................................................................................
    
    
    # LSOA levels 
    
   prueba = import("cqc.prices.pop_v1.csv")
    
    #Select the time frame for the analysis
    ana.1 = prueba %>% filter(registry == "location.start"  & date >= "2011-03-01") %>%
     select(-max.price, -min.price, -house.transactions) %>% arrange(local.authority, date)
   
    # These all referred to LSOA
    ana.1 = ana.1 %>% mutate(prop.pension.crd = (pension.credit/people),
                            prop.income.sup = (income.support/people),
                            prop.jsa.fem = (jsa.fem/people),
                            prop.old = (old/people),
                            size = care.homes.beds, 
                            entry = ifelse(is.na(flow.entry), 0, 1),
                            mean.price = round(mean.price, digits = 0),
                            log.price = log(mean.price)) %>% 
     select(location.id, provider.id, local.authority, entry, lsoa11, msoa11, year.entry, flow.entry, imd, mean.price, log.price, hhi:size)
   
   
   
    # Count the new entries at lsoa level
    # Link with variable at lsoa level
    
    entries.lsoa11 = ana.1 %>% group_by(lsoa11, year.entry, entry) %>% tally %>% 
      filter(entry == "1") %>% arrange(lsoa11, year.entry)
    
    
   ana.1_vars = ana.1 %>% select(local.authority, msoa11, lsoa11, year.entry, imd:size) %>% arrange(lsoa11, year.entry)
    
   entries_lsoa = left_join(entries.lsoa11, ana.1_vars, by = c("lsoa11", "year.entry"))
   
   #dataset at LSOA level
   entries_lsoa = entries_lsoa %>% group_by(lsoa11, year.entry) %>% distinct(.keep_all = TRUE) # remove distinct rows based on the id and year
   
   entries_lsoa = entries_lsoa %>% group_by(year.entry, lsoa11) %>% arrange(lsoa11, year.entry) %>%
     mutate_each(funs(as.numeric), n, imd, mean.price, hhi, prop.pension.crd, prop.income.sup,
                 prop.jsa.fem, prop.old, size)
   
    

#------------------------------ 
# Statistics
#------------------------------   

# ---------------
# PANEL: 1 year   
# ---------------
   
   
# ---------
# Figure 1 #
# ----------
   
fig1 = ggplot(test, aes(x = entry, y = log.price)) +
     geom_boxplot() + geom_jitter(width = 0.00015) +
     facet_grid(. ~ year.entry) 
   
   fig1
   
   
# ----------------------------- 
# Regression multilevel models 
# ----------------------------
   
   
   
   # Grouping years 
   
   mod1 = lmer(entry ~ hhi + log.price + imd + prop.pension.crd + prop.income.sup +
                 prop.jsa.fem + prop.old + size + (1 | year.entry), data = ana.1, family= binomial("probit"))
   summary(mod1)
   
   # Grouping local authorities 
   mod2 = lmer(entry ~ hhi + log.price + imd + prop.pension.crd + prop.income.sup +
                 prop.jsa.fem + prop.old + size + (1 | local.authority), data = ana.1, family= binomial("probit"))
   
   summary(mod2)
   
   # Grouping by local authorities and years 
   
   mod3 = lmer(entry ~ hhi + log.price + imd + prop.pension.crd + prop.income.sup +
                 prop.jsa.fem + prop.old + size + (1 | local.authority) + (1 | year.entry), data = ana.1, family= binomial("probit"))
   
   summary(mod3)
   
   
   # ---------------
   # PANEL: 3 years and two waves - march11 - march 2014 -    
   # ---------------
   
   stargazer(mod1, mod2, mod3)
    
