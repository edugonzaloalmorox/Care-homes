# Sample - time frame (1 year)
# ----------------------------





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


  # Load data 
      cqc  = import("cqc.prices.pop_v1.csv")
      hhi = import("data.hhi.lsa.wave2.csv")
      hhi1 = import("data.hhi.lsa.csv")
      instr_cqc = import("instr_cqc.csv")
      


  # select novo entries 
      
     sam.1 = cqc %>% filter(flow.entry == "novo") %>% select(-reg)
     

     
# count the entries in each local authority and year 
     
     entries_year = sam.1 %>% group_by(local.authority, year.entry, flow.entry) %>% 
       tally() %>% select(-flow.entry) %>% 
       dplyr::rename(entries = n)
     
     
     sam.1 = left_join(sam.1, entries_year, by = c("local.authority", "year.entry"))
     
  # sub-sample 
     
     entries_year = sam.1 %>% filter(date >= "2011-03-01") %>% select(-beds.provider.la, -beds.la, -share, -share2, -hhi)
     
  # select sample in terms of local authority 

     entries_la_year = entries_year %>% group_by(local.authority, year.entry) %>% arrange(local.authority, date)
     
     # create proportions of population variables at local authority level
     #    data are collected at lsoa level 
     #    calculate the mean number of people of the LSOAS where the care homes are
     #    get the proportion 
     
     entries_la_year = entries_la_year %>% group_by(local.authority, year.entry) %>%
       dplyr::mutate(mean.people.la = mean(people),
         mean.old.la = mean(old),
              mean.jsa.la = mean(jsa.fem),
              mean.pension.credit.la = mean(pension.credit),
              mean.income.support.la = mean(income.support),
              mean.imd = mean(imd),
              mean.price.la = mean(mean.price))
     
     
     entries_la_year = entries_la_year %>% group_by(local.authority, year.entry) %>%
       dplyr::mutate(prop.old.la = (mean.old.la/mean.people.la), 
              prop.jsa.la = (mean.jsa.la/mean.people.la),
              prop.pension.credit.la = (mean.pension.credit.la/mean.people.la), 
              prop.income.sup.la = (mean.income.support.la/mean.people.la), 
              log.prices = log(mean.price),
              sq.log.price = log(mean.price^2)) %>% arrange(local.authority, date) %>% as.data.frame()

     
    # link hhi -one year 

          # compare levels of the local authorities in both datasets 
     
          la.cqc = with(entries_la_year, unique(local.authority))
          la.cqc.all = with(cqc, unique(local.authority))
          la.hhi = with(hhi1, unique(local.authority))
          
          # local authorities that don´t have entries in sample of analysis 
          no.la = setdiff(la.cqc.all, la.cqc) 
      
     
     hhi_year_la = hhi1 %>% distinct(local.authority, year, hhi) %>% filter(year != 2010 & !(local.authority %in% no.la))
     
     hhi_year_la = hhi_year_la %>% mutate_each(funs(as.character), year)
     
     
     entries_la_year = left_join(entries_la_year, hhi_year_la, by= c("local.authority" =  "local.authority", "year.entry" = "year"))


     
# Add information regarding instruments
# --------------------------------------
     
  sample.1 = import("sample.1.csv")
  instr_cqc =  import("instr_cqc.csv")
     
  # Instruments are  10 years - create 'year.entry' for reflecting this gap
     
     instr_cqc = instr_cqc %>% mutate(year.entry = year + 10) %>% select(year, year.entry, local.authority, lpa_code:la)
     
     # subset the instruments
     
     anos = c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
     
     instr_cqc_clean = instr_cqc %>% filter(year.entry %in% anos) %>% mutate_each(funs(as.character), year.entry)
     
     instrumentos = instr_cqc_clean %>% select(year.entry, year, local.authority, rindex2:trafsqtrend, -total)

     instr_link = instrumentos %>% select(year, year.entry, local.authority, rindex2, lrindex2, male_earn_real, lmale_earn_real,
                                          refusal_maj_7908, delchange_maj1, pdevel90_m2, range_meters, pop_density_1911_imp, labourvotes1983)
     
     
     cumbria = instr_cqc %>% filter(local.authority == "Cumbria")
     
     # link instruments data and data from cqc (in sam.1)
     
     
     test = left_join(sample.1, instr_link, by = c("local.authority", "year.entry"))
    
     
check = test %>% filter(is.na(rindex2))
    
     
     
  # Link IV
   
  prueba = left_join(entries_la_year, instr_cqc, by = c("old.la" = "local.authority", "year.entry" = "year.entry.10") )
  
     
     
####   #####   
# Estimation 
############
     
     
# Prepare the data 
     
  sample.1 = entries_la_year %>% select(local.authority, entries, mean.imd, prop.old.la:hhi, year.entry) %>% distinct(local.authority, year.entry, .keep_all = TRUE)
  
   write.csv(sample.1, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/meetings/sample.1.csv", row.names = F)
     
     
  psample = pdata.frame(sample.1, index = c("local.authority", "year.entry"), row.names = TRUE)
  
# Estimations
  
  # OLS 
  
  mod_ols = lm(entries ~ log.prices, data=psample)
  summary(mod_ols)
  
  
  
  
  
  mod_fe = plm(entries~ mean.imd + prop.old.la + prop.jsa.la + prop.pension.credit.la + prop.income.sup.la + log.prices + hhi, data=psample, model="within")
  
  summary(mod_fe)
  
  mod_re = plm(entries~ mean.imd + prop.old.la + prop.jsa.la + prop.pension.credit.la + prop.income.sup.la + log.prices  + hhi, data=psample, model="random")
  
  summary(mod_re)

