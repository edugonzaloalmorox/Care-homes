# Sample - time frame (1 year)
# ----------------------------

# 



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



      cqc  = import("cqc.prices.pop_v1.csv")
      hhi = import("data.hhi.lsa.wave2.csv")
      hhi1 = import("data.hhi.lsa.csv")


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

####   #####   
# Estimation 
############
     
     
# Prepare the data 
     
  sample.1 = entries_la_year %>% select(local.authority, entries, mean.imd, prop.old.la:hhi, year.entry) %>% distinct(local.authority, year.entry, .keep_all = TRUE)
  
   write.csv(sample.1, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/meetings/sample.1.csv", row.names = F)
     
     
  psample = pdata.frame(sample.1, index = c("local.authority", "year.entry"), row.names = TRUE)
  
# Estimations
  
  mod_fe = plm(entries~ mean.imd + prop.old.la + prop.jsa.la + prop.pension.credit.la + prop.income.sup.la + log.prices + hhi, data=psample, model="within")
  
  summary(mod_fe)
  
  mod_re = plm(entries~ mean.imd + prop.old.la + prop.jsa.la + prop.pension.credit.la + prop.income.sup.la + log.prices  + hhi, data=psample, model="random")
  
  summary(mod_re)

