# Estimation at district level 
 
# Created: 25/10/2016
# Modified: 26/10/2016


# Rationale: the information concerning the planning decisions is at district level. 
# Information referred to instruments is related to district level


# ---------------------------------------
# Load regulatory data - for instruments 
# ---------------------------------------

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
library(AER)
library(Hmisc)
#-------------------------------


      cqc  = import("cqc.prices.pop_v1.csv")
      cqc_geo = import("cqc.geolocated.csv")

# -----------------
# DATA PREPARATION 
# -----------------

      
  # link local district authorities information to cqc 

      cqc_info = cqc_geo %>% select(location.id, dclg.code:la ) # la: district local authority
      
      cqc = left_join(cqc, cqc_info, by = "location.id")
      
      write.csv(cqc, "cqc_prices_pop_district.csv", row.names = FALSE)

# count entries at local authority district level 
      
      # select novo entries
      sam.1 = cqc %>% filter(flow.entry == "novo") %>% select(-reg) 
      
      # count of entries at district level for each year 
      
      entries_year = sam.1 %>% group_by(la, old.la, year.entry, flow.entry) %>% 
        tally() %>% select(-flow.entry) %>% 
        dplyr::rename(entries = n)
      
      # link to the sample data 
      
      sam.1 = left_join(sam.1, entries_year, by = c("la", "old.la", "year.entry"))
    
      # sub-sample 
      
      entries_year = sam.1 %>% filter(date >= "2011-03-01") %>% select(-beds.provider.la, -beds.la, -share, -share2, -hhi)
      
      
  # select sample in terms of district local authority 
      
      entries_la_year = entries_year %>% group_by(old.la, la, year.entry) %>% arrange(old.la, la, date)
      
      # create proportions of population variables at local authority level
      #    data are collected at lsoa level 
      #    calculate the mean number of people of the LSOAS where the care homes are
      #    get the proportion 
      
      entries_la_year = entries_la_year %>% group_by(old.la, la, year.entry) %>%
        dplyr::mutate(mean.people.la = mean(people),
                      mean.old.la = mean(old),
                      mean.jsa.la = mean(jsa.fem),
                      mean.pension.credit.la = mean(pension.credit),
                      mean.income.support.la = mean(income.support),
                      mean.imd = mean(imd),
                      mean.price.la = mean(mean.price))
      
      
      entries_la_year = entries_la_year %>% group_by(old.la, la, year.entry) %>%
        dplyr::mutate(prop.old.la = (mean.old.la/mean.people.la), 
                      prop.jsa.la = (mean.jsa.la/mean.people.la),
                      prop.pension.credit.la = (mean.pension.credit.la/mean.people.la), 
                      prop.income.sup.la = (mean.income.support.la/mean.people.la), 
                      log.prices = log(mean.price),
                      sq.log.price = log(mean.price^2)) %>% arrange(local.authority, date) %>% as.data.frame()
      

      
# insert hhi at city level - TO DO
      
      
  
      
# Prepare the data 
      
      sample.1 = entries_la_year %>% 
        select(old.la: log.prices, year.entry) %>% 
        distinct(old.la,la, year.entry, .keep_all = TRUE)
      
      sample.1 = sample.1 %>% select(old.la, la, year.entry, entries:log.prices)
      
    
# link IV information 
      
      
      instr_cqc = lpa %>% mutate(year.entry = year + 10) %>% select(year, year.entry, lpa_code, lpa_name, rindex2:trafsqtrend)
      
      # subset the instruments according to the time frame
      
      anos = c(2011, 2012, 2013, 2014, 2015, 2016)
      
      instr_cqc_clean = instr_cqc %>% filter(year.entry %in% anos) %>% mutate_each(funs(as.character), year.entry)
      
# link sample of analysis and instruments' information
      
      test = left_join(sample.1, instr_cqc_clean,  by = c("old.la" = "lpa_code", "year.entry" = "year.entry"))
      
# prepare the sample 
      psample = pdata.frame(test, index = c("la", "year.entry"), row.names = TRUE)
      
      p.sample.1 = psample %>% filter(!is.na(lpa_name))
      write.csv(p.sample.1, "la_district_year.csv", row.names = FALSE )
      
      sub_sample = p.sample.1 %>% filter(year.entry != "2011")
      
# ------------
# ESTIMATION 
# -------------     
      # distribution of entries over time 
      
      en = ggplot(p.sample.1, aes(entries)) +
        geom_point() + ggtitle("Entries distribution") + facet_wrap(~ year.entry)
      
      
      
      # correlations 
      
      x = p.sample.1 %>% select(refusal_maj_7908, entries)
      rcor(x, method = c("pearson", "kendall", "spearman"))
      
      
     
      
      # estimation 
      
      
      # OLS
      
      mod_ols1 = lm(entries ~ log.prices, data=p.sample.1)
      mod_ols1_print = summary(mod_ols1)
      
      mod_ols_iv1 = ivreg(entries ~ log.prices |refusal_maj_7908 + delchange_maj5 + delchange_maj6,   data=p.sample.1) 
      iv1 = summary(mod_ols_iv1, vcov = sandwich, diagnostics = TRUE)
      
      
      mod_ols2 = ivreg(entries ~ log.prices + prop.pension.credit.la, data=p.sample.1)
      mod_ols2_print = summary(mod_ols2)
      
      
      mod_ols_iv2 = ivreg(entries ~ log.prices |refusal_maj_7908 + delchange_maj5 + delchange_maj6,   data=p.sample.1) 
      iv2 = summary(mod_ols_iv2, vcov = sandwich, diagnostics = TRUE)
      iv2
      
      # first stage 
      
      h.price = lm(log.prices ~ refusal_maj_7908, data = p.sample.1 )
      summary(h.price)
      
      
      
      
      

      
      
      
