# Estimation at district level 
 
# Created: 25/10/2016
# Modified: 3/11/2016


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
    
      p.sample.1 = import("la_district_year.csv")
      
    
      
      # distribution of entries over time 
      
      en = ggplot(p.sample.1, aes(n)) +
        geom_point() + ggtitle("Entries distribution") + facet_wrap(~ year.entry)
      
      
      en
      
      # correlations 
head(p.sample.1)
d = p.sample.1 %>% group_by(la) %>% summarise(entries)
    
      instrs = p.sample.1 %>% select(entries, refusal_maj_7908, log.prices, delchange_maj1, delchange_maj5,
                                delchange_maj6, labourvotes1983)
      
      
      library(corrplot)
      library(psych)
      devtools::install_github("kassambara/ggcorrplot")
      install.packages("PerformanceAnalytics")
      library(ggcorrplot)
      library("PerformanceAnalytics")
      library("Hmisc")
      
      
      M <- round(cor(instrs), 1)
      M
      
      ggcorrplot(M, hc.order = TRUE,
                 method = "circle", lab = TRUE, insig = "blank")
      
      chart.Correlation(M, histogram=FALSE, pch=11)
      
      res2 <- rcorr(as.matrix(M))
      res2
      
      corrplot(M, type="upper", order="hclust", 
                sig.level = 0.01, insig = "blank")
      
      ct = corr.test(M, use = "pairwise",method="pearson",adjust="holm",alpha=.05)
      print(ct,short=FALSE)  
      
   
      rcor(x, method = c("pearson", "kendall", "spearman"))
      corrplot(M, method="number")
      
    
      
      # estimation 
      
  # no controls
  # ...........
      
      
      # OLS
      
            mod_ols1 = plm(entries ~ log.prices, data=p.sample.1,  index = c("la", "year.entry"), model = "pooling")
            mod_ols1_print = summary(mod_ols1)
            mod_ols1_print
      
      # IV #

  # 1. av. refusal rate
  # ...................
            
            mod_ols_iv1 = ivreg(entries ~ log.prices |refusal_maj_7908,   data=p.sample.1) 
            mod_ols_iv1_print = summary(mod_ols_iv1)
            mod_ols_iv1_print
       
    #  Note: There is just one covariate--> Wald test 
            
        
            # test instrument relevance 
            
           sum_mod1 = summary(mod_ols_iv1, diagnostics = TRUE)
            #interpretaion:
            # 'Weak' -  Ho (we have weak instruments); p value < 0.05 reject the Ho
            # 'Wu - Hausman' - Ho consistency of the OLS estimates under 
            #                 the assumption that the IV is consistent. No rejection: OLS and IV are similar
            
           
   # 2. Add more exogeneous regressors
   # .................................
           
          # OLS
           
           mod_ols2 = plm(entries ~ log.prices + mean.people.la + mean.old.la + mean.jsa.la, data=p.sample.1,  index = c("la", "year.entry"), model = "pooling")
           mod_ols2_print = summary(mod_ols2)
           mod_ols2_print
           
           # IV 
           
           mod_ols_iv2 = ivreg(entries ~ log.prices + mean.people.la + mean.old.la + mean.jsa.la|refusal_maj_7908 + mean.people.la + mean.old.la + mean.jsa.la,   data=p.sample.1) 
           mod_ols_iv2_print = summary(mod_ols_iv2)
           mod_ols_iv2_print
           
           
           sum_mod2 = summary(mod_ols_iv2, diagnostics = TRUE)
           
           
             
  # 3. Add more exogeneous variables and an additional instrument (share of labour votes)
  # .....................................................................................
         
            # OLS 
           
           mod_ols3 = plm(entries ~ log.prices + mean.people.la + mean.old.la + mean.jsa.la, data=p.sample.1,  index = c("la", "year.entry"), model = "pooling")
           mod_ols3_print = summary(mod_ols3)
           mod_ols3_print
           
           
           # IV #
          
            mod_ols_iv3 = ivreg(entries ~ log.prices + mean.people.la + mean.old.la + mean.jsa.la |refusal_maj_7908 + labourvotes1983 + mean.people.la + 
                                                                                                   mean.old.la + mean.jsa.la,   data=p.sample.1) 
            mod_ols_iv3_print = summary(mod_ols_iv3)
            mod_ols_iv3_print
           
        
            # test 
            summary(mod_ols_iv3, diagnostics = TRUE)
            
     # 4. Characterise the demand
            
            
         
      
      
            
      
      
      
      

      
      
      
