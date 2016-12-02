# Estimation waves of two years 
# Create a sample considering a wave  of two years 
#       Wave 0 2010 -  march2011
#       Wave 1 april 2011 - march 2013
#       Wave 2 april 2013 - march 2015
#       Wave 3 april 2015 - september 2016
      


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
      


# select location.start

      registros = c("location.start")
      cqc_2p = cqc %>% mutate_each(funs(as.Date), date) %>% filter(registry %in% registros)


      cqc_2p = cqc %>% mutate_each(funs(as.Date), date) %>% filter(registry %in% registros) %>%
                          mutate(wave = ifelse(year.entry >="2010" & date < "2011-03-01", 0,
                                          ifelse(date >= "2011-03-01" & date < "2013-03-01", 1,
                                                ifelse(date>= "2013-03-01" & date < "2015-03-01", 2,
                                                          ifelse(date >= "2015-03-01", 3, "other")))))

# select the levels of the location and subset the geocoded dataset 
    loc_cqc2p = with(cqc_2p, unique(location.id))
   
     dist_la = cqc_geo %>% filter(location.id %in% loc_cqc2p) %>% select(location.id, old.la, la)

      test = left_join(cqc_2p, dist_la, by = "location.id")
      test = unique(test)



setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw")

# change the class of variables 
         test$year.entry = as.numeric(test$year.entry)
          
# link information corresponding to the instruments 
          test.1 = left_join(test, lpa_instr, c("old.la" = "lpa_code", "year.entry" = "year.entry"))
          
# count the number of new entries -  group by wave and local authorities at district level
          y = test.1 %>% dplyr::group_by(flow.entry, wave, la) %>%
                    dplyr::summarise(n = n()) %>% filter(!is.na(flow.entry)) %>% ungroup() %>%
                          select(-flow.entry)
          

          test.1 = left_join(test.1, y, by = c("la", "wave"))

# NA observations are associated with spurious observations 
          test.1 = test.1 %>% mutate(n = ifelse(is.na(n), 0, n)) %>% arrange(wave)
          


           # create variables to 
          test.1 = test.1 %>% group_by(old.la, la, wave) %>%
            dplyr::mutate(mean.people.la = mean(people),
                          mean.old.la = mean(old),
                          mean.jsa.la = mean(jsa.fem),
                          mean.pension.credit.la = mean(pension.credit),
                          mean.income.support.la = mean(income.support),
                          mean.imd = mean(imd),
                          mean.price.la = mean(mean.price))
          
          
          test.1 = test.1 %>% group_by(old.la, la, wave) %>%
            dplyr::mutate(prop.old.la = (mean.old.la/mean.people.la), 
                          prop.jsa.la = (mean.jsa.la/mean.people.la),
                          prop.pension.credit.la = (mean.pension.credit.la/mean.people.la), 
                          prop.income.sup.la = (mean.income.support.la/mean.people.la), 
                          log.prices = log(mean.price.la)) %>% 
                          arrange(old.la, la, date) %>% as.data.frame()
          
          setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")       

          write.csv(test.1, "cqc_prices_instr_pop_v2.csv", row.names = FALSE)      
    
 
          
     sample2 = test.1 %>% filter(wave != 0) %>% select(wave, la:log.prices)
    
    sample2 = sample2 %>% distinct(la, wave, .keep_all = TRUE)
          
    # distribution of entries 
    

      
       sample2$wave = as.factor(sample2$wave)
       
       en <- ggplot(sample2, aes(x=n))
       en + geom_density() + ggtitle("Entries distribution 2011-2016")
       
       
       p1 <- ggplot(sample2, aes(y = log.prices,  x = n)) 
       p1 + geom_point(color="red") +  geom_smooth(method = "lm", se = TRUE)
          
#############
# REGRESSION        
#############
       
          
       # no controls
       # ...........
       
       
       # OLS
       
       mod_ols_s2_1 = plm(n ~ log.prices, data=sample2,  index = c("la", "wave"), model = "pooling")
       mod_ols_s2_1 = summary(mod_ols_s2_1)
       
       
       # IV #
       
       mod_ols_s2_1 = ivreg(n ~ log.prices |refusal_maj_7908,   data=sample2) 
       mod_ols_s2_1_print = summary(mod_ols_s2_1)
       mod_ols_s2_1_print
       
       summary(mod_ols_s2_1, diagnostics = TRUE)
       sum_mod1
        
       
       #more regrsor
       mod_ols2 = plm(n ~ log.prices + mean.people.la + mean.old.la + mean.jsa.la, data=sample2,  index = c("la", "wave"), model = "pooling")
       mod_ols2_print = summary(mod_ols2)
       mod_ols2_print
       
       # IV 
       
       mod_ols_iv2 = ivreg(n ~ log.prices +  mean.old.la + mean.jsa.la|refusal_maj_7908 + labourvotes1983  + mean.old.la + mean.jsa.la,   data=sample2) 
       mod_ols_iv2_print = summary(mod_ols_iv2)
       mod_ols_iv2_print
          
          
          
          
          