# Aggregate variables - prices, controls and instruments 
# Created 23/01/2016
# Modified 24/01/2016

# ---------------------------------------------
# Building the sample for alternative outcomes 
# ---------------------------------------------



 benefits_year_district = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/benefits_year_district.csv")
 
 
 waves = c("feb2014", "feb2015", "feb2016")
 
 benefits = benefits_year_district %>% filter(date %in% waves)
 
 # recode 
 
 benefits = benefits %>% mutate(wave = ifelse(date == "feb2014", 1,
                                              ifelse(date == "feb2015", 2,
                                                     ifelse(date == "feb2016", 3, "other"))))
 
 write.csv(benefits, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/benefits.csv", row.names = FALSE)  
 
 
# -------------------------------- 
# link count ratings with benefits
# --------------------------------
       write.csv(count_ratings, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/countratings_wide.csv", row.names = FALSE)  
       
       
       benefits = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/benefits.csv")
       count_ratings = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/count_ratings.csv")
       
       count_ratings = count_ratings %>% mutate(oslaua = ifelse(is.na(oslaua) & Location.Region == "Yorkshire and The Humber", "E08000034", oslaua))
       count_ratings = count_ratings %>% mutate(oslaua = ifelse(is.na(oslaua) & category == "Bad", "E06000051", oslaua))
       count_ratings = count_ratings %>% mutate(oslaua = ifelse(is.na(oslaua), "E06000020", oslaua))
       
       count_ratings 
       
       prueba = count_ratings %>% select(oslaua:inspections_oslaua)
       
       
       
       library(tidyr)
       prueba = prueba %>% mutate_each(funs(as.factor), oslaua, Location.Region, wave, category)
       
      
       prueba_wide = prueba %>% spread(category,inspections_oslaua) %>% as.data.frame()
       
       prueba_wide = prueba_wide %>% group_by(oslaua, Location.Region, wave) %>% fill(Bad, Good, Outstanding)
       prueba.w = prueba_wide %>% group_by(oslaua, Location.Region, wave) %>% filter(row_number() == n()) %>% as.data.frame()
       
       prueba_wide$wave = as.numeric(prueba_wide$wave) 
       benefits$wave = as.numeric(benefits$wave) 
       
       
       test = left_join(prueba.w, benefits, by = c("oslaua" = "la_code", "wave" = "wave"))
       # there are less observations in test because there are some local authorities that were not inspected during the first wave

       
# ------------ 
# house prices 
# ------------
       
       years = c(2014, 2015, 2016)
       
       house_prices = year_house_prices_geocoded %>% filter(year.trans %in% years) 
       av.prices = house_prices %>% group_by(oslaua, year.trans) %>% mutate(price_av = mean(mean_price), trans = sum(n.transactions)) %>% select(oslaua, year.trans, price_av, trans)
       av.prices = unique(av.prices)
       av.prices  = av.prices %>% filter(!grepl("W0", oslaua)) %>% filter(!grepl("S1", oslaua)) # drop observations from Scotland and Wales
       
       av.prices = av.prices %>% mutate(wave = ifelse(year.trans == 2014, 1,
                                                      ifelse(year.trans == 2015, 2,
                                                      ifelse(year.trans == 2016, 3, "other")))) %>% select(-year.trans)

# -------------------------------------------       
#link house prices with benefits and ratings
# --------------------------------------------
       
       av.prices$wave = as.numeric(av.prices$wave)
       test$wave = as.numeric(test$wave)
       
       test.1 = left_join(test, av.prices, by = c("oslaua", "wave"))
       
       # select information from the parent sample (test_3)
       
      redtest3 = test_3 %>% select(wave:dclg.code, cum_homes, total_pop:trafsqtrend, hhi, people_old65, local.authority)
      
      
      # link, order and clean redundant variables
      # ------------------------------------------
      
      test.2 = left_join(test.1, redtest3, by = c("oslaua", "wave")) %>% select(wave, oslaua, lpa = lpa.x, -lpa.y, la:dclg.code, Location.Region, local.authority, Bad:people_old65)
       
      # create variables of interest
      
      test.2 = test.2 %>% mutate_each(funs(as.numeric), total_pop, total_old, jsa_fem, carers_allowance, pension_credits, income_support) %>%
                                      mutate(prop.old.la = (total_old/total_pop),
                                 prop.jsa.la = (jsa_fem/total_pop),
                                 prop.allowance.la = (carers_allowance/total_pop),
                                 prop.pension.la = (pension_credits/total_pop),
                                 prop.income.sup.la = (income_support/total_pop))
       
      test.2 = test.2 %>% mutate(Outstanding = ifelse(is.na(Outstanding), 0, Outstanding))
      
      # proportion rated care homes 
      
      test.2 = test.2 %>% mutate(prop.good = (Good/cum_homes), 
                                 prop.bad = (Bad/cum_homes),
                                 prop.outstanding = (Outstanding/cum_homes)) %>% select(-date)
       
       
      write.csv(test.2, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/test_4.csv", row.names = FALSE)
      