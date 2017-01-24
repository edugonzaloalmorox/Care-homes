# Aggregate variables - prices, controls and instruments 
# Created 23/01/2016
# Modified 24/01/2016


 benefits_year_district = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/benefits_year_district.csv")
 
 
 waves = c("feb2014", "feb2015", "feb2016")
 
 benefits = benefits_year_district %>% filter(date %in% waves)
 
 # recode 
 
 benefits = benefits %>% mutate(wave = ifelse(date == "feb2014", 1,
                                              ifelse(date == "feb2015", 2,
                                                     ifelse(date == "feb2016", 3, "other"))))
 
 write.csv(benefits, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/benefits.csv", row.names = FALSE)  
 
 
 
 
 # link count ratings with benefits
 
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
 
# house prices 
       
       years = c(2014, 2015, 2016)
       
       house_prices = year_house_prices_geocoded %>% filter(year.trans %in% years) 
       av.prices = house_prices %>% group_by(oslaua, year.trans) %>% mutate(price_av = mean(mean_price), trans = sum(n.transactions)) %>% select(oslaua, year.trans, price_av, trans)
       av.prices = unique(av.prices)
       av.prices  = av.prices %>% filter(!grepl("W0", oslaua)) %>% filter(!grepl("S1", oslaua)) # drop observations from Scotland and Wales
       
# link house prices with benefits and ratings 
       
       
       
      