# Build the sample for waves at district level

##############
# house prices 
##############
setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves")

prices = import("prices2010.csv")
data = read.csv("/Users/Personas/Downloads/ONSPD_AUG_2016_UK_V2/Data/ONSPD_AUG_2016_UK.csv") 


# create waves in the house prices

district_prices = prices %>% mutate_each(funs(as.Date), trans.date) %>%
  mutate(wave = ifelse(trans.date < "2011-03-01", 0,
                       ifelse(trans.date >= "2011-03-01" & trans.date < "2013-03-01", 1,
                              ifelse(trans.date>= "2013-03-01" & trans.date < "2015-03-01", 2,
                                     ifelse(trans.date >= "2015-03-01", 3, "other")))))




# get the average, max and minimum price 
  district_prices = district_prices %>%
  group_by(district, wave) %>%
  dplyr::summarise(mean_price = mean(price), max_price = max(price), min_price = min(price), trans = n()) %>%
  select(district, wave, mean_price, max_price, min_price, trans) %>%
  as.data.frame()

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")
  write.csv(district_prices, "house_prices_waves_district.csv")
  
 

##########
# benefits 
##########
  
setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves")
  
    pension_credits = read.csv("pension_credits_district.csv", skip =1,  sep = ",", header = FALSE)
    carers_allowance = read.csv("carers_allowance_district.csv", skip =1,  sep = ",", header = FALSE)
    income_support = read.csv("income_support_district.csv", skip =1,  sep = ",", header = FALSE)
    jsa_fem =read.csv("jsa.fem.district.csv", skip =1,  sep = ",", header = FALSE)
    population = read.csv("population_district.csv", skip =1,  sep = ",", header = FALSE)
    
    pension <- stringr::str_split_fixed(pension_credits$V1, ",", 10)
    carers <- stringr::str_split_fixed(carers_allowance$V1, ",", 10)
    income <- stringr::str_split_fixed(income_support$V1, ",", 10)
    jsa <- stringr::str_split_fixed(jsa_fem$V1, ",", 10)
    pop <- stringr::str_split_fixed(population$V1, ",", 6)



    pension = as.data.frame(pension)
    carers = as.data.frame(carers)
    income = as.data.frame(income)
    jsa = as.data.frame(jsa)
    pop = as.data.frame(pop)

# some names are conflictive - eliminate conflictive elements 
conflictivos = c("Bristol", "Kingston upon Hull", "Herefordshire")

      #pension information
      pension_conf = pension %>% filter(V1 %in% conflictivos) %>% select(-V2)
      pension = pension %>% filter(!(V1 %in% conflictivos)) %>% select(-V10)
      
      colnames(pension) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                             "feb2014", "feb2015", "feb2016")
      
      colnames(pension_conf) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                                  "feb2014", "feb2015", "feb2016")
      
      
      pension = rbind(pension, pension_conf)
      pension = pension %>% arrange(lpa)

      pension = pension %>%  mutate(la_code = gsub("[[:punct:]]", "", la.code)) %>% select(-la.code)

      # carers
      carers_conf = carers %>% filter(V1 %in% conflictivos) %>% select(-V2) # V2 has the information of city and council of - garbage 
      carers = carers %>% filter(!(V1 %in% conflictivos)) %>% select(-V10)
      
      colnames(carers) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                             "feb2014", "feb2015", "feb2016")
      
      colnames(carers_conf) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                                  "feb2014", "feb2015", "feb2016")
      
      
      carers = rbind(carers, carers_conf)
      carers = carers %>% arrange(lpa)
      
      carers = carers %>%  mutate(la_code = gsub("[[:punct:]]", "", la.code)) %>% select(-la.code)
      
      #income 
      income_conf = income %>% filter(V1 %in% conflictivos) %>% select(-V2)
      income = income %>% filter(!(V1 %in% conflictivos)) %>% select(-V10)
      
      colnames(income) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                            "feb2014", "feb2015", "feb2016")
      
      colnames(income_conf) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                                 "feb2014", "feb2015", "feb2016")
      
      
      income = rbind(income, income_conf)
      income = income %>% arrange(lpa)
      
      income = income %>%  mutate(la_code = gsub("[[:punct:]]", "", la.code)) %>% select(-la.code)
      
      
      # jsa 
      jsa_conf = jsa %>% filter(V1 %in% conflictivos) %>% select(-V2)
      jsa = jsa %>% filter(!(V1 %in% conflictivos)) %>% select(-V10)
      
      colnames(jsa) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                            "feb2014", "feb2015", "feb2016")
      
      colnames(jsa_conf) <- c("lpa", "la.code", "feb2010", "feb2011", "feb2012", "feb2013",
                                 "feb2014", "feb2015", "feb2016")
      
      
      jsa = rbind(jsa, jsa_conf)
      jsa =  jsa %>% arrange(lpa)
      
      jsa = jsa %>%  mutate(la_code = gsub("[[:punct:]]", "", la.code)) %>% select(-la.code)
      
      # population 
      
      pop_conf = pop %>% filter(V1 %in% conflictivos) %>% select(-V2)
      pop = pop %>% filter(!(V1 %in% conflictivos)) %>% select(-V6)
      
      colnames(pop) <- c("lpa", "la.code", "total_pop", "pop_85", "pop_90")
      
      colnames(pop_conf) <- c("lpa", "la.code", "total_pop", "pop_85", "pop_90")
      
      
      pop = rbind(pop, pop_conf)
      pop =  pop %>% arrange(lpa)
      
      pop = pop %>% mutate_each(funs(as.numeric), pop_85, pop_90) %>% mutate(total_old = pop_85 + pop_90)%>% 
        select(lpa, la_code, total_pop:total_old)%>%mutate(la_code = gsub("[[:punct:]]", "", la.code))  
        
write.csv(pop, "population_district.csv", row.names = FALSE)
     

rm(carers_allowance, income_support, pension_credits, 
   jsa_fem, jsa_conf, carers_conf, income_conf, pension_conf, conflictivos)

library(tidyr)


    pension = pension %>% gather(date, pension_credits, feb2010:feb2016)
    carers = carers %>% gather(date, carers_allowance, feb2010:feb2016)
    income = income %>% gather(date, income_support,  feb2010:feb2016)
    jsa = jsa %>% gather(date, jsa_fem,  feb2010:feb2016)




# merge datasets in order to collect all the information concerning benefits 


      list_df <- list(pension,carers,income, jsa)
      
      benefits = import("benefits_district.csv")
      
      
      benefits <- Reduce(function(...) merge(..., all=T), list_df)
      head(benefits)
      
      setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")
      write.csv(benefits, "benefits_year_district.csv", row.names = FALSE)


# create the waves of benefits 
#-----------------------------
      # wave 1: march 11 - march 13 - take feb2013
      # wave 2: april 13 - march 15 - take feb2015
      # wave 3: april 15 - sept 16 - take feb2016

  #select months 

meses = c("feb2013", "feb2015", "feb2016")

benefits_waves = benefits %>% filter(date %in% meses) %>% 
  mutate(wave = ifelse(date == "feb2013", 1, ifelse(date == "feb2015", 2, ifelse(date == "feb2016", 3, "other"))))
  
setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves")
write.csv(benefits_waves, "benefits_district.csv", row.names = FALSE)


entry = import("entries_exits_rates_ditricts_waves.csv")
prices = import("house_prices_waves_district.csv")

####################################
# link prices, benefits and entries 
###################################

# clean crap in entry, prices and benefits
entry = entry %>% mutate(lpa = tolower(la)) %>% mutate(lpa = gsub(", city of ua", "", lpa)) %>%
                                                mutate(lpa = gsub("ua", "", lpa)) %>%
                                                mutate(lpa = gsub(", county of", "", lpa)) %>% 
                                                mutate(lpa = gsub("[.]", "", lpa))%>% 
                                                mutate(lpa = str_trim(lpa, side ="both")) 

write.csv(entry, "entries_exits_rates_ditricts_waves.csv", row.names = FALSE)

prices = prices %>% mutate(lpa = tolower(district)) %>% mutate(lpa = gsub("city of ", "", lpa)) %>% select(-V1)
                                                                                                           

benefits_waves = benefits_waves %>% mutate(lpa = tolower(lpa)) %>% 
                                    mutate(lpa = gsub("[`]", "'", lpa)) %>%
                                    mutate(lpa = gsub("[.]", "", lpa))

diff_districts = setdiff(unique(test$lpa), unique(benefits_waves$lpa))


entry_clean = entry %>% filter(wave != 0) 
prices_clean = prices %>% filter(wave != 0)

# link datasets


test = left_join(entry_clean, prices_clean, by = c("lpa", "wave"))

test.1 = left_join(test, benefits_waves, by = c("lpa", "wave"))

write.csv(test.1, "entries_prices_benefits_district.csv", row.names = FALSE)
test.1 = import( "entries_prices_benefits_district.csv")

######################
# link population data
#####################

test.2 = left_join(test.1, pop, by = "la_code") %>% select(wave, la, lpa = lpa.x, district, old.la, la_code, novo_entries:total_old, -lpa.y)
write.csv(test.2, "entries_prices_benefits_district.csv", row.names = FALSE)

##################################
# link instrumental variables data 
##################################

lpa_instr = import("data_LPA.dta")
 anos = c(2003, 2005, 2006)
 
 lpa_instr = lpa_instr %>% filter(year %in% anos) %>% mutate(wave = ifelse(year == 2003, 1, ifelse(
   year == 2005, 2, ifelse(year ==2006, 3, "other")))) %>% mutate(lpa = tolower(lpa_name_short)) %>% mutate(lpa = gsub(", city of ua", "", lpa)) %>%
 mutate(lpa = gsub("ua", "", lpa)) %>%
   mutate(lpa = gsub(", county of", "", lpa)) %>% 
   mutate(lpa = gsub("[.]", "", lpa))%>% 
   mutate(lpa = str_trim(lpa, side ="both")) 

 common_districts = intersect(unique(test.1$old.la), unique(lpa_instr$lpa_code))
 common_districts
 
 
 # first by code 
 
 instr_test = lpa_instr %>% select(wave, lpa, year, lpa_code, county_pre_96, lpa_name_2005on, male_earn_real:labourvotes1983)
 
 
 linked = left_join(test.2, instr_test, by = c("old.la" ="lpa_code", "wave" = "wave")) %>% select(wave:lpa.x, lpa.y, lpa_name_2005on, district:labourvotes1983)
 

write.csv(linked, "sample_entry.csv", row.names = FALSE)    




 


 