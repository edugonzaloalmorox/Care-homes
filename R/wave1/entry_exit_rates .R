# Calculate the number of entries throughout the market entry rates. 

# Created: (8/11/2016)
# Edu Gonzalo Almorox 



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


cqc = import("cqc_prices_pop_district.csv")



# Entry rate - new entrants /incumbents 
#######################################
   
# 1. select location.start and location.end
# -----------------------------------------

      location_flow = c("location.start", "location.end")
      
      cqc = cqc %>% mutate_each(funs(as.Date), date) %>% filter(registry %in% location_flow) 
        
  
# 2. get the novo entries and definite exits 
# ------------------------------------------

      # get the last observation
      
      last = cqc %>% group_by(postcode) %>% arrange(postcode, date) %>% filter(row_number()==n()) %>%
        mutate(flow = ifelse(!is.na(date), "exit.def", flow.entry))  %>%
        select(location.id, location.name, provider.id, provider.name,
               postcode, registry, flow.entry, flow, date)

# link the information 

      test = left_join(cqc, last, by = c("location.id", "location.name", "provider.id", "provider.name",
                                         "postcode", "registry", "flow.entry", "date"))
      
      test = test %>% select(location.id:reg, flow.entry, flow, postcode:la) %>% mutate(flow = ifelse(is.na(flow), flow.entry, flow)) %>%
        select(-flow.entry, -reg)


# 3. create the waves - based on the date of location entry
# ----------------------------------------------------------

    # select dates when the location registered
    # drop NAs - they are care homes already in the market that got re-registered

      counts.entry = test %>% filter(registry == "location.start") %>% mutate(wave = ifelse(year.entry >="2010" & date < "2011-03-01", 0,
                           ifelse(date >= "2011-03-01" & date < "2013-03-01", 1,
                                  ifelse(date>= "2013-03-01" & date < "2015-03-01", 2,
                                         ifelse(date >= "2015-03-01", 3, "other"))))) %>% select(old.la, la, flow, date, wave)
    
      counts.entry.clean = counts.entry %>% filter(!is.na(flow)) # only new entries 

  # select dates of location.end
  # remove NAs- - they are care homes that are still active or where re-registered

      counts.exit = test %>% filter(registry == "location.end") %>% mutate(wave = ifelse(date < "2011-03-01", 0, ifelse(date >= "2011-03-01" & date < "2013-03-01", 1,
                                                                                              ifelse(date>= "2013-03-01" & date < "2015-03-01", 2,
                                                                                                     ifelse(date >= "2015-03-01", 3, "other"))))) %>% select(old.la, la, flow, date, wave)
    
      counts.exit.clean = counts.exit  %>% filter(!is.na(flow)) # only definite exits 




# 4. get entry rates (entries/incumbent)
# --------------------------------------

      entries = counts.entry.clean %>% group_by(wave, la, old.la) %>% tally() %>% arrange(la, old.la, wave) %>% dplyr::rename(novo_entries = n)
      
      exits = counts.exit.clean %>% group_by(wave, la, old.la) %>% tally() %>% arrange(la, old.la, wave) %>% dplyr::rename(def_exit = n)
                                                                                                                           
  entry_rate = left_join(entries, exits, by = c("wave", "la", "old.la"))   
  
  entry_rate = entry_rate %>% mutate(def_exit = ifelse(is.na(def_exit), 0, def_exit))
  
write.csv(entry_rate, "entries.exits.waves.district.csv", row.names = FALSE)


entry_rate = import("entries.exits.waves.district.csv")


entry_rate = entry_rate %>% group_by(la, old.la, wave) %>% mutate(net = novo_entries - def_exit) %>% as.data.frame()


str(entry_rate)

entry_rate = entry_rate %>% mutate_each(funs(as.factor), la, old.la)


entry_rate = entry_rate %>% group_by(la, old.la) %>%
  dplyr::mutate(cum_homes = cumsum(net))
   
entry_rate = entry_rate %>% group_by(la, old.la) %>% dplyr::mutate(rate = round(novo_entries/lag(cum_homes), 4)*100)
  
  write.csv(entry_rate, "entries_exits_rates_ditricts_waves.csv", row.names = FALSE)


summary(entry_rate)
                                                                                                                     