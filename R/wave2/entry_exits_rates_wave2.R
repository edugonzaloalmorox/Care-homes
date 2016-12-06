# Calculate entry rates at district level - this is based on entries_exits_cqc.R
# Steps: - link geographical information to have the code of English districts -  oslaua 
#        - calculate entry rates and exit rates

# Purpose: The final idea is to link the information of the entry rates with the house prices.

# created:  06/12/2016
# modified: 06/12/2016
# @EduGonzalo


cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.prices.pop_v1.csv")

cqc_geo = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.geolocated.csv")

# select flow and add to the geocoded 

# select information referred to locations rather than providers 
      niveles = c("location.start", "location.end")
      
      cqc_clean = cqc %>% filter(registry %in% niveles) %>% select(-mean.price, -max.price, -min.price, -house.transactions, -beds.provider.la,
                                                                   -beds.la, -share, -share2, -hhi)
    
    
# link additional geographical information and create the waves 
      
      # create waves associated with location entry and location end

      cqc_clean = cqc_clean %>% mutate_each(funs(as.Date), date)
      
      cqc_clean= cqc_clean %>% mutate(wave = ifelse(date < "2011-03-01",0,
                                                     ifelse(date >= "2011-03-01" & date < "2013-03-01", 1, 
                                                            ifelse(date >= "2013-03-01" & date< "2015-03-01", 2,
                                                                   ifelse(date >= "2015-03-01", 3, NA)))))
       
      cqc_clean = cqc_clean %>% mutate(wave = ifelse(is.na(wave), 3, wave))
    

      # select geographical variables -oslaua
      
      geographic = cqc_geo %>% select(location.id, local.authority, oslaua, dclg.code:la)
      
      test = left_join(cqc_clean, geographic, by = c("location.id", "local.authority"))
      
      write.csv(test, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc_geolocation_waves.csv", row.names = FALSE)
      
# Entry and exit rates
# --------------------
 
      # ENTRIES 
      # ------------------------------------
      # Idea: get the number of (novo entries)
      #     - filter location start
      #     - filter those that are flow entries 
      
      counts.entry = test %>% filter(registry == "location.start") %>% filter(!is.na(flow.entry))
      entries = counts.entry.clean %>% group_by(wave, la, old.la) %>% tally() %>% arrange(la, old.la, wave) %>% dplyr::rename(novo_entries = n)
      
      
      
      # select dates of location.end
      # remove NAs- - they are care homes that are still active or where re-registered
      
      counts.exit = test %>% filter(registry == "location.end") %>% mutate(wave = ifelse(date < "2011-03-01", 0, ifelse(date >= "2011-03-01" & date < "2013-03-01", 1,
                                                                                                                        ifelse(date>= "2013-03-01" & date < "2015-03-01", 2,
                                                                                                                               ifelse(date >= "2015-03-01", 3, "other"))))) %>% select(old.la, la, flow, date, wave)
      
      counts.exit.clean = counts.exit  %>% filter(!is.na(flow)) # only definite exits 

  
      