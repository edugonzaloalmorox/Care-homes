# Calculate entry rates at district level - this is based on entries_exits_cqc.R
# Steps:  - link geographical information to have the code of English districts -  oslaua 
#         - count entries and exits 
#         - calculate entry rates and exit rates

# Purpose: The final idea is to link the information of the entry rates with the house prices.

# created:  06/12/2016
# modified: 09/12/2016
# @EduGonzalo

##############################
# Add geographical information
###############################

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two")

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

#########################
# Count exits and Enties    
#########################   

  # Steps:
      
      # Count the exits and entries on independent data frames
      # Link entries and exits in a single data frame
      # Calculate entries and exit rates
      
 # ---------  
 #  Exits 
 # ---------
      
  # Steps: 
  #   1. Get those postcodes that have been unregistered 
  #   2. Select the observation that constitutes a definite entry (some postcodes reflect various "exits" -  we are interested in the last one) 
      
      
      # all the exits
      
      exit_all = cqc %>% filter(registry == "location.end") %>% select(location.id,registry, date, postcode, wave, oslaua)
      
      # duplicated postcodes
      
      dup_exits = exit_all %>% group_by(postcode) %>% filter(n()>=2)%>% as.data.frame()
      
      # get those postcodes that have a NA; this means that there is still a care home in that postcode 
      dup_active = dup_exits %>% filter(is.na(date)) %>% mutate(active = "yes")
      
      dup_exits = left_join(dup_exits, dup_active, by = c("location.id", "registry", "date", "postcode", "wave", "oslaua"))
      dup_exits$active = as.factor(dup_exits$active)
      
      # recode those postcodes that are still active - postcodes that have a date and also a missing
      library(data.table)
      
      dup_exits = setDT(dup_exits)[, active1 := levels(droplevels(active)), by = postcode] # active1 informs those postcodes that are still active 
      dup_exits = dup_exits %>% mutate(gone = ifelse(is.na(active1), "exit.def", "no")) %>% select(-active1, -active)
      
      # single postcodes 
      
      # get those that are not NA; postcodes that have been closed
      single_exits = exit_all %>% group_by(postcode) %>% filter(n()<2)%>% mutate(gone = ifelse(!is.na(date), "exit.def", "no")) %>% 
        as.data.frame()         
      
      # link single and duplicated postcodes
      
      exits = rbind(single_exits, dup_exits) 
      
      
      # definite exits - the last postcode that has been registered as an exit (order first - arrange)
      
      exits = exits %>% mutate_each(funs(as.factor), location.id, registry, postcode, oslaua, gone)  %>% arrange(postcode, date)
      
      definite_exits = exits %>% group_by(postcode) %>% filter(row_number()==n())
      
      # get the total exits 
      exits_final = definite_exits %>% group_by(wave, oslaua) %>% filter(gone == "exit.def") %>% tally() %>% 
        arrange(oslaua, wave) %>% select(wave, oslaua, exits = n) %>% as.data.frame()
      
      
      # Expand the data frame to all the waves 
      
      distritos_cqc =  with(exits_final, levels(oslaua))
      check = rep.int(distritos_cqc, 4)
      w = rep(0:3, 325)
      
      data = data.frame(oslaua = check, wave = w) # df with all the districts and waves
      
      # link data frames
      test = left_join(data, exits_final, by = c("oslaua", "wave")) %>% arrange(oslaua, wave)
      
      # recode exits 
      
      test_exits = test %>% mutate(exits = ifelse(is.na(exits), 0, exits))
      
  # --------
  # Entries 
  # --------             
      cqc = import("cqc.geolocated.csv")
      entries1 = import("entries_district_wave2.csv")
      
      # select all entries 
      
      entry_all = cqc %>% filter(registry == "location.start") %>% select(location.id,registry, date, postcode, oslaua, old.la, la, dclg.code)
      entry_all = entry_all %>% mutate_each(funs(as.Date), date)
      
      # create the waves
      
      entry_all= entry_all%>% mutate(wave = ifelse(date < "2011-03-01",0,
                                                   ifelse(date >= "2011-03-01" & date < "2013-03-01", 1, 
                                                          ifelse(date >= "2013-03-01" & date< "2015-03-01", 2,
                                                                 ifelse(date >= "2015-03-01", 3, NA)))))
      
      entry_all = entry_all %>% mutate(wave = ifelse(is.na(wave), 3, wave)) %>% 
        
        
        # duplicated entries 
        dup_entries = entry_all %>% group_by(postcode) %>% arrange(postcode, date) %>% filter(n()>=2)%>% as.data.frame()
      
      # select the earliest observation for each postcode; this is the de "novo" entry
      
      definite_entry = dup_entries %>% group_by(postcode) %>% filter(row_number()==1) %>% mutate(novo_entry = "novo")
      
      dup_entries = left_join(dup_entries, definite_entry, by = c("location.id", "registry", "date", "postcode", "oslaua", "old.la", "la", "dclg.code", "wave"))
      
      # get single novo entries
      
      single_entries = entry_all %>% group_by(postcode) %>% arrange(postcode, date) %>% filter(n()<2)%>% as.data.frame() %>% mutate(novo_entry = "novo")
      
      
      
      # link duplicate and single entries 
      
      entries =  rbind(dup_entries, single_entries) 
      
      # definite entries - the first postcode that has been registered is an entry (order first - arrange)
      
      entries = entries %>% mutate_each(funs(as.factor), location.id, registry, postcode, oslaua, old.la, la, dclg.code, novo_entry)  %>% arrange(postcode, date)
      
      
      entries_final = entries %>% group_by(wave, oslaua, old.la, la, dclg.code) %>% filter(novo_entry == "novo") %>% tally() %>% 
        arrange(oslaua, wave) %>% select(wave, oslaua, old.la, la, dclg.code, entries = n) %>% as.data.frame()
      
      
      
      # Expand the observations corresponding to entries
      
      
      check = rep.int(distritos_cqc, 4)
      w = rep(0:3, 325)
      
      data = data.frame(oslaua = check, wave = w) # df with all the districts and waves
      
      data$wave = as.factor(data$wave)
      
      # link data frames
      test_entries = left_join(data, entries_final, by = c("oslaua", "wave")) %>% arrange(oslaua, wave) # this has all the observations and all the waves
      
      # fill the NAs 
      
      # entries that are NA are transformed into 0 
      
      test_entries = test_entries %>% mutate(entries = ifelse(is.na(entries), 0, entries)) 
      
      
      library(data.table)
      nm1 <-  c("old.la", "la", "dclg.code")
      
      test_entries <-  setDT(test_entries)[, lapply(.SD, function(x) levels(droplevels(x))[1]) , 
                                           by = oslaua, .SDcols = nm1][test_entries,  on = "oslaua"]
      
      test_entries = test_entries[, !grepl("i\\.", names(test_entries)), with = FALSE]
  
  # -----------------------------------   
  # Link datasets of entries and exits 
  # -----------------------------------    
      
      test_exits$wave =as.factor(test_exits$wave)
      
      
      entries_exits = left_join(test_entries, test_exits, by = c("oslaua", "wave"))
      
      
      
      # Save important data    
      
      write.csv(test_entries, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/entries_district_wave2.csv", row.names = FALSE)
      write.csv(test_exits, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/exits_district_wave2.csv", row.names = FALSE)
      write.csv(entries_exits, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/entries.exits_district_wave2.csv", row.names = FALSE)

######################
# Entry and exit rates
######################  
      
  entries_exits = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/entries.exits_district_wave2.csv")
      
      # get the net flow
        entries_exits  = entries_exits %>% group_by(oslaua, old.la, la, wave) %>% mutate(net = entries - exits) %>% as.data.frame()
      
      # get the cumulative sum of the flow
        entries_exits = entries_exits %>% group_by(oslaua, old.la, la) %>%
          dplyr::mutate(cum_homes = cumsum(net))
      
      
      # get the entry rates (entries/incumbent)
      
      entries_exits= entries_exits %>% group_by(la, old.la) %>% dplyr::mutate(entry_rate = round(entries/lag(cum_homes), 4)*100)
      
      write.csv(entries_exits, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/entries_exits_rates_ditricts_waves2.csv", row.names = FALSE)
      
      

      
