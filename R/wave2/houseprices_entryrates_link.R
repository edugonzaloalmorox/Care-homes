#############################################
# Link house prices and care home entry rates
#############################################

# I link data corresponding to entry rates and house prices


# created:  09/12/2016
# modified: 09/12/2016
# @EduGonzalo

    # load data of house prices and entry rates 
    house_prices = import("house_prices_districts_waves.csv") # house prices
    entries_exit = import("entries_exits_rates_ditricts_waves2.csv") # entry rates


#  Link prices and care homes entries 
# ------------------------------------

  # check that there are the same oslaua levels in both data frames
  setdiff(levels(as.factor(house_prices$oslaua)), levels(as.factor(entries_exits$oslaua))) # check that there are the same oslaua levels in both data frames 

# note: E09000001 (city of london) is not in the cqc level 

    house_prices$wave = as.factor(house_prices$wave) # make waves objects of the same class - e.g. factors 
    
    # link and get rid of city of london 
    prices_entries = left_join(house_prices, entries_exit, by = c("oslaua", "wave")) %>% filter(oslaua != "E09000001")
    
    # data frame with prices and entries
    write.csv(prices_entries, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/prices_entries_wave2.csv", row.names = FALSE)
    

      
     
      
      
