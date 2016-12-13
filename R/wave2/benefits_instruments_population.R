# Link further information: benefits, population, instruments

# created 09/12/2016
# modified 13/12/2016



# information concerning entries and house prices 

prices_entries = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/prices_entries_wave2.csv") 


# Further information 
      # ----------
      # Benefits
      # ----------

      
    benefits = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/one/benefits_district.csv")  
      
      prices_entries =  prices_entries %>% filter(wave != 0)
    
      setdiff(levels(as.factor(benefits$la_code)), levels(as.factor(prices_entries$oslaua)))
      # "E09000001": city of london is not included in the entry rates 
      
      
      test = left_join(prices_entries, benefits, by = c("oslaua" = "la_code", "wave" = "wave")) %>% select(wave, oslaua, old.la, la, lpa, dclg.code, mean_price:jsa_fem, -date)

     # ------------
     # Population
     # -----------
      
      population = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/one/population_district.csv")
      
      
      test = left_join(test, population, by = c("oslaua" = "la_code", "lpa" = "lpa"))
      
      
      # ------------
      # Instruments 
      # -------------
      
      instr = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/one/data_LPA.dta")
      instr_cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/instr_cqc.csv")
      
      
      instr = instr %>% mutate(lpa = tolower(lpa_name_short)) %>% mutate(lpa = gsub(", city of ua", "", lpa)) %>%
        mutate(lpa = gsub("ua", "", lpa)) %>%
        mutate(lpa = gsub(", county of", "", lpa)) %>% 
        mutate(lpa = gsub("[.]", "", lpa))%>% 
        mutate(lpa = str_trim(lpa, side ="both")) 
      
      # get the average of the planning variables 
      
      instr = instr %>% select(lpa, refusal_maj_7908:labourvotes1983, -total)
      
      # homogenize the info of LA in test 
      
      test = test %>% mutate(lpa = tolower(la)) %>% mutate(lpa = gsub(", city of ua", "", lpa)) %>%
        mutate(lpa = gsub("ua", "", lpa)) %>%
        mutate(lpa = gsub(", county of", "", lpa)) %>% 
        mutate(lpa = gsub("[.]", "", lpa))%>% 
        mutate(lpa = str_trim(lpa, side ="both")) 
      
      setdiff(unique(test$lpa), unique(instr$lpa))
      
      
     
      