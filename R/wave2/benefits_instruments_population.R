# Link further information: benefits, population, instruments
# Two main issues: 
#               - include information corresponding to the market and instruments 
#               - create variables to reflect characteristics of the market 
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
      test = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices.entries.csv")
      
      
      str(test)
      
      # compare information regarding planning authorities
      old_la = with(test, unique(old.la))
      old_lpa = with(instr, unique(lpa_code))
      
      dif =  setdiff(old_la, old_lpa) # these are the ones that are in cqc but not in the instruments 
      
      
      check = test %>% filter(old.la %in% dif)
      
      #   Note: "Bedford","Central Bedfordshire","Cheshire East","Cheshire West and Chester", "Cornwall",
      #   "County Durham","Isles of Scilly","Northumberland","Shropshire", "Wiltshire"  have changed their structure 
      #   as a result of the 2009 reform. 
      
      
      test = test %>% mutate(lpa.1 = tolower(lpa)) %>% mutate(lpa.1 = gsub(", city of ua", "", lpa.1)) %>%
        mutate(lpa.1 = gsub("ua", "", lpa.1)) %>%
        mutate(lpa.1 = gsub(", county of", "", lpa.1)) %>% 
        mutate(lpa.1 = gsub("[.]", "", lpa.1))%>% 
        mutate(lpa.1 = str_trim(lpa.1, side ="both")) 
      
      instr = instr %>% mutate(lpa.1 = tolower(lpa_name_2005on)) %>% mutate(lpa.1 = gsub(", city of ua", "", lpa.1)) %>%
        mutate(lpa.1 = gsub("ua", "", lpa.1)) %>%
        mutate(lpa.1 = gsub(", county of", "", lpa.1)) %>% 
        mutate(lpa.1 = gsub("[.]", "", lpa.1))%>% 
        mutate(lpa.1 = str_trim(lpa.1, side ="both")) 
      
      instr_cqc = instr %>% filter(year == 2008)
      
      test.1 = left_join(test, instr_cqc, by = "lpa.1")
      
      
      write.csv(test.1, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices_entries_instruments.csv", row.names = FALSE)
      
      
      t = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices_entries_instruments.csv")
      
      t = t %>% select(wave, oslaua, old.la, la, lpa, lpa.1, dclg.code:total_old, refusal_maj_7908:pdevel90_m2, 
                       pop_density_1911_imp:trafsqtrend)
      
      
     # -------
     # Market
     # -------
      
      # create the variables for the sample to be estimated - proportions of people
      
      s1 =  s1 %>% mutate_each(funs(as.numeric), pension_credits, carers_allowance, income_support,
                               jsa_fem, total_pop, total_old)
      
      s1 =  s1 %>% mutate(log.price = log(mean_price), 
                          prop.pension.la = (pension_credits/total_pop),
                          prop.allowance.la = (carers_allowance/total_pop),
                          prop.old.la = (total_old/total_pop),
                          prop.jsa.la = (jsa_fem/total_pop),
                          prop.income.sup.la = (income_support/total_pop))
      
      hhi_waves = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/one/hhi_district_la_wave3.csv")
      
      hhi_waves = hhi_waves %>% filter(year != 0) %>% arrange(lpa) %>% select(-local.authority)
      
      setdiff(unique(hhi_waves$lpa), unique(s1$la))
      
      s2 = left_join(s1, hhi_waves, by = c("la" = "lpa", "wave" = "year"))
      
      check = s3 %>% group_by(la, oslaua) %>% filter(n()>3) 
      
      # remove duplicates 
      
      s3 = s2[-c(172, 284, 321,442), ]
      
      
      # Isles of Scilly - excluded
      
      s4 = s3 %>% filter(la != "Isles of Scilly UA")
      
      write.csv(s4, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices_entries_instruments.csv", row.names = FALSE)
      
      
      
      
     
      