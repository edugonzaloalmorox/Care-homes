
# regression 6 
# Use of another outcomes - quality rating


test.5 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/test_4.csv")


test.5 =  test.5 %>% mutate(good_pop = ((Good/people_old65)*1000),
                            bad_pop = ((Bad/people_old65)*1000),
                            out_pop= ((Outstanding/people_old65)*1000))



test_4 = test.5




summary(test_4$price_av)

test_4 = test_4 %>% mutate(log.price = log(price_av))
                           

# OLS


# no controls 

              linear_good = plm(good_pop ~ log.price, data=test_4 , index = c("oslaua", "wave"), model = "pooling", na.action = na.exclude)
              summary(linear_good)
              
              linear_bad = plm(bad_pop ~ log.price, data=test_4 , index = c("oslaua", "wave"), model = "pooling", na.action = na.exclude)
              summary(linear_bad)
              
              linear_outstanding = plm(out_pop ~ log.price, data=test_4 , index = c("oslaua", "wave"), model = "pooling", na.action = na.exclude)
              summary(linear_outstanding)


# market 
              linear_good_controls = plm(good_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,
                                         data=test_4 , index = c("oslaua", "wave"), model = "pooling", na.action = na.exclude)
                    summary(linear_good_controls)
              
              linear_bad_controls = plm(bad_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,
                                         data=test_4 , index = c("oslaua", "wave"), model = "pooling", na.action = na.exclude)
                    summary(linear_bad_controls)
              
              linear_outstanding_controls = plm(out_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,
                                        data=test_4 , index = c("oslaua", "wave"), model = "pooling", na.action = na.exclude)
              
                    summary(linear_outstanding_controls)

# region
            
              linear_good_region = plm(good_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
              summary(linear_good_region)
              
              linear_bad_region = plm(bad_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
              summary(linear_bad_region)
              
              linear_outstanding_region = plm(out_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
              summary(linear_outstanding_region)

# IV 

# ------------------------
# labour votes instrument 
# ------------------------

# no controls
              iv_good_lab_free =   ivreg(good_pop ~ log.price | labourvotes1983,  data=test_4) 
              summary(iv_good_lab_free,  diagnostics = TRUE)
              
              iv_bad_lab_free =   ivreg(bad_pop ~ log.price | labourvotes1983,  data=test_4) 
              summary(iv_bad_lab_free,  diagnostics = TRUE)
              
              iv_outstanding_lab_free =   ivreg(out_pop~ log.price | labourvotes1983,  data=test_4) 
              summary(iv_outstanding_lab_free,  diagnostics = TRUE)

# market controls
              iv_good_lab_controls = ivreg(good_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|
                                             labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test_4) 
              summary(iv_good_lab_controls, diagnostics = TRUE)
          
          
              iv_bad_lab_controls = ivreg(bad_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|
                                               labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test_4) 
                summary(iv_bad_lab_controls, diagnostics = TRUE)
                
              
              iv_outstanding_lab_controls = ivreg(out_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|
                                              labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test_4) 
                summary(iv_outstanding_lab_controls, diagnostics = TRUE)
          

# region controls 
      
                iv_good_lab_regions = ivreg(good_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + Location.Region -1,   data=test_4) 
                summary(iv_good_lab_regions, diagnostics = TRUE)
                
                iv_bad_lab_regions = ivreg(bad_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + Location.Region -1,   data=test_4) 
                summary(iv_bad_lab_regions, diagnostics = TRUE)
                
                iv_outstanding_lab_regions = ivreg(out_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + Location.Region -1,   data=test_4) 
                summary(iv_outstanding_lab_regions, diagnostics = TRUE)



# ---------------
# reform instrument 
# -----------------

# no controls 

                iv_good_reform = ivreg(good_pop~ log.price|delchange_maj1, data = test_4)
                summary(iv_good_reform, diagnostics = TRUE)
                
                iv_bad_reform = ivreg(bad_pop~ log.price|delchange_maj1, data = test_4)
                summary(iv_bad_reform, diagnostics = TRUE)
                
                iv_outstanding_reform = ivreg(out_pop~ log.price|delchange_maj1, data = test_4)
                summary(iv_outstanding_reform, diagnostics = TRUE)

# market controls 
                
                iv_good_reform9404 = ivreg(good_pop~ log.price +  prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                          data=test_4) 
                summary(iv_good_reform9404, diagnostics = TRUE)
                
                iv_bad_reform9404 = ivreg(bad_pop~ log.price +  prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                           data=test_4) 
                summary(iv_bad_reform9404, diagnostics = TRUE)
                
                iv_outstanding_reform9404 = ivreg(out_pop~ log.price +  prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                          data=test_4) 
                summary(iv_outstanding_reform9404, diagnostics = TRUE)


# region controls 
        
                iv_good_reform9404_regions = ivreg(good_pop ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1|
                                                    delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1,  data=test_4) 
        
                summary(iv_good_reform9404_regions, diagnostics = TRUE)
                
                iv_bad_reform9404_regions = ivreg(bad_pop ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1|
                                                     delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1,  data=test_4) 
                
                summary(iv_bad_reform9404_regions, diagnostics = TRUE)
                
                iv_outstanding_reform9404_regions = ivreg(out_pop ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1|
                                                    delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1,  data=test_4) 
                
                summary(iv_outstanding_reform9404_regions, diagnostics = TRUE)
                

# use counties as control   
                
                # County districts --oscty
                
                cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.geolocated.csv") %>% select(oslaua, oscty, osward)
                
                counties = cqc %>% select(oslaua, oscty) %>% unique()       
                
                
                test_4 = left_join(test_4, counties, by = "oslaua")
                
                iv_good_lab_counties = ivreg(good_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + oscty -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + oscty -1,   data=test_4) 
                summary(iv_good_lab_counties, diagnostics = TRUE)
                
                iv_bad_lab_counties = ivreg(bad_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + oscty -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + oscty -1,   data=test_4) 
                summary(iv_bad_lab_counties, diagnostics = TRUE)
                
                iv_outstanding_lab_counties = ivreg(out_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + oscty -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + oscty -1,   data=test_4) 
                summary(iv_outstanding_lab_counties, diagnostics = TRUE)
                
                
        
        