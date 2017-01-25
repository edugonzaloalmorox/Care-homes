
# regression 6 
# Use of another outcomes - quality rating

test_4 = test.5

test_4 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/test_4.csv")
summary(test_4$price_av)

test_4 = test_4 %>% mutate(log.price = log(price_av))
                           

# OLS


# no controls 
linear_good = plm(prop.good ~ log.price, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
summary(linear_good)

linear_bad = plm(prop.bad ~ log.price, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
summary(linear_bad)

linear_outstanding = plm(prop.outstanding ~ log.price, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
summary(linear_outstanding)


# market 
      linear_good_controls = plm(prop.good ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,
                                 data=test_4 , index = c("oslaua", "wave"), model = "pooling")
            summary(linear_good_controls)
      
      linear_bad_controls = plm(prop.bad ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,
                                 data=test_4 , index = c("oslaua", "wave"), model = "pooling")
            summary(linear_bad_controls)
      
      linear_outstanding_controls = plm(prop.outstanding ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,
                                data=test_4 , index = c("oslaua", "wave"), model = "pooling")
      
            summary(linear_outstanding_controls)



# region 
      linear_good_region = plm(prop.good ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
      summary(linear_good_region)
      
      linear_bad_region = plm(prop.bad ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
      summary(linear_bad_region)
      
      linear_outstanding_region = plm(prop.outstanding ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1, data=test_4 , index = c("oslaua", "wave"), model = "pooling")
      summary(linear_outstanding_region)



# IV 

# ------------------------
# labour votes instrument 
# ------------------------

# no controls
      iv_good_lab_free =   ivreg(prop.good ~ log.price | labourvotes1983,  data=test_4) 
      summary(iv_good_lab_free,  diagnostics = TRUE)
      
      iv_bad_lab_free =   ivreg(prop.bad ~ log.price | labourvotes1983,  data=test_4) 
      summary(iv_bad_lab_free,  diagnostics = TRUE)
      
      iv_outstanding_lab_free =   ivreg(prop.outstanding ~ log.price | labourvotes1983,  data=test_4) 
      summary(iv_outstanding_lab_free,  diagnostics = TRUE)

# market controls
    iv_good_lab_controls = ivreg(prop.good ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|
                                   labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test_4) 
    summary(iv_good_lab_controls, diagnostics = TRUE)


    iv_bad_lab_controls = ivreg(prop.bad ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|
                                     labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test_4) 
      summary(iv_bad_lab_controls, diagnostics = TRUE)
      
    
    iv_outstanding_lab_controls = ivreg(prop.outstanding ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|
                                    labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test_4) 
      summary(iv_outstanding_lab_controls, diagnostics = TRUE)


# region controls 
      
              iv_good_lab_regions = ivreg(prop.good ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + Location.Region -1,   data=test_4) 
              summary(iv_good_lab_regions, diagnostics = TRUE)
              
              iv_bad_lab_regions = ivreg(prop.bad ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + Location.Region -1,   data=test_4) 
              summary(iv_bad_lab_regions, diagnostics = TRUE)
              
              iv_outstanding_lab_regions = ivreg(prop.outstanding ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + Location.Region -1,   data=test_4) 
              summary(iv_outstanding_lab_regions, diagnostics = TRUE)



# ---------------
# reform instrument 
# -----------------

# no controls 

        iv_good_reform = ivreg(prop.good~ log.price|delchange_maj1, data = test_4)
        summary(iv_good_reform, diagnostics = TRUE)
        
        iv_bad_reform = ivreg(prop.bad~ log.price|delchange_maj1, data = test_4)
        summary(iv_bad_reform, diagnostics = TRUE)
        
        iv_outstanding_reform = ivreg(prop.outstanding~ log.price|delchange_maj1, data = test_4)
        summary(iv_outstanding_reform, diagnostics = TRUE)

# market controls 
        iv_good_reform9404 = ivreg(prop.good~ log.price +  prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                  data=test_4) 
        summary(iv_good_reform9404, diagnostics = TRUE)
        
        iv_bad_reform9404 = ivreg(prop.bad~ log.price +  prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                   data=test_4) 
        summary(iv_bad_reform9404, diagnostics = TRUE)
        
        iv_outstanding_reform9404 = ivreg(prop.outstanding~ log.price +  prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                  data=test_4) 
        summary(iv_outstanding_reform9404, diagnostics = TRUE)


# region controls 
        
        iv_good_reform9404_regions = ivreg(prop.good ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1|
                                            delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1,  data=test_4) 

        summary(iv_good_reform9404_regions)
        
        iv_bad_reform9404_regions = ivreg(prop.bad ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1|
                                             delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1,  data=test_4) 
        
        summary(iv_bad_reform9404_regions)
        
        iv_outstanding_reform9404_regions = ivreg(prop.outstanding ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1|
                                            delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + Location.Region -1,  data=test_4) 
        
        summary(iv_outstanding_reform9404_regions)
        
        
        
        