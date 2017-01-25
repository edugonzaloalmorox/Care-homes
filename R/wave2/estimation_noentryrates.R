# estimation test - 4 
# --------------------

library(AER)
library(plm)
library(rio)
library(dplyr)

test.3 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test.3.csv")
 write.csv(test.3, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test.3.csv", row.names = FALSE)

# create dependent variable 
      # number of care homes per 1000 people aged 65 years or more 

      s1 = s1 %>% mutate(homes_pop = (cum_homes/people_old65)*1000)

# consider dummy variables for regions and local authorities
      
      # link regions and local authorities
      
      cqc_geo = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc_geolocation_waves.csv") 
      
      vars = cqc_geo %>% select(oslaua, location.region, local.authority)
      
      vars = unique(vars)
      
      test = left_join(s1, vars, by = "oslaua")
      test = test %>% group_by(oslaua) %>% filter(local.authority != "Unspecified") # clean garbage 
      
      
      test = test %>% mutate_each(funs(as.factor), location.region, local.authority) %>% as.data.frame()
      
      # create the dummy variables
      
      test.1 = model.matrix(~ location.region -1, data = test )
      test.2 = model.matrix(~ local.authority -1, data = test )
      
      test.3 = cbind(test, test.1, test.2)

    
      

# entries as dependent variable 
# -----------------------------

# FIRST STAGE 
      
      # refusal rate
      fs_ref1 = lm(log.price ~ refusal_maj_7908 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1, 
                     data=test.3) 
      summary(fs_ref1)
      
      fs_ref2= lm(log.price ~  refusal_maj_7908 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1 +  local.authority -1, 
                   data=test.3) 
      
      summary(fs_ref2)
                  
      
      # labour votes 
      fs_lab1 = lm(log.price ~ labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test.3 )
      summary(fs_lab1)
      
      waldtest(fs_lab1, .~.-labourvotes1983-prop.allowance.la-prop.pension.la-prop.jsa.la- prop.income.sup.la-hhi)

      
      
      fs_lab2 = lm(log.price ~ labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + location.region -1,   data=test.3 )
      summary(fs_lab2)  
      
      waldtest(fs_lab2, .~.-labourvotes1983-prop.allowance.la-prop.pension.la-prop.jsa.la- prop.income.sup.la-hhi-region-1)
   
      fs_lab3 = lm(log.price ~ labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + local.authority -1,   data=test.3 )
      summary(fs_lab3)  
      
      waldtest(fs_lab2, .~.-labourvotes1983-prop.allowance.la-prop.pension.la-prop.jsa.la- prop.income.sup.la-hhi-region-1)
      
      
      # reform 
      
      fs_reform1 = lm(log.price ~ delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,  data=test.3)
      summary(fs_reform1)
      
      waldtest(fs_reform1, .~.-labourvotes1983-prop.allowance.la-prop.pension.la-prop.jsa.la- prop.income.sup.la-hhi)
      
      fs_reform2 = lm(log.price ~ delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi + location.region - 1,  data=test.3)
      summary(fs_reform2)
      
      fs_reform3 = lm(log.price ~ delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi + local.authority - 1,  data=test.3)
      summary(fs_reform3)
      
      fs_reform4 = lm(log.price ~ delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1 + local.authority - 1,  data=test.3)
      summary(fs_reform4)
      
   
      
      
      
# SECOND STAGE 

  # no controls 
  mod_linear = plm(homes_pop ~ log.price, data=test.3 , index = c("lpa", "wave"), model = "pooling")
  summary(mod1) 
      
  # market 
  mod_linear_controls = plm(homes_pop ~ log.price + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, data=test.3 , index = c("lpa", "wave"), model = "pooling")
  summary(mod_linear_controls) 
  
  # controls 
  mod_linear_region = plm(homes_pop ~ log.price +prop.old.la + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1, data=test.3 , index = c("lpa", "wave"), model = "pooling")
  summary(mod_linear_region) 
  

# IV 

   # refusal rate (only)
        # -------------------
  
  mod_iv = ivreg(homes_pop ~ log.price  |refusal_maj_7908,   data=test.3) 
  summary(mod_iv, diagnostics = TRUE)
  
  mod_iv_refusal = ivreg(homes_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi |refusal_maj_7908 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi,   data=test.3) 
  summary(mod_iv_refusal, diagnostics = TRUE)
  
# region controls
mod_iv_refusal_regions = ivreg(homes_pop~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1 |refusal_maj_7908 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1,   data=test.3) 
summary(mod_iv_refusal_controls, diagnostics = TRUE)
        


 # labour votes - no refusal rates 
 # --------------------------------
        
modivlab =   ivreg(homes_pop ~ log.price | labourvotes1983,  data=test.3) 
summary(modivlab,  diagnostics = TRUE)


mod_iv_labour = ivreg(homes_pop ~ log.price + prop.old.la +  prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi| labourvotes1983 + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test.3) 
summary(mod_iv_labour, diagnostics = TRUE)


# region controls 
mod_iv_labour_regions = ivreg(homes_pop ~ log.price + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1| labourvotes1983 + prop.old.la  + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + location.region -1,   data=test.3) 
        summary(mod_iv_labour_regions, diagnostics = TRUE)

        
# local authorities controls 
mod_iv_labour_loc.auth = ivreg(homes_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + local.authority -1| 
                                 labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + local.authority -1, 
                                  data=test.3)

        summary(mod_iv_labour_loc.auth, diagnostics = TRUE)


        

        # policy reform  - delay rate major projects 
        # -----------------------------------------
        
        
        # no controls 
        
        mod_iv_reform = ivreg(homes_pop~ log.price|delchange_maj1, data = test.3)
        summary(mod_iv_reform, diagnostics = TRUE)
        
        # market controls 
        mod_iv_reform9404 = ivreg(homes_pop~ log.price + prop.allowance.la +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi|delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi, 
                                  data=test.3) 
        
        summary(mod_iv_reform9404, diagnostics = TRUE)
        
        # region controls 
        mod_iv_reform9404_regions = ivreg(homes_pop ~ log.price +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1|delchange_maj1 +  prop.allowance.la  +prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + location.region -1,  data=test.3) 
        summary(mod_iv_reform9404_regions, diagnostics = TRUE)
        
        
        # labour votes and policy reform
        # ------------------------------
        
        mod_iv_labrefor = ivreg(homes_pop~ log.price |
                                  delchange_maj1 +labourvotes1983 + prop.jsa.la + prop.allowance.la + prop.income.sup.la + hhi,   data=test.3) 
        
        summary(mod_iv_labrefor, diagnostics = TRUE)
        
        
        
        mod_iv_labreform = ivreg(homes_pop~ log.price + prop.jsa.la + prop.allowance.la +  prop.income.sup.la + hhi|
                                   delchange_maj1 +labourvotes1983 + prop.jsa.la + prop.allowance.la + prop.income.sup.la + hhi,   data=test.3) 
        
        summary(mod_iv_labreform, diagnostics = TRUE)
        
        # region controls 
        
        mod_iv_labreform_controls = ivreg(homes_pop~ log.price + prop.jsa.la + prop.allowance.la + prop.income.sup.la + hhi + location.region -1|
                                   delchange_maj1 +labourvotes1983+  prop.jsa.la + prop.allowance.la +  prop.income.sup.la + hhi + location.region -1,   data=test.3) 
        
        summary( mod_iv_labreform_controls, diagnostics = TRUE)
        
        # local authorities controls
        
        mod_iv_labreform_loc.auth = ivreg(homes_pop~ log.price + prop.jsa.la + prop.allowance.la + prop.income.sup.la + hhi + local.authority -1|
                                            delchange_maj1 +labourvotes1983+ prop.old.la + prop.jsa.la + prop.allowance.la +  prop.income.sup.la + hhi + local.authority -1,   data=test.3) 
        
        summary( mod_iv_labreform_loc.auth, diagnostics = TRUE)
        

        
  