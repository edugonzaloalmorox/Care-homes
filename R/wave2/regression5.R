# Regression 5 
# Analysis of different geographical agglomerations
#         - Regressions region by region
#         - Analysis of region variability

library(ggplot2)
library(grid)
library(gridExtra)
library(AER)
library(plm)
library(rio)
library(stargazer)
library(sandwich)
library(dplyr)
library(broom)

test.3 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test.3.csv")

test.3 = test.3 %>% select(wave:local.authority)


  
  # -------
  # regions
  # --------
  
  # ---
  # OLS
  # ---
  
          # no control
          olsregionfree = test.3 %>% group_by(location.region) %>%
            do(fitolsregionfree = plm(homes_pop ~ log.price, data=. , 
                               index = c("lpa", "wave"),
                               model = "pooling"))
          
          df.ols.region.free = tidy(olsregionfree, fitolsregionfree)
          df.ols.region.free
          
          
          # market control
          olsregionmarket = test.3 %>% group_by(location.region) %>%
          do(fitolsregionmarket = plm(homes_pop ~ log.price + prop.old.la + 
                               prop.allowance.la + prop.pension.la + 
                               prop.jsa.la + prop.income.sup.la + hhi , data=. , 
                               index = c("lpa", "wave"), model = "pooling"))
          
          df.reg.ols.market = tidy(olsregionmarket, fitolsregionmarket)
          df.reg.ols.market
  
  # ---
  # IV
  # ---
  
          X = split(test.3, test.3$location.region) 
     
      # Labour votes
      # ------------

          #no control
          iv_labregionfree = lapply(X, function(x) summary(ivreg(homes_pop ~ log.price | labourvotes1983,  data=x), diagnostics = TRUE))
          iv_labregionfree
          
          # market control 
          iv_labregionmarket = lapply(X, function(x) summary(ivreg(homes_pop ~ log.price + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi| 
                                                                     labourvotes1983 + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,  data=x), diagnostics = TRUE))
          iv_labregionmarket
          
          
          
          
      # change of delay 
      # --------------
          
          #no control
          iv_delayregionfree = lapply(X, function(x) summary(ivreg(homes_pop ~ log.price | delchange_maj1,  data=x), diagnostics = TRUE))
          iv_delayregionfree
          
          # market control 
          iv_labregionmarket = lapply(X, function(x) summary(ivreg(homes_pop ~ log.price + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi| 
                                                                     delchange_maj1 + prop.old.la + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi,  data=x), diagnostics = TRUE))
          iv_labregionmarket
          
          
    # ----------------------------------   
    #  variability left after region controls
    # ----------------------------------

          
          # Here we try to check the 
          # ----
          
          mod_outcome = lm(homes_pop ~  oscty -1, data = test.4)
          
          index = c("lpa", "wave"),
                            model = "pooling")
          summary(mod_outcome)
          
          mod_instr1 =  lm(labourvotes1983 ~ oscty -1, data = test.4) 
                           
                           index = c("lpa", "wave"),
                            model = "pooling")
          summary(mod_instr1)
          
          mod_instr2 =  lm(delchange_maj1 ~ oscty -1, data = test.4)
          
          index = c("lpa", "wave"),
                            model = "pooling")
          summary(mod_instr2)
          
          
          
          # create a dataframe with the outcome of the model and plot
          
          
          
          # outcome 
          o = mod_outcome %>% tidy() 
          ro = glance(mod_outcome) %>% mutate(mod = "mod_outcome")
          
          o = cbind(o, ro)
          
          # instrument1
          i1 = mod_instr1 %>% tidy()
          ri1 = glance(mod_instr1) %>% mutate(mod = "mod_i1")
          
          i1 = cbind(i1, ri1)
          
          # instrument2
          i2 = mod_instr2 %>% tidy()
          ri2 = glance(mod_instr1) %>% mutate(mod = "mod_i2")
          
          i2 = cbind(i2, ri2)
          
          # the data frame
          models = rbind(o, i1, i2)
          
          # the plot 
          graph = ggplot(models, aes(r.squared, estimate))  + geom_point(aes(colour = factor(mod)))
          graph 
          
 
# --------------------         
# Further geographies
# --------------------
          
          # County districts --oscty
          
  cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.geolocated.csv") %>% select(oslaua, oscty, osward)
          
   counties = cqc %>% select(oslaua, oscty) %>% unique()       
          
          
    test.4 = left_join(test.4, counties, by = "oslaua")
    
    # IV #
    
    # Labour
    
    #no control
    
    county_iv_lab_free =   ivreg(homes_pop ~ log.price | labourvotes1983,  data=test.4) 
    summary(county_iv_lab_free,  diagnostics = TRUE)
    
    # county control control 
    
    county_iv_lab = ivreg(homes_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + oscty -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + oscty -1,   data=test.4) 
    summary(county_iv_lab, diagnostics = TRUE)
    
    # Delay change 
    
    county_iv_reform = ivreg(homes_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + oscty -1| delchange_maj1 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + oscty -1,   data=test.4) 
    summary(county_iv_reform, diagnostics = TRUE)
    
    
    
    # considering counties as uniit of analyis
    
    county_iv_lab = ivreg(homes_pop ~ log.price + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la + hhi + oscty -1| labourvotes1983 + prop.allowance.la + prop.pension.la + prop.jsa.la + prop.income.sup.la +hhi + oscty -1,   data=test.4) 
    summary(county_iv_lab, diagnostics = TRUE)
    
          
          
       