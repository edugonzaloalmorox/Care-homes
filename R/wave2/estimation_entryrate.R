# estimation- test 3 

# created 14/12/2016
# modified 10/01/2016

library(AER)
library(plm)
library(rio)
library(dplyr)



s1 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices_entries_instruments.csv")

s1 = s1 %>% mutate(entry_rate_nat = entry_rate/100,
                   jsa = prop.jsa.la*100, old = prop.old.la*100, income  = prop.income.sup.la*100)



# correlations - mechanisms 



s3  = smple %>% filter(wave == 3)

p <- ggplot(smple, aes(cum_homes, log.price)) + geom_smooth(method = "lm") + geom_point() + facet_grid(wave ~ .)
p = p +  labs(x="Number of care homes", 
              y = "House prices (log)")  





# subset a sample with the variables 



s1 = smple %>% select(lpa, wave, entry_rate, log.price,  prop.old.la,  prop.jsa.la, prop.income.sup.la,  hhi, 
                        refusal_maj_7908, delchange_maj1, delchange_maj5, delchange_maj6, labourvotes1983)



# linear model
# -------------
          mod_ols4 = plm(entry_rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi, data=s1 , index = c("lpa", "wave"), model = "pooling")
           summary(mod_ols4)
          
           mod_ols4 = plm(entry_rate_nat ~ log.price + old + jsa + income + hhi, data=s1 , index = c("lpa", "wave"), model = "pooling")
           summary(mod_ols4)
           
 
# IV 
 mod_ols_iv4.1 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|refusal_maj_7908 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
 summary(mod_ols_iv4.1, diagnostics = TRUE)

 
 mod_ols_iv4.1 = ivreg(entry_rate_nat~ log.price + old + jsa + income + hhi|refusal_maj_7908 + old + jsa +income +hhi,   data=s1) 
 summary(mod_ols_iv4.1, diagnostics = TRUE)
 
    # labour votes 
    mod_ols_iv4.2 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|refusal_maj_7908 + labourvotes1983 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
    summary(mod_ols_iv4.2, diagnostics = TRUE)


            # maj1 - 94/96 and 04/06
            mod_ols_iv5 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                      refusal_maj_7908 + delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary(mod_ols_iv5, diagnostics = TRUE)
            
            #maj5 - 79/01 and 03/08
            mod_ols_iv6= ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                      refusal_maj_7908 + delchange_maj5 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary(mod_ols_iv6, diagnostics = TRUE)
            
            #maj6 - 96/01 and 03/08
            mod_ols_iv7 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                      refusal_maj_7908 + delchange_maj6 +labourvotes1983+ prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary(mod_ols_iv7, diagnostics = TRUE)



# robustness
            
        # - drop the first 5% and 10% of the sample
            
            s1 <- s1 %>% mutate(decile = ntile(entry_rate_nat, 100))
            s1r = s1 %>% filter(decile >= 5 & decile <= 95)
            
           
            mod_ols4r = plm(entry_rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi, data=s1r , index = c("lpa", "wave"), model = "pooling")
            summary(mod_ols4r)
            
            
            
            # IV 
            mod_ols_iv4.1r = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                    refusal_maj_7908 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi, 
                                  data=s1r) 
            summary(mod_ols_iv4.1, diagnostics = TRUE)
            
            fs_mod4.1 = lm(log.price ~ refusal_maj_7908 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi, 
                           data=s1r) 
            
            
            # refusal + labour votes 
            mod_ols_iv4.2r = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                    refusal_maj_7908 + labourvotes1983 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,      data=s1) 
            summary(mod_ols_iv4.2r, diagnostics = TRUE)
            
            fs_mod4.2 = lm(log.price ~ refusal_maj_7908 + labourvotes1983 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi, 
                           data=s1) 
            
            
            # refusal + maj1 - 94/96 and 04/06
            mod_ols_iv5 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                  refusal_maj_7908 + delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary(mod_ols_iv5, diagnostics = TRUE)
            
            fs_mod5 = lm(log.price ~ refusal_maj_7908 + delchange_maj1+ prop.old.la + prop.jsa.la + prop.income.sup.la +hhi, 
                         data=s1) 
            
            #maj5 - 79/01 and 03/08
            mod_ols_iv6= ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                 refusal_maj_7908 + delchange_maj5 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary(mod_ols_iv6, diagnostics = TRUE)
            
            #labour + maj6 - 96/01 and 03/08
            
            # labour + maj1 94/96 - 04/06
            mod_ols_iv7r = ivreg(entry_rate_nat ~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                  refusal_maj_7908 + delchange_maj1 +labourvotes1983+ prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary(mod_ols_iv7r, diagnostics = TRUE)
            
            fs_mod7 = lm(log.price ~ refusal_maj_7908 + delchange_maj1 +labourvotes1983+ prop.old.la + prop.jsa.la + prop.income.sup.la +hhi, 
                         data=s1) 
            
            summary(fs_mod7)
            
  # estimation test - 4 
  # --------------------
            
        # not considering the refusal rates as an instrument
            
           # linear model 
            
            mod_ols4 = plm(entry_rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi, data=s1 , index = c("lpa", "wave"), model = "pooling")
            summary(mod_ols4) 
          
            
            # IV 
           
            # refusal rate (only)
            # -------------------
             mod_iv_refusal = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|refusal_maj_7908 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
            summary(mod_iv_refusal, diagnostics = TRUE)
            
            
            # labour votes - no refusal rates 
            # --------------------------------
            mod_iv_labour = ivreg(entry_rate_nat ~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi| labourvotes1983 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
            summary(mod_iv_labour, diagnostics = TRUE)
            
            
            # policy reform  - delay rate major projects 
            # -----------------------------------------
            
            # maj1 - 94/96 and 04/06
            mod_iv_reform9404 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                  delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,  data=s1) 
            
            summary(mod_iv_reform9404, diagnostics = TRUE)
            
            #maj5 - 79/01 and 03/08
            mod_iv_reform7903= ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                  delchange_maj5 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary( mod_iv_reform7903, diagnostics = TRUE)
            
            #maj6 - 96/01 and 03/08
            mod_iv_reform9603= ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                   delchange_maj6 + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary( mod_iv_reform9603, diagnostics = TRUE)
            
            # labour votes and policy reform
            # ------------------------------
            
            mod_iv_labreform = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|
                                       delchange_maj1 +labourvotes1983+ prop.old.la + prop.jsa.la + prop.income.sup.la + hhi,   data=s1) 
            
            summary( mod_iv_labreform, diagnostics = TRUE)
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            