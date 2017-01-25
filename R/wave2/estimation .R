# estimation- test 3 

# created 14/12/2016
# modified 14/12/2016

library(AER)
library(plm)



s1 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices_entries_instruments.csv")

s1 = s1 %>% mutate(entry_rate_nat = entry_rate/100)



# correlations - mechanisms 



s3  = smple %>% filter(wave == 3)

p <- ggplot(smple, aes(cum_homes, log.price)) + geom_smooth(method = "lm") + geom_point() + facet_grid(wave ~ .)
p = p +  labs(x="Number of care homes", 
              y = "House prices (log)")  

p

homes_prices = homes_prices + + geo



# subset a sample with the variables 



s1 = smple %>% select(lpa, wave, entry_rate, log.price,  prop.old.la,  prop.jsa.la, prop.income.sup.la,  hhi, 
                        refusal_maj_7908, delchange_maj1, delchange_maj5, delchange_maj6, labourvotes1983)


mod_ols = plm(entry_rate ~  prop.old.la , data=s1 , index = c("lpa", "wave"), model = "pooling")
summary(mod_ols)


# linear model
mod_ols4 = plm(entry_rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi, data=s1 , index = c("lpa", "wave"), model = "pooling")
 summary(mod_ols4)

# IV 
 mod_ols_iv4.1 = ivreg(entry_rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|refusal_maj_7908 + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
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



# add pension credits 


# OLS
mod_ols8 = plm(entry_rate ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi, data=s1 , index = c("lpa", "wave"), model = "pooling")
summary(mod_ols8)

# IV
mod_ols_iv8 = ivreg(entry_rate~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la|
                      refusal_maj_7908 + prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi,   data=s1) 

summary(mod_ols_iv8, diagnostics = TRUE)

      # likely instrument is also endogenous 

        # labour votes 
        mod_ols_iv4.1.1 = ivreg(entry_rate~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la|
                                refusal_maj_7908 + labourvotes1983 + prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi,   data=s1) 
        
        summary(mod_ols_iv4.1.1, diagnostics = TRUE)

        # delayed 
        # maj1 - 94/96 and 04/06
        mod_ols_iv4.1.2 = ivreg(entry_rate~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi|
                                  refusal_maj_7908 + delchange_maj1 + prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi,   data=s1) 
        
        summary(mod_ols_iv4.1.2, diagnostics = TRUE)
        
        #maj5 - 79/01 and 03/08
        mod_ols_iv4.1.3 = ivreg(entry_rate~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi|
                                  refusal_maj_7908 + delchange_maj5 + prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi,   data=s1) 
        
        summary(mod_ols_iv4.1.3, diagnostics = TRUE)
        
        #maj6 - 96/01 and 03/08
        mod_ols_iv4.1.4 = ivreg(entry_rate~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi|
                                  refusal_maj_7908 + delchange_maj6 +labourvotes1983+ prop.old.la + prop.jsa.la + prop.income.sup.la + prop.pension.la + hhi,   data=s1) 
        
        summary(mod_ols_iv4.1.4, diagnostics = TRUE)
        
lab_instr = data.frame(attr(`lpa`, "names"), attr(`lpa`, "var.labels"))
names(lab_instr) = c("variable", "label")
