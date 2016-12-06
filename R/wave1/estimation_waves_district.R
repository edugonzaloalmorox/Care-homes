# Estimation with entry rates, instruments and market characteristics 
# Exrpressed at district level

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves")

# 1 select variables of interest


smpl = import("sample_entry.csv")    

smpl = smpl %>% mutate_each(funs(as.numeric), pension_credits, carers_allowance, income_support, jsa_fem, total_pop) %>%
  select(wave, la_code, lpa, novo_entries:labourvotes1983)


smpl1 = smpl %>% group_by(lpa, wave) %>%
  dplyr::mutate(prop.old.la = (total_old/total_pop), 
                prop.jsa.la = (jsa_fem/total_pop),
                prop.pension.credit.la = (pension_credits/total_pop), 
                prop.income.sup.la = (income_support/total_pop), 
                prop.allowance.la = (carers_allowance/total_pop), 
                log.price = log(mean_price),
                rate_nat = rate/100) %>% 
  arrange(lpa, wave) %>% as.data.frame()


# descriptive stats 

library(ggplot2)

en <- ggplot(smpl1, aes(x=rate_nat)) 
en = en + geom_density() + ggtitle("Entry rates distribution 2011-2016") 

p1 <- ggplot(smpl1, aes(y = log.price,  x = rate_nat)) 
p1 = p1 + geom_point(color="red") + ggtitle("Prices and entry rates 2011-2016")

install.packages("gridExtra")
library(gridExtra)

plots = grid.arrange(en, p1, ncol=2)
plots


library(AER)
library(plm)
#############
# REGRESSION        
#############


# no controls
# ...........


# OLS

s1 = smpl1 %>% select(-total)
s1 = s1[-164, ] # get rid of derby 

s1 = smpl1


write.csv(s1, "sample_entry.csv", row.names = FALSE)

olsmod1 = plm(rate_nat ~ log.price, data=s1,  index = c("la_code", "wave"), model = "pooling")
sum_oldmod1 = summary(olsmod1)


# IV #



mod1iv= ivreg(rate ~ log.price |refusal_maj_7908,   data=s1) 
 summary(mod1iv)

 mod1iv= ivreg(rate ~ log.price |refusal_maj_7908 + labourvotes1983,   data=s1) 
 summary(mod1iv)


 mod_ols2 = plm(rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.allowance.la, data=s1 , index = c("lpa", "wave"), model = "pooling")
 mod_ols2_print = summary(mod_ols2)
 mod_ols2_print
 
 # IV 
 
 mod_ols_iv2 = ivreg(rate_nat~ log.price |refusal_maj_7908 + labourvotes1983 + male_earn_real,   data=s1) 
 mod_ols_iv2_print = summary(mod_ols_iv2)
 mod_ols_iv2_print
 
 
 
summary(mod_ols_iv2, diagnostics = TRUE)
sum_mod1


# -------------------

mod_ols3 = plm(rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la, data=s1 , index = c("lpa", "wave"), model = "pooling")
mod_ols3_print = summary(mod_ols3)
mod_ols3_print

mod_ols_iv3 = ivreg(rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la|refusal_maj_7908 + labourvotes1983 + male_earn_real + prop.old.la + prop.jsa.la + prop.income.sup.la,   data=s1) 
mod_ols_iv3_print = summary(mod_ols_iv3, vcov = sandwich, diagnostics = TRUE)
mod_ols_iv3_print

# ----------------------

mod_ols4 = plm(rate_nat ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi, data=test.3 , index = c("lpa", "wave"), model = "pooling")
mod_ols4_print = summary(mod_ols4)
mod_ols4_print

mod_ols_iv4 = ivreg(rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|refusal_maj_7908 + labourvotes1983 + male_earn_real + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=test.3) 
mod_ols_iv4_print = summary(mod_ols_iv4, vcov = sandwich, diagnostics = TRUE)
mod_ols_iv4_print


# first stage 

first_stage = lm(log.price ~ refusal_maj_7908 + labourvotes1983 + male_earn_real + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
instrFtest <- waldtest(first_stage,.~.-refusal_maj_7908 + labourvotes1983 + male_earn_real)
print(instrFtest)






# -----------------------


mod_ols4.1 = plm(log_rate ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi, data=s1 , index = c("lpa", "wave"), model = "pooling")
mod_ols4_print = summary(mod_ols4)
mod_ols4_print

mod_ols_iv4.1 = ivreg(log_rate~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi|refusal_maj_7908 + labourvotes1983 + male_earn_real + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi,   data=s1) 
mod_ols_iv4.1_print = summary(mod_ols_iv4.1)
mod_ols_iv4.1_print






mod_ols5 = plm(log_rate ~ log.price + prop.old.la + prop.jsa.la + prop.income.sup.la + hhi + prop.allowance.la, data=s1 , index = c("lpa", "wave"), model = "pooling")
mod_ols5_print = summary(mod_ols5)
mod_ols5_print

mod_ols_iv5 = ivreg(rate_nat~ log.price +  prop.old.la + prop.jsa.la + prop.income.sup.la + hhi + prop.allowance.la|refusal_maj_7908 + labourvotes1983 + male_earn_real + prop.old.la + prop.jsa.la + prop.income.sup.la +hhi + prop.allowance.la,   data=s1) 
mod_ols_iv5_print = summary(mod_ols_iv5)
mod_ols_iv5_print



summary(mod_ols_iv5, diagnostics = TRUE)
sum_mod1




