library(ggplot2)
library(ggthemes)

prices_earnings = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/price_paid_for_property_and_annual_earnings_indices.csv", sep = ";")


fig1 = ggplot(prices_earnings, aes(Period)) + 
  geom_line(aes(y = House.Prices, colour = "House Prices")) + 
  geom_line(aes(y = Earnings, colour = "Earnings")) + 
  labs(title = "Median price paid for property and annual earning indices - England Wales from 1997 to 2016",
       subtitle = "Index (1997 = 100)", 
       caption = "Source: Office of National Statistics", 
       x = "year", y = "") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=18, hjust=0))


  

fig1 = fig1 + theme_hc()

fig1






instr = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/data_LPA.dta")
names(instr)

sample_instr = instr %>% select(year, lpa_code:lpa_name_2005on, delchange_maj1, labourvotes1983, pop_density_1911_imp)


cqc_code =  import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.geolocated.csv")

cqc = cqc_code %>% filter(registry == "location.start")



diffcodes = setdiff(unique(cqc$old.la), unique(instr$lpa_code) )


diffcodes1 = setdiff(unique(instr$lpa_code), unique(cqc$old.la) )





check = cqc %>% filter(old.la %in% diffcodes)
check1 = instr %>% filter(lpa_code %in% diffcodes1)
