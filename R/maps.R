# Geographical representation of house prices and care homes entries 
# We are considering the sample of analysis 

library(rgdal)
library(foreign)
library(ggplot2)
library(choroplethr)
library(XML)
library(maptools)
library(dplyr)
library(tidyverse)
library(stringr)
library(rio)
library(grid)
library(gridExtra)
library(viridis)


# manage house prices 
# get the averages at district level


prices = import("/Users/Personas/Documents/research/other/visualizations/house prices/house_prices_england/data /raw/house_prices_geolocated.csv")

prices = prices %>% mutate_each(funs(as.Date), date.1) 

district_prices = prices %>% filter(date.1 >= "2011-03-01")



gpclibPermit()

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/geography_districts_england_dic15")

# 1. Get the geographical information of England - county level
#    - transform shape file into a data frame 
################################################################

sectors<- readOGR("/Users/Personas/Downloads/Great Britain Local Authority Districts Council Areas and Unitary Authorities", "Local_UnitaryAuthority")
s1 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/test_prices_entries_instruments.csv")
s1 = s1 %>% mutate(entry_rate_nat = entry_rate/100)



summary(sectors)


sectors@data$id = rownames(sectors@data)


sectors.points = fortify(sectors, region = "id")

sectors.df = inner_join(sectors.points, sectors@data, by = "id")  #data frame that we use for drawing the map 

# clean England data 
sectors.df.england = sectors.df %>% filter(str_detect(CODE, 'E0')) %>% arrange(NAME) 


# 2 Calculate an average price by district
# #####################################

library(lubridate)

prices_england = prices %>% filter(grepl("^E0", oslaua))
prices_england = prices_england %>% filter(date.1 >= "2011-03-01")

prices_clean = prices_england %>% group_by(oslaua) %>% mutate(mean_price = mean(price)) %>% select(mean_price, oslaua)
prices_clean = unique(prices_clean)
summary(prices_clean)


# 3 Link geographical information and house prices information 
# #############################################################

# check the codes - there may be changes from 2015 in the denomination
setdiff(unique(sectors.df.england$CODE), unique(prices_clean$oslaua))
no_geo = setdiff(unique(prices_clean$CODE), unique(sectors.df.england$CODE))

check = prices_clean %>% filter(oslaua %in% no_geo) # gives the district codes that are outdated

# renew the code in prices_clean 

prices_clean = prices_clean %>% mutate(CODE = ifelse(oslaua == "E06000057", "E06000048",
                                                     ifelse(oslaua == "E08000037", "E08000020",
                                                            ifelse(oslaua == "E07000243", "E07000101", 
                                                                   ifelse(oslaua == "E07000242", "E07000097", oslaua)))))
check = prices_clean %>% filter(oslaua %in% no_geo)

ggplot(prices_clean, aes(mean_price)) + geom_histogram()



data_mapping = left_join(sectors.df.england, prices_clean, by = "CODE")

write.csv(data_mapping, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/geography_districts_england_dic15/mapping_prices.csv", row.names = FALSE)


data_mapping = rio::import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/geography_districts_england_dic15/mapping_prices.csv")



data_mapping = data_mapping %>% mutate(quintile= ntile(mean_price, 5)) %>%
             mutate(log.price = log(mean_price)) 


map_price = ggplot(data_mapping, aes(long, lat, group=group, fill = quintile)) +
  geom_polygon() + 
  labs(fill = "House prices (£)") +
  scale_fill_viridis(labels = c("(95,487 - 176,472)", "(176,472 - 218,500)", "(218,500 - 242,251)", "(242,251 - 308,048)", 
                                "(308,048 - 2,170,757)")) +   theme(axis.text = element_blank()
                                 ,axis.title = element_blank()
                                 ,axis.ticks = element_blank()
                                 ,panel.grid = element_blank()
                                 ,legend.title = element_text(size =6)
                                 ,legend.text = element_text(size = 6)
                                 ,legend.background = element_blank()
                                 ,panel.background = element_blank()) 



ggsave("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /wp /april/map_price.png", map_price, scale = 0.5)

# 4 Information regarding care homes is developed in maps2.R
#############################################################


data_mapping = data_mapping %>% mutate(quintilech= ntile(ch.pop, 5)) 

data_mapping1 = data_mapping %>% filter(!is.na(quintilech))


map_ch = ggplot(data_mapping1, aes(long, lat, group=group, fill = quintilech)) +
  geom_polygon() + 
  labs(fill = "Proportion of \n care homes")  +
  scale_fill_viridis(labels = c("(0 - 1.35)", "(1.35 - 1.62)", "(1.62 - 1.86)", "(1.86 - 1.99)", "(1.99 - 4.06)")) +   theme(axis.text = element_blank()
                                ,axis.title = element_blank()
                                  ,axis.ticks = element_blank()
                                   ,panel.grid = element_blank()
                                   ,legend.title = element_text(size = 6)
                                    ,legend.text = element_text(size = 6)
                                     ,legend.background = element_blank()
                                      ,panel.background = element_blank())
                               

ggsave("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /wp /april/map_carehomes.png", map_ch, scale = 0.5)

g = grid.arrange(map_price, map_ch)

ggsave("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /wp /april/maps.png", g, scale = 1)


