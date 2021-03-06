# Geographic plots 

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
sectors.df.england = sectors.df %>% filter(DESCRIPTIO != "Council Area (Scotland)" | CODE != "W0") %>% filter(str_detect(CODE, 'E0')) %>% arrange(NAME) 

# link

head(sectors.df.england)

# sample with entry rates 
sm = s1 %>% select(wave, oslaua, lpa.1, mean_price, entry_rate_nat)

sm = sm %>% group_by(oslaua) %>% mutate(mean.entry = mean(entry_rate_nat)) %>% as.data.frame()
sm.unique = sm %>% distinct(oslaua, lpa.1, mean.entry)

data_mapping = left_join(sectors.df.england, sm.unique, by = c("CODE" = "oslaua" ))

data_mapping = unique(data_mapping)


library(Hmisc)
data_mapping$var =  with(data_mapping, cut2(mean.entry, g = 5))
                                   


# select names that do not have information regarding a wave 

names_na = data_mapping %>% filter(is.na(wave)) 

          names_na_1 = names_na %>% mutate(wave = 1)
          names_na_2 = names_na %>% mutate(wave =2)
          names_na_3 = names_na %>% mutate(wave = 3)


# wave 1 
      data_1 = data_mapping %>% filter(wave == 1)
      
      data_1 = rbind(data_1, names_na_1)

# wave 2 

      data_2 = data_mapping %>% filter(wave == 2)
      
      data_2 = rbind(data_2, names_na_2)
      
# wave 3 
      
      data_3 = data_mapping %>% filter(wave == 3)
      
      data_3 = rbind(data_2, names_na_3)

# map_mean
map_mean = ggplot(data_mapping, aes(long, lat, group=group)) +
  scale_fill_brewer(type = "seq", palette = "Blues", na.value = "grey", name = "Entry rate (%)", 
                    labels=c("0 - 4.1",
                             "4.1 - 5.8", 
                             "5.8 - 7.6",
                             "7.6 - 9.5",
                             "9.5 - 23.6")) +
  geom_polygon(aes(fill = var))+ 
  coord_equal() +
  geom_path(colour = "black", size =0.25) +
  labs(x = "Longitude", y = "Latitude")
  

ggsave("map_mean.png", map_mean, scale = 0.5)


# map 2
map_2 = ggplot(data_2, aes(long, lat, group=group)) +
  scale_fill_brewer(type = "seq", palette = "Blues", na.value = "grey", name = "Entry rate (%)",
                    labels=c("0", "3", "3 - 5", "5 - 9", "9 - 16", "16 - 68")) +
  geom_polygon(aes(fill = var))+ 
  coord_equal() +
  geom_path(colour = "black", size =0.25) +
  labs(x = "Longitude", y = "Latitude", title = "Average entry rate (wave 2)")


ggsave("map_2.png", map_2, scale = 0.5)

# map 3
map_3 = ggplot(data_3, aes(long, lat, group=group)) +
  scale_fill_brewer(type = "seq", palette = "Blues", na.value = "grey", name = "Entry rate (%)",
                    labels=c("0", "1.7 - 3", "3 - 5", "5 - 9", "9 - 16", "16 - 68")) +
  geom_polygon(aes(fill = var))+ 
  coord_equal() +
  geom_path(colour = "black", size =0.25) +
  labs(x = "Longitude", y = "Latitude", title = "Average entry rate (wave 3)")


ggsave("map_3.png", map_3, scale = 0.5)

plots = list(map_1, map_2, map_3)

grid.arrange(map_1, map_2, ncol = 2, main = "Main title")



                               



