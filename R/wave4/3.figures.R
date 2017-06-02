#######################################
#     Figures - Spatial distribution of care homes and house prices
#     Date: June 2017
#     @ Edu Gonzalo Almorox
########################################

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
library(Hmisc) 


###########################
# Geographical information
###########################

# sectors information 
  sectors<- readOGR("/Users/Personas/Downloads/Great Britain Local Authority Districts Council Areas and Unitary Authorities", "Local_UnitaryAuthority")

# data id
  sectors@data$id = rownames(sectors@data)

# transform to a data.frame 
  sectors.points = fortify(sectors, region = "id")

  sectors.df = inner_join(sectors.points, sectors@data, by = "id")  #data frame that we use for drawing the map 

# clean England data 
  sectors.df.england = sectors.df %>% filter(str_detect(CODE, 'E0')) %>% arrange(NAME) 
  
###########################
# Load information to be plotted
#############################
  
  entries = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/count_entries_exits_wave.csv")
  population = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/population_waves_oslaua.csv")
  house_prices = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/sum_prices_waves_oslaua.csv")
  
  
  test =  left_join(entries, population, by = c("oslaua", "wave"))
  test = left_join(test, house_prices, by = c("oslaua", "wave"))
  
  
  # codes that are  not similar - recode this
  
  codes = setdiff(sectors.df.england$CODE, test$oslaua)
  
  
test = test %>% mutate(CODE = ifelse(oslaua == "E06000057", "E06000048",
                                                       ifelse(oslaua == "E08000037", "E08000020",
                                                              ifelse(oslaua == "E07000243", "E07000101", 
                                                                     ifelse(oslaua == "E07000242", "E07000097", oslaua)))))
  
  
  # select the information to be plotted 

  test =  test %>% select(CODE, wave, care_homes, geoavprice, population)
  

  # create care homes per 1000 population over 65 
  
  test$population = as.numeric(test$population)
  test = test %>% mutate(carehomespop = (care_homes/population)*1000)
  
  test = test %>% group_by(CODE) %>% mutate(carehomes = mean(carehomespop),
                                                  price = mean(geoavprice))%>% as.data.frame()
  
  
  test = test %>% group_by(CODE) %>% select(CODE, carehomes, price) %>% unique()
  
  
  
##############################################
# Link information
#  - care homes per population
#  - average house prices 
#  - geographical codes
###############################################
  
  data_mapping = left_join(sectors.df.england, test, by = "CODE")
  
  # create quintiles for care homes and prices
  
  data_mapping = data_mapping %>% mutate(quintile_care= ntile(carehomes, 5), 
                                         quintile_prices = ntile(price, 5)) 
  
  
  
##################
# Plot
##################
  
  # care homes per population
  # -------------------------
  
  
  # quintiles
  Hmisc::cut2(data_mapping$carehomes, g=5) 
  
  
  map_care = ggplot(data_mapping, aes(long, lat, group=group, fill = quintile_care)) +
    geom_polygon() + 
    labs(fill = "Care homes per population") +
    scale_fill_viridis(labels = c("[0.245,0.815)", "[0.815,0.995)", "[0.995,1.155)", "[1.155,1.364)", "[1.364,2.505]")) +   
    theme(axis.text = element_blank()
        ,axis.title = element_blank()
        ,axis.ticks = element_blank()
        ,panel.grid = element_blank()
         ,legend.title = element_text(size =6)
          ,legend.text = element_text(size = 6)
           ,legend.background = element_blank()
             ,panel.background = element_blank())
  
  ggsave("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /wp /june2017/map_care.png", map_care, scale = 0.5)
  
  

  # house prices
  # ----------------
  
  # quintiles
  Hmisc::cut2(data_mapping$price, g=5) 
  
  
  
  map_prices = ggplot(data_mapping, aes(long, lat, group=group, fill = quintile_prices)) +
    geom_polygon() + 
    labs(fill = "House prices (£)") +
    scale_fill_viridis(labels = c("[ 70,124, 143,180)",
"[143,180, 185,758)",
"[185,758, 205,371)",
"[205,371, 252,467)", 
"[252,467, 1,166,779]")) +   
    theme(axis.text = element_blank()
          ,axis.title = element_blank()
          ,axis.ticks = element_blank()
          ,panel.grid = element_blank()
          ,legend.title = element_text(size =6)
          ,legend.text = element_text(size = 6)
          ,legend.background = element_blank()
          ,panel.background = element_blank())
  
  ggsave("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /wp /june2017/map_prices.png", map_prices, scale = 0.5)
  
  
  
  
  
  
  
  