# Quality ratings 
# Created 19/01/2017
# Modified 20/01/2017

# Cleaning the data concerning the ratings of quality inspections and counting the proporitons 



library(ggplot2)
library(grid)
library(gridExtra)
library(rio)
library(AER)
library(plm)
library(dplyr)
library(tidyr)
library(stargazer)
library(gdata)
library(lubridate)


services = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/types.services.csv")
reg.activities = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/reg.activities.csv")
users = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/user.band.csv")

# import quality 
 
quality_oct16 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/quality_ratings_oct16.csv", sep =";", header = TRUE)
quality_sep15 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/quality_ratings_sep15.csv", sep =";", header = TRUE)
quality_apr16 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/quality_ratings_april16.csv", sep =";", header = TRUE)

 # september 2015

      sep15 = quality_sep15 %>% filter(Care.Home. == "Y")
      
      sep15$date = as.Date(sep15$Publication.Date, format='%d/%m/%Y')
      sep15 = sep15 %>% select(Location.ID, Location.Name, Care.Home., Location.Post.Code, Location.Local.Authority,
                                   Location.Region, Overall.Rating, date)


 # april 2016

      april16 = quality_apr16 %>% filter(Care.Home. == "Y")
      
      april16$date = as.Date(april16$Publication.Date, format='%d/%m/%Y')
      april16 = april16 %>% select(Location.ID, Location.Name, Care.Home., Location.Post.Code, Location.Local.Authority,
                                         Location.Region, Key.Question, Latest.Rating,  date)


 # october 2016
      
      october16 = quality_oct16 %>% filter(Care.Home. == "Y")
      
      october16$date = as.Date(october16$Publication.Date, format='%d/%m/%Y')
      october16 = october16 %>% select(Location.ID, Location.Name, Care.Home., Location.Post.Code, Location.Local.Authority,
                                   Location.Region, Key.Question, Latest.Rating,  date)
  
  # link data from 2016 and create a variable with the overall rating  

        ratings16 = rbind(april16, october16) %>% arrange(`Location.ID`, date) 
        
        rat16 = ratings16 %>% filter(Key.Question == "Overall") %>% 
                              rename(Overall.Rating = Latest.Rating) %>% select(- Key.Question)
        
  # link data 15 y 16 and drop repeated vars 
          ratings = rbind(sep15, rat16)
        
          ratings = ratings %>%  arrange( date, `Location.ID`) %>% unique()
          
          
          # collate inadequate and requires improvement into a new category
          
          cats = c("Inadequate", "Requires improvement")
          ratings = ratings %>% mutate(category = NA)
          ratings = ratings %>% mutate(category = ifelse(Overall.Rating %in% cats, "Bad", Overall.Rating))
          
          ratings$category[ratings$category == 1] <- "Good"
          ratings$category[ratings$category == 3] <- "Outstanding"
          
          
  # create waves 
          
          # 1 - Oct 2014 - May 2015
          # 2 - June 2015 - February 2016
          # 3 - February 2016 - September 2016
          
          
          ratings = ratings %>% mutate(wave = ifelse(date <="2015-05-31", "1",
                                                     ifelse(date >= "2015-06-01" & date <= "2016-02-29" , "2",
                                                          ifelse(date >= "2016-03-01", "3", "4"))))
        
        # expand the panel - those who are inspected in a certain period remain with this rating 
          
          
          
          write.csv(ratings, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings.csv")
          
          # geolocate ratings 
      
          cqc_geo = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/geo.ch.postcodes.csv")   
          
          ratings = ratings %>% mutate(post2 = gsub("[[:space:]]", "", Location.Post.Code)) %>% select(-V1)
          
          test = left_join(ratings, cqc_geo, by = c("post2" = "post2"))
          
      write.csv(test, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/geo_ratings.csv")
          
          
          
          
         
          
    
