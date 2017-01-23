# Quality ratings 
# Created 19/01/2017
# Modified 23/01/2017


# ---------------------------------------------------------------------------------------------
# Cleaning the data concerning the ratings of quality inspections and counting the proporitons #
# ---------------------------------------------------------------------------------------------



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
          
          
          write.csv(ratings, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings.csv", row.names = FALSE)
        
# ------------------------------------------------------------------------------------------------------------        
# Expand the panel - those who are inspected in a certain period remain with this rating in subsequent periods 
# ------------------------------------------------------------------------------------------------------------   
          
          ratings = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings.csv") 
          
          # get the last for each wave (if there are more than one in one wave I take the last obsthe observation in the wave )
          
          last = ratings  %>% 
            group_by(Location.ID, wave) %>% filter(row_number() == n()) %>% as.data.frame()
          
          last$wave = as.numeric(last$wave)
          
          
          # syntehtic dataframe 
          
          # create a data frames with the IDs and all the waves. Link the real waves with all the possible waves.
          # Then supress the waves before 
          
          last$Location.ID = as.factor(last$Location.ID)
          
          levels.care.homes = unique(last$Location.ID)
          
          # new data frame
          new_data = data.frame(Location.ID = rep(levels.care.homes, 3), wave = rep(1:3, length(levels.care.homes))) %>% arrange(Location.ID, wave)
          new_data$Location.ID = as.character(new_data$Location.ID)
          
          
          
          last = last  %>% mutate(wave2 = 3, 
                                  initial = "Y") 
          
          last$wave = as.numeric(last$wave)
          test.1 = left_join(new_data, last, by = c("Location.ID", "wave"))
          
          
          # drop those observations that where not existent during wave 1 
          check.1 = test.1 %>% group_by(Location.ID) %>% filter(!(is.na(initial) & wave == 1))
          
          # drop those observations in wave 2 that are 
          prueba = check.1  %>% group_by(Location.ID) %>% filter(!(wave == 2 & is.na(initial) & n() == 2))
          
          # fill NAs; in cases where there is more than one level, it takes the value of the wave
          # note: I keep the integration without filling since it will show the observations that were given initially
          
          df = prueba %>% group_by(Location.ID) %>% fill(Location.Name, Care.Home., Location.Post.Code,
                                                         Location.Local.Authority, Location.Region, Overall.Rating, date, category) %>% select(-wave2)
          
 write.csv(df, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings_extended.csv", row.names = FALSE)
 
 
# ------------------          
# Geolocate ratings 
# ------------------        
          
 
          cqc_geo = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/geo.ch.postcodes.csv")   
          
          ratings = ratings %>% mutate(post2 = gsub("[[:space:]]", "", Location.Post.Code)) %>% select(-V1)
          
          test = left_join(ratings, cqc_geo, by = c("post2" = "post2"))
          
      write.csv(test, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/geo_ratings.csv", row.names = FALSE)
          
          
      # geolocate ratings expanded
      ratings_exp = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/ratings_extended.csv")
      
      ratings_exp = ratings_exp %>% mutate(post2 = gsub("[[:space:]]", "", Location.Post.Code))
      
      geo_ratings = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/geo_ratings.csv")
      
      ons_geo  = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/ons.postcode.csv")  
      
       
       geo = geo_ratings %>% select(Location.ID, X, Y, oslaua, post2)
          
          ext_geo = left_join(ratings_exp, geo, by  = c("Location.ID", "post2"))
   
          ext_geo = unique(ext_geo)
          
          check = ext_geo %>% filter(is.na(oslaua)) 
          # note: there are three care homes that cannot be geolocated: TF75FN, SY129DY, HD21NH
          
          write.csv(ext_geo, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/geo_ratings_extended.csv", row.names = FALSE)  
  
# ------------------------------       
# Count the ratings by category 
# ------------------------------
          
          count_ratings = ext_geo %>% 
            group_by(oslaua, Location.Region, wave, category) %>% 
            tally() %>%
            mutate(inspections_oslaua_wave = sum(n)) %>% 
            ungroup() %>%
            group_by(oslaua) %>% mutate(inspections_oslaua_total = sum(n)) %>% select(oslaua:category, inspections_oslaua = n, inspections_oslaua_wave, inspections_oslaua_total)
         
          write.csv(count_ratings, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/three/count_ratings.csv", row.names = FALSE)
 
           
          