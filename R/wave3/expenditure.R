# Data on social care spending 
# Obtained from the HSCIC

library(rio)
library(dplyr)
library(tidyr)


# load the data 
      sce1112 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/Final_PSS_Expenditure_2011_12_Raw_Data.csv")
      sce1213 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/Final_PSS_Expenditure_2012_13_Raw_Data.csv", sep = ",", header = TRUE)
      sce1314 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/Final_PSS_Expenditure_2013_14_Raw_Data.csv", sep = ",", header = TRUE)
      sce1415 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/sce1415.csv",sep = ",", header = TRUE )
      sce1516 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/pss-exp-eng-15-16-fin-exp.csv", sep = ",", header = TRUE)

# note:  2014/15 and 2015/16 have different structures 

# filter observations that correspond to residential care placements and the gross current expenditure of the total expenditure on adult social care

      expenditure = function(df){
        
      x = df %>% filter(MainHeading == "8. GROSS CURRENT EXPENDITURE")
      return(x)
      }
      
      
      
      
      residential = function(df){
        
        
        
        x = df %>% filter(Subset == "Residential care placements")
        x = x %>% filter(MainHeading == "8. GROSS CURRENT EXPENDITURE" & Category == "B. OLDER PEOPLE (AGED 65 OR OVER) INCLUDING OLDER MENTALLY ILL")
        return(x)
        
      }

# select variables of interest
# note: values are expressed in thousand pounds

        res1112 = residential(sce1112) %>% select(Name = council, Year, Value)
        res1213 = residential(sce1213) %>% select(Name, Year, Value)
        res1314 = residential(sce1314) %>% select(Name, Year, Value)

# analyse data from 2014/15 and 2015/16
        
        # 2014/15
        # --------
        head(sce1415)
        
        # rename variables with the elements of the first row 
        #colnames(sce1415) = sce1415[1,]
        #sce1415 = sce1415[-1, ]
        
        #write.csv(sce1415, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/sce1415.csv",  row.names = FALSE)
        
       # make it long format 
        
       sce1415_long = sce1415 %>%
         gather(county, value, `X102...Cumbria`:`X914...Torbay`)
       
       res1415 = sce1415_long %>% filter(`Service.delivery.mechanism` == "Residential" &
                                           `Age.category` == "Age 65 and over"& 
                                           `Accounting.category` == "Gross current expenditure" &
                                           `Primary.support.reason` == "Total") %>%  # select total - there are more categories though
         mutate(Year = 201415) %>% 
         select(county, Year, value)
        
       
       # 2015/16
       # -------
       
       
       colnames(sce1516) = sce1516[1,]
       write.csv(sce1516, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/sce1516.csv",  row.names = FALSE)
       
       sce1516 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/sce1516.csv")
       sce1516 = sce1516[-1, ]
       
    
       # make it long format 
       
       sce1516_long = sce1516 %>%
         gather(county, value, `102 - Cumbria`:`914 - Torbay`)
       
       
       res1516 = sce1516_long %>% filter(`Service delivery mechanism` == "Residential" &
                                           `Age category` == "Age 65 and over"& 
                                           `Accounting category` == "Gross current expenditure" &
                                           `Primary support reason` == "Total")  %>%  # select total - there are more categories though
         mutate(Year = 201516) %>% 
         select(county, Year, value)
       
       
      # homogeneous names of the counties
       
       res1415$place = res1415$county
       
       substr(res1415$place, 5, 7) <- " - "
       res1415$place = gsub("X", "", res1415$place)
       
      res1415 = res1415 %>% select(place, Year, value)
      res1516 = res1516 %>% select(place = county, Year, value)
      
      res1416 = rbind(res1415, res1516) %>% arrange(place) # collects information ob both 2014/15 and 2015/16
      
      # names
      res1416$counties = stringr::str_sub(res1416$place, start = 7, end = -1)
      res1416$counties = gsub('[[:punct:]]',' ',res1416$counties)
      
      # county codes
      res1416$codes = stringr::str_sub(res1416$place, start = 1, end = 3)
      
      
      res1416$Name = with(res1416, paste(counties, codes, sep = " ("))
      res1416$Name = paste0(res1416$Name, ")") 
      
      res1416 = res1416 %>% select(Name, Year, Value = value)
      
      
      res1116 = rbind(res1112, res1213, res1314, res1416) %>% arrange(Name, Year)
      
      # save data 
      
      write.csv(res1116, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/expenditure1116.csv", row.names = FALSE)
    
# ---------------------      
# cleaning expenditure
# ----------------------
      
      
      exp = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/expenditure1116.csv")
      
      # clean the numbers in exp - there are repeated variables
      
      exp = exp %>% mutate(authority = Name)  %>% 
        mutate( authority= gsub("&", "and", authority))  %>%
        mutate(authority = gsub("-", " ", authority))
      
      exp = exp %>% mutate(local = str_sub(local, start = -5, end = -1)) %>% group_by(local) %>% arrange(local, Year) %>% data.frame()
      
      exp = exp %>% mutate(autoridad = word(authority, 1, -2))
      
      
      
      test_4_1 = test_4_1 %>% mutate(local = local.authority)
      
      test_4_1$local = with(test_4_1, gsub(", City of", " ", local))
      test_4_1$local  = with(test_4_1, gsub(", County of", " ", local))
      test_4_1$local  = with(test_4_1, str_trim(local, side = "both"))
      test_4_1$local  = with(test_4_1, gsub("-", " ", local))
      test_4_1$local  = with(test_4_1, gsub("[[:punct:]]", " ", local))
      
      
      # recode those that are problematic in exp 
      
      
      
      exp$autoridad = recode(exp$autoridad, Durham = "County Durham", 
                             `East Riding`  = "East Riding of Yorkshire", 
                             `N E Lincolnshire` = "North East Lincolnshire", 
                             `N Lincolnshire` = "North Lincolnshire",
                             `St Helens` = "St Helens", 
                             `Blackburn` = "Blackburn with Darwen",
                             `Cheshire West And Chester` = "Cheshire West and Chester", 
                             `Telford and the Wrekin` = "Telford and Wrekin", 
                             Southend = "Southend on Sea",
                             `Bedford Borough` ="Bedford", 
                             `St Helens` = "" , 
                             `Medway Towns` = "Medway", 
                             `Bath and N E Somerset` =  "Bath and North East Somerset", 
                             `Isle of Wight Council` = "Isle of Wight",
                             `NA` = "Isles of Scilly")
      
      
      # check the sets        
      c = setdiff(unique(exp$autoridad), unique(test_4_1$local))
      c
      
      c = intersect(unique(test_4_1$local), unique(exp$autoridad))
      c
      
      test_4_1$local = gsub("[[:punct:]]", " ", test_4_1$local)
      test_4_1$local = recode(test_4_1$local, `St  Helens` = "St Helens")
      
      # set the time periods 
      
      # - 2011/13 t1
      # - 2013/15 t2
      # - 2015/16 t3
      
      t1 = c(201112, 201213)
      t2 = c(201314, 201415)
      t3 = c(201516)
      
      exp$Value = as.numeric(exp$Value)
      exp = exp %>% mutate(wave = ifelse(Year %in% t1, "1", ifelse(
        Year %in% t2, "2", 
        ifelse(Year %in% t3, "3", "other")))) %>% select(-period)
      
      # create an average of the expenditure for those years
      
      expenditure= exp %>% group_by(autoridad, wave) %>% mutate(av.exp = mean(Value)) %>% select(autoridad, av.exp, wave)
      expenditure = unique(expenditure)
      
      exp = as.data.frame(exp)
      
      expenditure$wave = as.integer(expenditure$wave)
      
      # link data of the expenditure 
      
      
      test5 = left_join(test_4_1, expenditure, by = c("local" = "autoridad", "wave" = "wave"))
      
      check = test5 %>% filter(is.na(av.exp))
      
      
      
      
     
        
       
        
        


