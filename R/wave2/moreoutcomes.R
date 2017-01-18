# Further outcomes 
# created 17/01/2017
# modified 18/01/2017
# @EduGonzalo


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

# functions ------------


# rename variables 
        garbage = function(x){
          names(x) = x[1, ]
          x = x[-1, ]
          return(x)
        }
        
# rename variables which name is repeate
        dup.names = function(x){
          
          test = sub("Postal Code", "location.code", names(x))
          names(x) = test
          return(x)
          
        }

# activities

      service = function(x){
        test = select_vars(names(x), starts_with("Service type -"))
        test
        test.1 = x %>% select(`Location ID`, `Care home?`, `Location Name`, `location.code`,
                              `City`, `County`, `Region`, one_of(test))
        return(test.1)
      }
      
      reg.activity = function(x){
        test = select_vars(names(x), starts_with("Regulated activity -"))
        test
        test.1 = x %>% select(`Location ID`, `Care home?`, `Location Name`, `location.code`,
                              `City`, `County`, `Region`, one_of(test))
        return(test.1)
      }
      
      user = function(x){
        test = select_vars(names(x), starts_with("Service user band -"))
        test
        test.1 = x %>% select(`Location ID`, `Care home?`, `Location Name`, `location.code`,
                              `City`, `County`, `Region`, one_of(test))
        return(test.1)
      }
  


# ------------------------------------------

april13 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/4 April 2013 HSCA Active Locations.xlsx")
april15 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/01 April 2015 HSCA Active Locations.xlsx")
oct16 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/4 October 2016 HSCA Active Locations.xlsx")


#  cleaning variables

    apr13 = april13[-c(1:7),]
    apr15 = april15[-c(1:5), ]
    oc16 = oct16[-c(1:2),]
    
  # create a list of data frames  
    list.df = list(apr13, apr15, oc16)
   
  # apply functions for cleaning garbage 
    list.df = lapply(list.df, garbage)
    list.df = lapply(list.df, dup.names)
    
  # extract data frames from the list 
    mapply(assign, list("apr13","apr15", "oc16"), list.df, MoreArgs=list(envir=.GlobalEnv))
    
  rm(april13, april15, oct16, listdf)

# make chunks of long datsets associated with service type,  regulated activity and service user band
# ---------------------------------------------------------------------------------------------------   
 
  
  
 names(apr13)[27] <- "provider.postcode" 

# list of data frames  
    list_df = list(apr13, apr15, oc16)
    
    # subset data frames
    list_serv = lapply(list_df, service)
    list_reg = lapply(list_df, reg.activity)
    list_user = lapply(list_df, user)
  
  # extract data frames
      mapply(assign, list("serv_apr13","serv_apr15", "serv_oct16"), list_serv, MoreArgs=list(envir=.GlobalEnv))
      
      mapply(assign, list("reg_apr13","reg_apr15", "reg_oct16"), list_reg, MoreArgs=list(envir=.GlobalEnv))
      
      mapply(assign, list("user_apr13","user_apr15", "user_oct16"), list_user, MoreArgs=list(envir=.GlobalEnv))




keep(serv_apr13, serv_apr15, serv_oct16, reg_apr13, 
     reg_apr15, reg_oct16, 
     user_apr13, user_apr15, user_oct16, list_df, dup.names, garbage, reg.activity, service, user, apr13, 
     apr15, oc16, sure = TRUE)

# ------------------------------
# set a data frame for each wave 
# ------------------------------

# services
# --------



  #wave 1

      ser_1 = serv_apr13 %>% 
        gather(service.type, yes, `Service type - Acute services with overnight beds`:`Service type - Urgent care services`) %>%
        arrange(`Location ID`)%>% mutate_each(funs(as.factor), service.type)
      
      ser_1= ser_1 %>% mutate(service.type = gsub("Service type - ", "", service.type), wave =1) %>% filter(!is.na(yes))

  #wave 2
      ser_2 = serv_apr15 %>% 
        gather(service.type, yes, `Service type - Acute services with overnight beds`:`Service type - Urgent care services`) %>%
        arrange(`Location ID`)%>% mutate_each(funs(as.factor), service.type)
      
      ser_2 = ser_2 %>% mutate(service.type = gsub("Service type - ", "", service.type), wave = 2) %>% filter(!is.na(yes))

  # wave 3 
      ser_3 = serv_oct16 %>% 
        gather(service.type, yes, `Service type - Acute services with overnight beds`:`Service type - Urgent care services`) %>%
        arrange(`Location ID`)%>% mutate_each(funs(as.factor), service.type)
      
      ser_3 = ser_3 %>% mutate(service.type = gsub("Service type - ", "", service.type), wave = 3) %>% filter(!is.na(yes))
      

types.services = rbind(ser_1, ser_2, ser_3) %>% 
  arrange(`Location ID`) %>% 
  filter(`Care home?` == "Y") %>%
  mutate_each(funs(as.factor), service.type)


# regulated activity
# ------------------


    #wave 1
    reg_1 = reg_apr13 %>% 
      gather(regulated.activity, yes, 
             `Regulated activity - Accommodation and nursing or personal care in the further education sector`:`Regulated activity - Treatment of disease, disorder or injury`) %>%
      arrange(`Location ID`)%>% mutate_each(funs(as.factor), regulated.activity)
    
    reg_1= reg_1 %>% mutate(regulated.activity = gsub("Regulated activity - ", "", regulated.activity), wave =1) %>% filter(!is.na(yes))
    
    #wave 2
    reg_2 = reg_apr15 %>% 
      gather(regulated.activity, yes, 
             `Regulated activity - Accommodation and nursing or personal care in the further education sector`:`Regulated activity - Treatment of disease, disorder or injury`) %>%
      arrange(`Location ID`)%>% mutate_each(funs(as.factor), regulated.activity)
    
    reg_2= reg_2 %>% mutate(regulated.activity = gsub("Regulated activity - ", "", regulated.activity), wave = 2) %>% filter(!is.na(yes))
    
    # wave 3 
    reg_3 = reg_oct16 %>% 
      gather(regulated.activity, yes, 
             `Regulated activity - Accommodation for persons who require nursing or personal care`:`Regulated activity - Treatment of disease, disorder or injury`) %>%
      arrange(`Location ID`)%>% mutate_each(funs(as.factor), regulated.activity)
    
    reg_3= reg_3 %>% mutate(regulated.activity = gsub("Regulated activity - ", "", regulated.activity), wave =1) %>% filter(!is.na(yes))
    

    regulated.activities = rbind(reg_1, reg_2, reg_3) %>% 
      arrange(`Location ID`) %>% 
      filter(`Care home?` == "Y") %>%
      mutate_each(funs(as.factor), regulated.activity)

# users 
    
    
    user_1 = user_apr13 %>% 
      gather(user, yes, 
             `Service user band - Children 0-18 years`:`Service user band - Younger Adults`) %>%
      arrange(`Location ID`)%>% mutate_each(funs(as.factor), user)
    
    user_1= user_1 %>% mutate(user = gsub("Regulated activity - ", "", user), wave =1) %>% filter(!is.na(yes))
    
    #wave 2
    user_2 = user_apr15 %>% 
      gather(user, yes, 
             `Service user band - Children 0-18 years`:`Service user band - Younger Adults`) %>%
      arrange(`Location ID`)%>% mutate_each(funs(as.factor), user)
    
    user_2= user_2 %>% mutate(user = gsub("Regulated activity - ", "", user), wave = 2) %>% filter(!is.na(yes))
    
    # wave 3 
    user_3 = user_oct16 %>% 
      gather(user, yes, 
             `Service user band - Children 0-18 years`:`Service user band - Younger Adults`) %>%
      arrange(`Location ID`)%>% mutate_each(funs(as.factor), user)
    
    user_3= user_3 %>% mutate(user = gsub("Regulated activity - ", "", user), wave =3) %>% filter(!is.na(yes))
    
    
    user.band = rbind(user_1, user_2, user_3) %>% 
      arrange(`Location ID`) %>% 
      filter(`Care home?` == "Y") %>%
      mutate_each(funs(as.factor), user)
    

# save 
    
  write.csv(types.services, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/types.services.csv", row.names = FALSE)
  write.csv(regulated.activities, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/reg.activities.csv", row.names = FALSE)
  write.csv(user.band, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/user.band.csv", row.names = FALSE)
  