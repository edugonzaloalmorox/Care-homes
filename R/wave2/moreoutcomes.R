# Further outcomes 
# created 17/01/2017
# modified 18/01/2017


library(ggplot2)
library(grid)
library(gridExtra)
library(rio)
library(AER)
library(plm)
library(dplyr)
library(tidyr)
library(stargazer)


april13 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/4 April 2013 HSCA Active Locations.xlsx")
april15 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/01 April 2015 HSCA Active Locations.xlsx")
oct16 = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/raw/4 October 2016 HSCA Active Locations.xlsx")


# rename duplicated variables
garbage = function(x){
  names(x) = x[1, ]
  x = x[-1, ]
  return(x)
}

dup.names = function(x){
  
  test = sub("Postal Code", "location.code", names(x))
  names(x) = test
  return(x)
  
}
# do a bit of cleaning

    apr13 = april13[-c(1:7),]
    apr15 = april15[-c(1:5), ]
    oc16 = oct16[-c(1:2),]
    
  # create a list of data frames  
    list.df = list(apr13, apr15, oc16)
   
    
    list.df = lapply(list.df, garbage)
    list.df = lapply(list.df, dup.names)
    
      
    mapply(assign, list("apr13","apr15", "oc16"), list.df, MoreArgs=list(envir=.GlobalEnv))
    
  rm(april13, april15, oct16, listdf)

# make chunks of long datsets associated with service type,  regulated activity and service user band
# ---------------------------------------------------------------------------------------------------   
  # rename duplicated names of the variables
  
 


  apr13 = dup.names(apr13)
  apr15 = dup.names(apr15)
  oc16 =  dup.names(oc16)
  
 
  # service type
  
  test = select_vars(names(apr13), starts_with("Service type -"))
  test
  
  names(apr13)[16] <- "location.postcode"
  names(apr13)[27] <- "provider.postcode"
  
  test.1 = apr13 %>% select(`Location ID`: `location.postcode`, one_of(test))
  
 
  names(apr15) = sub("Postal Code", "code", names(apr15))
  
  names(apr15)
 
 