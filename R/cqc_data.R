# importing CQC data and cleaning some variables 

# created: 27/09/2016
# modified: 27/09/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# This script contains the information associated with the registry of care homes.
# It contains the active and inactive care homes

# Registry that enables to get the entries and exits of care homes.
# Create variables that reflect the entries and the exits. 
# Care homes are identified by postal codes. 
#Some cases have duplicated postal codes - (re) registrations that may reflect different things

# Steps for the analysis:

#     - 1. Identify duplicated observations 
#     - 2. Classify entries depending on _ a) de novo entries b) spurious entries (re -registrations)
#               Note: spurious entries occur for different reasons -  mostly change of the provider
#     - 3. Create a variable that reflects the flow: type of enty and type of exit
#     - 4. Data are transformed into long format






# load data corresponding to CQC registry and do some cleaning
# -------------------------------------------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/R")



library(readxl)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(rio)
library(magrittr)




data = read_excel("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data /raw/20160927_Active and inactive care homes.xlsx", sheet = 2, col_names = TRUE, col_types = NULL)


# load function for renaming variables of teh datset
      rename.var <- function(dta){
        
        x = tolower(colnames(dta))
        x = gsub(" ", ".", x)
        x = gsub("\\s*\\([^\\)]+\\)","",as.character(x))
      
        return(x)
      }
  
# change the name of the care homes 
      
      # makes easier the management of variables 
      # it helps to compare other variables from alternative datasets
      
  cqc = rename.var(data)
  colnames(data) = cqc


 
  data = write.csv(data, "data.csv", row.names = F)


# select common variables in data that may be linkable with other datasets 

data.1 = data %>% select(Location.ID, location.start,location.end,  `Location Name`, `Location Status`, Care.homes.beds, 
                         location.type, City, Postal.Code, Local.Authority, County, Region, Provider.ID, Provider.Name, 
                         provider.start, provider.end, provider.postal.code, provider.status, provider.local.authority) 



library(tidyr)

# obtain duplicate postcodes 
# --------------------------

dup_data = data.1 %>%
  group_by(Postal.Code) %>%
  filter(n()>1) %>%
  mutate_each(funs(as.Date), location.start, location.end, provider.start, provider.end) %>%
  #select(Location.ID:Care.homes.beds, `Location Status`, Provider.Name, Provider.ID, provider.status, provider.start, provider.end) %>%
  arrange(Postal.Code, location.start, location.end)



# variable entry
# ---------------

# there are two types of entries: novo entries and spurious
#     spurious entries are assumed to be those that reflect an entry location and a change in the provider 
#     novo entries are those that reflect the first registration


entry = dup_data %>%
  group_by(Postal.Code, Provider.ID) %>%
  filter(row_number()==1) %>%
  mutate(flow.entry = "entry") %>%
  select(Postal.Code, Location.ID, Location.ID, Provider.ID,  location.start, location.end, provider.status, flow.entry)

# link datasets
test = left_join(dup_data, entry, by = c("Postal.Code", "Location.ID", "Provider.ID", "location.start", "location.end", "provider.status"))

# entry novo
entry.novo = dup_data %>%
  group_by(Postal.Code) %>%
  filter(row_number()==1) %>%
  mutate(flow.entry.nov = "novo") %>%
  select(Postal.Code, Location.ID, Location.ID, Provider.ID,  location.start, location.end, provider.status, flow.entry.nov)
test = left_join(test, entry.novo, by = c("Postal.Code", "Location.ID", "Provider.ID", "location.start", "location.end", "provider.status"))


test = test %>% group_by(Postal.Code) %>%
  mutate(ent = ifelse(!is.na(flow.entry.nov), flow.entry.nov, flow.entry))

# exit variable 
# --------------

# exits are those that reflect the total unregistration of the care home 

# definite  exit 
exit = dup_data %>%
  group_by(Postal.Code) %>%
  filter(row_number()==n() & !is.na(location.end)) %>%
  mutate(flow.exit.def= "exit.def") %>%
  select(Postal.Code, Location.ID, Location.ID, Provider.ID, location.start, location.end, provider.status, flow.exit.def)

# link datasets
test = left_join(test, exit, by = c("Postal.Code", "Location.ID", "Provider.ID", "location.start", "location.end", "provider.status"))

# prov.change - when it is an exit that it is not definite
exit.change = dup_data %>%
  group_by(Postal.Code, Provider.ID) %>%
  filter(row_number()==n() & !is.na(location.end)) %>%
  mutate(flow.exit= "prov.change") %>%
  select(Postal.Code, Location.ID, Location.ID, Provider.ID, location.start, location.end, provider.status, flow.exit)


test = left_join(test, exit.change, by = c("Postal.Code", "Location.ID", "Provider.ID", "location.start", "location.end", "provider.status"))

test = test %>% group_by(Postal.Code) %>%
  mutate(exit = ifelse(!is.na(flow.exit.def), flow.exit.def, flow.exit))

prueba = test

library(tidyr)


# present the dataset in long format

test_long = prueba %>% select(Postal.Code, Location.ID, `Location Name`, Provider.Name, Provider.ID,
                              Care.homes.beds, `Location Status`, provider.status, location.type, City, 
                              Local.Authority, County, Region, provider.local.authority, ent, exit, location.start, location.end, provider.start, provider.end)

test_long = test_long %>% gather(registry, date, location.start:provider.end)


test_long = test_long %>% arrange(Postal.Code, date, Location.ID)

# create a variable of the flow
# -----------------------------

test_long = test_long %>% group_by(Postal.Code) %>% 
  mutate(reg = ifelse(registry == "location.start" & ent == "novo", ent, 
                      ifelse(registry == "location.start" & ent == "entry", "change.prov", NA)))

test_long = test_long %>% group_by(Postal.Code) %>% 
  mutate(reg = ifelse(is.na(reg) & registry == "location.end" & exit == "exit.def", exit, reg)) 


write.csv(test_long, "cqc_duplicates.csv", row.names = FALSE)            


# unique postcodes
# ---------------


uni.post = 
  data.1 %>%
  group_by(Postal.Code) %>%
  filter(n() == 1) %>% 
  mutate_each(funs(as.Date), location.start, location.end, provider.start, provider.end) %>%
  #select(Location.ID:Care.homes.beds, `Location Status`, Provider.Name, Provider.ID, provider.status, provider.start, provider.end) %>%
  arrange(Postal.Code, location.start, location.end)


# re - arrange variables 
uni.post = uni.post %>% select(Postal.Code,Location.ID,`Location Name`, Provider.Name, Provider.ID,             
                               Care.homes.beds, `Location Status`, provider.status, location.type, City,                 
                               Local.Authority, County, Region, provider.local.authority, location.start, location.end, 
                               provider.start, provider.end)


uni.entry = uni.post %>% group_by(Postal.Code, Provider.ID) %>%
  gather(registry, date, location.start:provider.end) %>% arrange(Postal.Code, date, Location.ID)




# create variables corresponding to entries and exists 
#     -  there are no repeated entries; in this case entries are de novo entries 

uni.cqc = uni.entry %>% group_by(Postal.Code, Provider.ID) %>%
  mutate(reg = ifelse(registry == "location.start" & !is.na(date), "novo",
                      ifelse(registry == "location.end" & !is.na(date), "exit.def", NA)))


# link datasets corresponding to both duplicated and single post codes 

uni.names = colnames(uni.cqc)


dup.cqc = test_long %>% select(one_of(uni.names)) # removes the information corresponding to the entry and exit created  in the duplicates

# checks --
dup.names = colnames(dup.cqc)
setdiff(dup.names, uni.names)
# ----------------

# link datasets 
cqc.entries = rbind(uni.cqc, dup.cqc)

cqc.entries = cqc.entries %>% group_by(Postal.Code, date, Location.ID) %>%
  mutate_each(funs(as.factor), Postal.Code, reg)

summary(cqc.entries)

write.csv(cqc.entries, "cqc.entries.csv", row.names = FALSE)







