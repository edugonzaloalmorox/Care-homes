# Entries and exits 

# created: 27/09/2016
# modified: 28/09/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------


# This script calculates the entries and exits used for the analysis.
# Entries can be differentiated as "de novo" or spurious. I calculate variables
# to differentiate between both types of entries.

# I do the same with entries. They may be originated by a change in the provider
# or a definite closure.

# Steps:

      # select variables for analysis
      # define entries
      # define exits 
      # change the structure of the dataset -long format: it enables to identify each date referred to 
      # each data frame


# -------------------------------
# Define the dataset for analysis 
# --------------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output/datasets")

# - select common variables in data that may be linkable with other datasets 


cqc_data = data %>% select(location.id, location.start = hsca.start.date, location.end = hsca.end.date,location.name, location.status,
                           care.homes.beds, location.type = `location.type/sector`, city, postal.code, local.authority, county, location.region,
                           provider.id, provider.name, 
                           provider.start = provider.hsca.start.date, provider.end = provider.hsca.end.date, 
                           provider.postal.code = `provider.-.postal.code`, provider.status, provider.local.authority) 



# --------------------------------------
# Prepare the data for entries and exits 
# ----------------------------------------

      # create entry and exit variables
      # duplicate postcodes may mean different types of entries 
      # split the dataset between those records that are duplicated (e.g. postcodes associated with
      # various locations ID) and those records which are referred to a single postcode  

library(tidyr)


# Obtain duplicated postcodes 
# -----------------------------

        dup_data = cqc_data %>%
          group_by(postal.code) %>%
          filter(n()>1) %>%
          mutate_each(funs(as.Date), location.start, location.end, provider.start, provider.end) %>%
          #select(Location.ID:Care.homes.beds, `Location Status`, Provider.Name, Provider.ID, provider.status, provider.start, provider.end) %>%
          arrange(postal.code, location.start, location.end)


        # ---------------
        # variable entry
        # ---------------

# there are two types of entries: novo entries and spurious
#     spurious entries are assumed to be those that reflect an entry location and a change in the provider 
#     novo entries are those that reflect the first registration



# select all entries (regardless of the type of entry)
        
        entry = dup_data %>%
          group_by(postal.code, provider.id) %>%
          filter(row_number()==1) %>%
          mutate(flow.entry = "entry") %>%
          select(postal.code, location.id, provider.id,  location.start, location.end, provider.status, flow.entry)

# link duplicates and entries 
        
        test = left_join(dup_data, entry, by = c("postal.code", "location.id", 
                                                 "provider.id", "location.start", 
                                                 "location.end", "provider.status"))

# create entry novo
# identify those observations that came first

        entry.novo = dup_data %>%
          group_by(postal.code) %>%
          filter(row_number()==1) %>%
          mutate(flow.entry.nov = "novo") %>%
          select(postal.code, location.id, provider.id,  location.start, location.end, provider.status, flow.entry.nov)

# link with 'root' dataset and create 'ent' for reflecting the type of entry - eg. novo and/or (spurious entry)

          test = left_join(test, entry.novo, by = c("postal.code", "location.id", 
                                                    "provider.id", "location.start", 
                                                    "location.end", "provider.status"))
          
          
          test = test %>% group_by(postal.code) %>%
            mutate(ent = ifelse(!is.na(flow.entry.nov), flow.entry.nov, flow.entry))
          
            # --------------
            # exit variable 
            # --------------

# there are various types of exits: unregistration due to a change in the provider and total unregistration
#           exits are those that reflect the total unregistration of the care home 

# create definite  exits
          
        exit = dup_data %>%
          group_by(postal.code) %>%
          filter(row_number()==n() & !is.na(location.end)) %>%
          mutate(flow.exit.def= "exit.def") %>%
          select(postal.code, location.id, location.id, provider.id, 
                 location.start, location.end, provider.status, flow.exit.def)

# link datasets
        
          test = left_join(test, exit, by = c("postal.code", "location.id", 
                                              "provider.id", "location.start", 
                                              "location.end", "provider.status"))

# prov.change - when it is an exit that it is not definite
        
          exit.change = dup_data %>%
              group_by(postal.code, provider.id) %>%
              filter(row_number()==n() & !is.na(location.end)) %>%
              mutate(flow.exit= "prov.change") %>%
              select(postal.code, location.id, location.id, provider.id, 
                     location.start, location.end, provider.status, flow.exit)


          test = left_join(test, exit.change, by = c("postal.code", "location.id", 
                                                     "provider.id", "location.start", 
                                                     "location.end", "provider.status"))
          
          test = test %>% group_by(postal.code) %>%
            mutate(exit = ifelse(!is.na(flow.exit.def), flow.exit.def, flow.exit))

# in 'test' `ent` and `exit` reflect what type of entry and/or exit corresponds  to each record.

prueba = test

library(tidyr)

# ------------------------------------
# Change the structure of the dataset
# ------------------------------------

# present the dataset in long format

          test_long = prueba %>% select(postal.code, location.id, location.name, provider.name, provider.id,
                                        care.homes.beds, location.status, provider.status, location.type, city, 
                                        local.authority, county, location.region, provider.local.authority, ent, exit, 
                                        location.start, location.end, provider.start, provider.end)
          
          test_long = test_long %>% gather(registry, date, location.start:provider.end)
          
          
          test_long = test_long %>% arrange(postal.code, date, location.id)

# create a variable of the flow
# -----------------------------

          test_long = test_long %>% group_by(postal.code) %>% 
            mutate(reg = ifelse(registry == "location.start" & ent == "novo", ent, 
                                ifelse(registry == "location.start" & ent == "entry", "change.prov", NA)))
          
          test_long = test_long %>% group_by(postal.code) %>% 
            mutate(reg = ifelse(is.na(reg) & registry == "location.end" & exit == "exit.def", exit, reg)) 
          
          
          
          write.csv(test_long, "cqc_duplicates.csv", row.names = FALSE)            

# - test long gets the duplicated  records of various ID´s referred to a single postcode

# ----------------
# Unique postcodes
# -----------------

        uni.post = 
            cqc_data %>%
            group_by(postal.code) %>%
            filter(n() == 1) %>% 
            mutate_each(funs(as.Date), location.start, location.end, provider.start, provider.end) %>%
            #select(Location.ID:Care.homes.beds, `Location Status`, Provider.Name, Provider.ID, provider.status, provider.start, provider.end) %>%
            arrange(postal.code, location.start, location.end)


# re - arrange variables 
          uni.post = uni.post %>% select(postal.code, location.id, location.name, provider.name, provider.id,             
                                   care.homes.beds, location.status, provider.status, location.type, city,                 
                                   local.authority, county, location.region, provider.local.authority, location.start, location.end, 
                                   provider.start, provider.end)


          uni.entry = uni.post %>% group_by(postal.code, provider.id) %>%
            gather(registry, date, location.start:provider.end) %>% arrange(postal.code, date, location.id)




# create variables corresponding to entries and exists 
#     -  there are no repeated entries; in this case entries are de novo entries 

          uni.cqc = uni.entry %>% group_by(postal.code, provider.id) %>%
            mutate(reg = ifelse(registry == "location.start" & !is.na(date), "novo",
                                ifelse(registry == "location.end" & !is.na(date), "exit.def", NA)))
          
          write.csv(uni.cqc, "cqc_single_records.csv",  row.names = F )


# link datasets corresponding to both duplicated and single post codes 

          uni.names = colnames(uni.cqc)
          
          
          dup.cqc = test_long %>% select(one_of(uni.names)) # removes the information corresponding to the entry and exit created  in the duplicates

# checks --
dup.names = colnames(dup.cqc)
setdiff(dup.names, uni.names)
rm(dup.names)
# ---------------


# link datasets with unique and duplicated records 

          cqc.entries = rbind(uni.cqc, dup.cqc)
          
          cqc.entries = cqc.entries %>% group_by(postal.code, date, location.id) %>%
            mutate_each(funs(as.factor), postal.code, reg)
          
          summary(cqc.entries)
          
          write.csv(cqc.entries, "cqc.entries.csv", row.names = FALSE)
          
          # cqc.entries.csv reflects the entries and exits in the CQC registry
          # it identifies which entries are de novo and which are definite exits
          






