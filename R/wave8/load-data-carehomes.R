##############################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Information from the care home 
#         - Different dates (even before than 2010)
#         - Add information regarding de-registrations
##############################################

library(tidyverse)
library(rio)
library(lubridate)
library(purrr)
library(dummies)
library(janitor)
library(haven)
library(readxl)

# -------------------------

# -------------------------
# ACTIVE CARE HOMES 
# -------------------------

    
    active_jan18  = read_excel("data/raw/1 January 2018 HSCA active locations.xlsx", sheet = "HSCA Active Locations", range = "A7:AY49269")
    
    active_jan18 = clean_names(active_jan18) %>% 
      rename(registered_manager = registered_manager_note_where_there_is_more_than_one_manager_at_a_location_only_one_is_included_here_for_ease_of_presentation_the_full_list_is_available_if_required) %>%
      filter(care_home == "Y") %>%
      mutate(postcode = gsub("[[:blank:]]", "", location_postal_code), 
    telephone = gsub("[[:blank:]]", "", location_telephone_number))

# Old register
    csa  = read_excel("data/raw/CSA all.xlsx", sheet = "CSA data", range = "A9:V43495")
      
    csa = clean_names(csa) %>% select(region, council, provider_name = name_of_owning_organisation_where_designated_large_corporate_provider, 
                                        registration_date:type_of_service, location_name = name, location_street  = street,
                                        location_district = district, location_town = town, location_telephone = telephone, 
                                        location_postal_code = postcode, type_of_service ) %>% 
        mutate(postcode = gsub("[[:blank:]]", "", location_postal_code), 
               telephone = gsub("[[:blank:]]", "", location_telephone)) %>%
        select(registration_date, deregistration_date, location_name, postcode, telephone, ownership_type, ) %>% filter(is.na(deregistration_date)) %>% select(-deregistration_date)
      


# link old and new data and clean up 
    # - Idea: Get the initial date of registration and the registration in the market 

      old_new = left_join(active_jan18, csa, by = c("postcode", "telephone", "location_name")) %>% 
        arrange(postcode, telephone, location_name) %>%
        select(location_id, postcode, location_postal_code, location_name, telephone, location_telephone_number, everything(), -location_web_address, -registered_manager)
        
      
      old_new =  old_new %>% 
        group_by(postcode, telephone, location_name) %>%
        mutate_at(vars(registration_date, location_hsca_start_date), funs(as.character)) %>%
        mutate(initial_reg_date = ifelse(!is.na(registration_date), registration_date, location_hsca_start_date)) %>%
        group_by(postcode, telephone, location_name) %>%
        mutate(registration_hsca = row_number(), # registration in the hsca
               total_reg = n()) %>%              # total registrations in the hsca
        select(location_id:location_hsca_start_date, registration_date, initial_reg_date, registration_hsca, total_reg, everything()) %>%
        filter(registration_hsca == 1) %>%
        mutate_at(vars(initial_reg_date), funs(as.Date)) %>% arrange(initial_reg_date) 

# create vars for linking inactive 

 old_new = old_new %>% 
   mutate(location_status ="active",
                              location_hsca_end_date = NA, 
                              provider_status = "active", 
                              provider_hsca_end_date = NA) %>%
   select(-location_commissioning_ccg, -location_telephone_number,
           -provider_nominated_individual_name) %>%
   rename(location_ownership_type = ownership_type) %>%
   mutate_at(vars(ends_with("_date")), funs(as.character))

# ------------------
# INACTIVE CARE HOMES
# ------------------


inactive_jan18 = read_excel("data/raw/20180101 De-activated locations.xlsx", sheet = "De-activated Locations", range = "A7:AY34431")


inactive_jan18 = clean_names(inactive_jan18) %>% 
  filter(care_home == "Y") %>%
  mutate(postcode = gsub("[[:blank:]]", "", location_postal_code)) %>% 
  rename(care_homes_beds = care_homes_beds_at_point_location_de_activated) %>%
  select(-location_commissioning_ccg_name, -provider_ni_name) %>% 
  mutate(telephone = NA, 
         registration_date = NA, 
         initial_reg_date = NA, 
         registration_date = NA,
         registration_hsca = NA,
         total_reg = NA, 
         provider_ownership_type = NA,
         location_ownership_type = NA) 
         


setdiff(names(inactive_jan18), names(old_new))

# reorder variables for the same  
inactive_jan18 = inactive_jan18 %>% select(names(old_new)) %>% 
  mutate_at(vars(ends_with("_date")), funs(as.character))


care_homes = bind_rows(old_new, inactive_jan18) %>% arrange(location_id)

care_homes = care_homes %>% 
  mutate(initial_reg_date = ifelse(is.na(initial_reg_date), location_hsca_start_date, initial_reg_date)) %>% 
  select(location_id:initial_reg_date, location_hsca_end_date, everything())
  

care_homes = care_homes %>% group_by(postcode, telephone, location_name)

  
# -------------------------
# INFORMATION FROM THE ONS
# -------------------------

# geography 
ons = import("data/geography/ONSPD_NOV_2017_UK/Data/ONSPD_NOV_2017_UK.csv")

ons = ons %>%  mutate(postcode = gsub("[[:blank:]]", "", pcd)) %>% 
  select(pcd, postcode, oslaua, lsoa11, msoa11, imd) 

ons = ons %>% filter(postcode %in% unique(care_homes$postcode))

care_homes = left_join(care_homes, ons, by = "postcode")

care_homes = care_homes %>% filter(!is.na(oslaua))

# ----------------------------


write.csv(care_homes, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/care_homes.csv", row.names = FALSE)
