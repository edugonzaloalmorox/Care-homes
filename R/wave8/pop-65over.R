################################################################################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Information on population: based on districts
# NOMIS
# Output: Dataset with population over 65 and share of population over 65 + per oslaua /year
##################################################################################################
library(readxl)

# number of adults 65 + 
      pop65 = read_excel("/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/raw/population-65-number.xlsx", sheet = "Data", range = "A7:I333")
      
      pop65 = clean_names(pop65) %>% rename(local_authority = local_authority_district_unitary_as_of_april_2015, 
                                            code = x_1)
      
      pop65_long = pop65 %>% 
        gather(year, population_65over, x2010:x2016) %>%
        mutate(year = gsub("x", "", year)) %>% 
        arrange(code, year)



# share of population over 65
      pop65share = read_excel("/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/raw/population-over65.xlsx", sheet = "Data", range = "A7:P334")
      
      pop65share = clean_names(pop65share) %>% rename(local_authority = local_authority_district_unitary_as_of_april_2015, 
                                                      code = x_1)
      
      share_pop65 = pop65share %>% select(local_authority, code, starts_with("x_")) 
      
      names_vars = pop65share %>% select(local_authority, code, starts_with("x2")) %>% colnames()
      
      names(share_pop65) <- names_vars 
      
      share_pop65 = share_pop65[-1, ]
      
      sharepop65_long = share_pop65 %>% 
        gather(year, share_pop65_over, x2010:x2016) %>%
        mutate(year = gsub("x", "", year)) %>% 
        arrange(code, year)

# link data 
pop65_long = left_join(pop65_long, sharepop65_long, by = c("code", "local_authority", "year"))

write.csv(pop65_long, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/pop65.csv", row.names = FALSE)

