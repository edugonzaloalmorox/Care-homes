############################################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Information on instruments and other controls: based on districts
# Output: Dataset with the intruments  used for the analysis
# Use former dataset "care_homes_complete.dta"
#############################################################

care_homes_complete = import("data/waves/seven/stata_files/care_homes_complete.dta")

instruments = care_homes_complete %>% 
  select(oslaua, location_region_east_midlands:location_region_west_midlands, 
         location_region_yorkshire = location_region_yorkshire_and_th, delchange_maj1:av_share15) %>%
  unique()

write.csv(instruments, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/instruments_controls.csv", row.names = FALSE)
