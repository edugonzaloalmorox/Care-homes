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


# Create lagged house prices
      
      # import main dataset 
      care_homes_complete = import("data/waves/eight/care_homes_complete_sample.csv")
      
      # import data on house prices
      house_prices = import("data/waves/eight/house_prices_oslaua_year.csv")
      
      # create lagged variables and rename 
      house_prices = house_prices %>% mutate(years_lag2 = year_transaction + 2,
                                             years_lag1 = year_transaction + 1,
                                             years_lag3 = year_transaction + 3)
      
      lag1_prices= house_prices %>% select(oslaua, years_lag1, mean_price_lag1 = mean_price)
      
      lag2_prices= house_prices %>% select(oslaua, years_lag2, mean_price_lag2 = mean_price)
      
      lag3_prices= house_prices %>% select(oslaua, years_lag3, mean_price_lag3 = mean_price)


      


#Link lagged prices to main sample

df_complete_test = left_join(care_homes_complete, lag1_prices, by = c("oslauas" = "oslaua",
                                                                   "years" = "years_lag1"))

df_complete_test = left_join(df_complete_test, lag2_prices, by = c("oslauas" = "oslaua",
                                                                      "years" = "years_lag2"))

df_complete_test = left_join(df_complete_test, lag3_prices, by = c("oslauas" = "oslaua",
                                                                   "years" = "years_lag3"))


write.csv(df_complete_test, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/care_homes_complete_sample.csv", row.names = FALSE )
