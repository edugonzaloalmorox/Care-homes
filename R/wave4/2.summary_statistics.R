############################################
# Calculate summary statistics
# 31/05/2017
###########################################




library(rio)
library(fBasics)
library(dplyr)

# -----------
# Load data 
# ---------

carehomes_oslauas =  import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_alloslauas_wave.csv")
carehomes = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/care_homes_waves_complete.csv")


######################
# Select variables   #
######################

# Reduced sample
      vars = carehomes %>% select(carehomespop, entry_rate, sum.bed,
                              av.size, outstandingpop, inadequatepop, badpop, goodpop,
                              geoavprice, delchange_maj1:av.share15)
      
      
      
      stats_vars = basicStats(vars)
      
      stats_vars = data.frame(t(stats_vars)) 
      
      stats_vars = stats_vars %>% select(nobs, Mean, Minimum, Maximum, Stdev)
      
      stats_vars = round(stats_vars, 4)

write.csv(stats_vars,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/stats.csv", row.names = TRUE)


# Amplified sample
      vars_oslauas = carehomes_oslauas %>% select(carehomespop, entry_rate, sum.bed,
                                  av.size, outstandingpop, inadequatepop, bad, goodpop,
                                  geoavprice, delchange_maj1:avshare15)
      
      
      stats_vars_oslauas = basicStats(vars_oslauas)
      
      stats_vars_oslauas = data.frame(t(stats_vars_oslauas)) 
      
      stats_vars_oslauas = stats_vars_oslauas %>% select(nobs, Mean, Minimum, Maximum, Stdev)
      
      stats_vars_oslauas= round(stats_vars_oslauas, 4)


write.csv(stats_vars_oslauas,"/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/five/stats_oslauas.csv", row.names = TRUE)
