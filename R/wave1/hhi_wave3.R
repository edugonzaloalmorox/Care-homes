
# Calculation of HHI 

# created: 19/10/2016
# modified: 11/11/2016
# author: Edu Gonzalo (Newcastle University)
# ------------------------------------------

# Idea: Calculate the HHI for each local authority considering the number of beds of each provideer per wave.
#       It´s important to consider the cumulative number of beds of each provider in each local authority in each wave 
#       in addition to the beds that new care homes bring to the market. 
#       Note:  These calculations may change depending on the time frames that are considered (This is important)
#       
#             Wave: 
#             0 from 2010 to march 2011
#             1 from march 2011 to march 2014
#             2 from march 2011 to march 2015
#             2 form march 2015 onwards (sept 2016)
#-----------------------------------------------------------------------------------------------------------------------

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed")

library(pander)
library(rio)
library(dplyr)
library(plm)
library(stringr)
library(tibble)
library(XML)
library(stringi)
library(tidyr)
library(plyr)
library(magrittr)
library(ggplot2)
library(pglm)
library(stargazer)
library(lme4)
library(AER)
library(Hmisc)



data = import("data.csv")

# -----------------
# Prepare the data
# -----------------

df = data %>% mutate_each(funs(as.Date), hsca.start.date, hsca.end.date) %>%
  mutate(year.entry = format(hsca.start.date, "%Y"),
         year.exit = format(hsca.end.date, "%Y")) %>%
  select(location.status, provider.id, local.authority, year.entry, year.exit, care.homes.beds, hsca.start.date, hsca.end.date) %>% arrange(hsca.start.date)

# define the waves 

df  = df %>% mutate(wave.entry = ifelse(hsca.start.date < "2011-03-01", 0,
                                        ifelse(hsca.start.date >= "2011-03-01" & hsca.start.date < "2013-03-01", 1, 
                                               ifelse(hsca.start.date >= "2013-03-01" & hsca.start.date < "2015-03-01", 2,
                                                      ifelse(hsca.start.date >= "2015-03-01", 3, NA)))))


df  = df %>% mutate(wave.exit = ifelse(hsca.end.date < "2011-03-01", 0,
                                       ifelse(hsca.end.date >= "2011-03-01" & hsca.end.date < "2013-03-01", 1, 
                                              ifelse(hsca.end.date >= "2013-03-01" & hsca.end.date < "2015-03-01", 2,
                                                     ifelse(hsca.end.date >= "2015-03-01", 3, NA)))))


# NAs in wave exit are still active - go to last wave 
df = df %>% mutate(wave.exit = ifelse(is.na(hsca.end.date), 3,wave.exit))


# expand the data 

y = df %>%
  rowwise() %>%
  do(data.frame(provider.id = .$provider.id, 
                local.authority = .$local.authority,
                beds = .$care.homes.beds, 
                year = seq(.$wave.entry, .$wave.exit, by = 1),
                status = .$location.status))


y = y %>% mutate(year = as.factor(year)) %>% arrange(local.authority, provider.id)
y = unique(y) # remove  duplicates 


# - Select the last observation of each care provider for each local authority 
# Get drop of those observations where the care home is inactive 
#   -  assumption: if that wave the care home gets unregistered then is not active that year 

last = y %>% group_by(local.authority, provider.id, beds) %>% 
  filter(row_number() == n()) %>%
  mutate(toy = 0) %>%
  arrange(provider.id, local.authority, year)

# - Link the last observations 
y_linked = left_join(y, last, by = c("provider.id","local.authority", "beds", "year", "status"))

# - Arrange
y_linked = y_linked %>% mutate_each(funs(as.factor), provider.id, local.authority, status)


# - Recode the dummy - where the last is active is a NA and therefore not removable 
y_linked = y_linked %>% mutate(toy = ifelse(status == "Active", NA, toy)) %>% as.data.frame()

# - Get rid of the '0' (unactive years)
y_clean = y_linked %>% filter(is.na(toy)) %>% select(-toy, status)


write.csv(y_clean, "hhi.sample3.csv", row.names = FALSE) # hhi sample wave2 (2 waves of three years)


# ---------------
# Count the beds
# ---------------

y_clean = import("hhi.sample3.csv")

# Note: year represents the wave (0: before march 2011, 1: between march 2011 - march 2013, 2: march 2013 - march2015 -  3: march2015 - sept2016)

y_clean = y_clean %>% mutate_each(funs(as.numeric), beds)

prueba =  y_clean %>%
  dplyr::group_by(local.authority, provider.id, year) %>%
  dplyr::mutate(beds.provider.la = sum(beds, na.rm = TRUE))


prueba = prueba  %>%
  dplyr:: group_by(local.authority,year) %>%
  dplyr:: mutate(beds.la = sum(beds, na.rm = TRUE))

prueba = prueba %>% arrange(local.authority, year) %>% select(-status)

# Get an observation per provider - duplicated rows of shares won´t sum 1 otherwise           

prueba = prueba %>% group_by(local.authority, year) %>% 
  dplyr::mutate(share = (beds.provider.la/beds.la),
                check = sum(share)) # check is not considering single proportions

prueba_unique  = prueba %>% group_by(local.authority, provider.id,year) %>% 
  distinct(beds.provider.la, .keep_all = TRUE) %>%
  select(-check) %>%
  as.data.frame()

# check that the shares are right - should be 1 
# -----------------------------------------------------------
check =prueba_unique_hhi%>% group_by(local.authority, year) %>% 
  dplyr::mutate(share = (beds.provider.la/beds.la),
                check = sum(share))

rm(check)
# ----------------------------------------------------

prueba_unique_hhi = prueba_unique %>% group_by(local.authority, year) %>% 
  dplyr::mutate(share = (beds.provider.la/beds.la),
                share2 = share^2,
                hhi = sum(share2))

# Save data 
write.csv(prueba_unique_hhi, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/data.hhi.lsa.wave3.csv", row.names = FALSE) 
# data referred to the hhi per local authority in wave3

# -------------------------------------
# link HHI data to local planning data 
# -------------------------------------



hhi_wave= import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/data.hhi.lsa.wave3.csv")
cqc = import("cqc_prices_pop_district.csv") 

cqc_hhi = cqc %>% select(local.authority, lpa = la) %>% unique()
hhi_wave_district = hhi_wave %>% select(local.authority, year, hhi) %>% unique()


test = left_join(cqc_hhi, hhi_wave_district, by = "local.authority")

setwd("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves")

write.csv(test, "hhi_district_la_wave3.csv", row.names = FALSE)

hhi_district = import("hhi_district_la_wave3.csv") %>% filter(year != 0) %>% select(local.authority, lpa, wave = year, hhi)
s1 = import("sample_entry.csv")

# clean garbage in lpa 


hhi_district = hhi_district %>% mutate(lpa = tolower(lpa)) %>%
  mutate(lpa = gsub(", city of ua", "", lpa)) %>%
  mutate(lpa = gsub("ua", "", lpa)) %>%
  mutate(lpa = gsub(", county of", "", lpa)) %>% 
  mutate(lpa = gsub("[.]", "", lpa))%>% 
  mutate(lpa = str_trim(lpa, side ="both")) 

# link data 

test.2 = left_join(s1, hhi_district, by = c("lpa", "wave"))

check = test.2 %>% group_by(lpa) %>% filter(n()>3)

crappy_lpa = levels(as.factor(check$lpa))

test.3 = test.2[-c(125,209,237,320), ]
