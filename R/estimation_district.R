# Estimation at district level 
 
# Created: 25/10/2016
# Modified: 25/10/2016


# Rationale: the information concerning the planning decisions is at district level. 
# Information referred to instruments is related to district level


# ---------------------------------------
# Load regulatory data - for instruments 
# ---------------------------------------


#-------------------------------
library(rio)
library(dplyr)
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
#-------------------------------


cqc  = import("cqc.prices.pop_v1.csv")
cqc_geo = import("cqc.geolocated.csv")

# link local district authorities 

