# load data 

#clean variables
clean <- function(dta){
  dta = gsub("[[:digit:]]+", "", dta)
  dta = str_trim(dta, side = "both")
  return(dta)
}