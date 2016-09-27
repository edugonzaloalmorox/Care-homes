# functions for data handling
# --------------------------



# rename variables of the dataset 
      rename.var <- function(dta){
        
        x = tolower(colnames(dta))
        x = gsub(" ", ".", x)
        x = gsub("\\s*\\([^\\)]+\\)","",as.character(x))
       
        return(x)
      }


#clean care homes names
      clean <- function(dta){
        dta = gsub("[[:digit:]]+", "", dta)
        dta = str_trim(dta, side = "both")
        return(dta)
      }

