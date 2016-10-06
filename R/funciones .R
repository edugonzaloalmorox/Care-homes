# functions for data management
# -----------------------------


# rename variables of the dataset 
      rename.var <- function(dta){
        
        x = tolower(colnames(dta))
        x = gsub(" ", ".", x)
        x = gsub("\\s*\\([^\\)]+\\)","",as.character(x))
       
        return(x)
      }


# clean care homes names
      clean <- function(dta){
        dta = gsub("[[:digit:]]+", "", dta)
        dta = str_trim(dta, side = "both")
        dta = gsub(".*-","",dta)
        dta = str_replace_all(dta, "[[:punct:]]", "")
        return(dta)
      }

# matching words for creating a key variable
      key = function(dta){
        
        nombres = dta %>% select(name1, name2)
        
        nombres$key <- apply(nombres,1,function(x) 
          paste(Reduce(intersect, strsplit(x, " ")), collapse = " "))
        nombres = nombres %>% select(key.2)
        dta = cbind(dta, nombres)
        return(dta)
      }
      
# recoding missing information associated with variables 
      
      recode.missing <- function(dta, g1, g2, a){
        # dta is data frame
        # g1 : grouping variable 1
        # g2 : grouping variable 2 
        # a is variabe
      dta = dta %>%
        group_by(g1, g2) %>%
        mutate(a=na.omit(a)[1])
      
      return(dta)
      
      }
# creating long data.frames with neighbourhood statistics 
      
      neighbour <-  function(dta){
        
        dta = dta %>% gather(year, pension.credit, feb.2010:feb.2016)
        return(dta)
      }