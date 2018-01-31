###########################################
# New wave (8) 
# January 2018
# March 2010 - January 2018
# Unit of analysis - district (OSLAUA)
# Variables referred to quality of care homes
###########################################


# load data 
    quality2015 = read_excel("data/raw/01 December Latest ratings.xlsx", sheet = "Locations", range = "A1:N88086") %>% clean_names()
     
    
    quality2016 =  import("data/raw/quality-dec-2016.csv") %>% clean_names()
    
    quality2017 = import("data/raw/quality-dec-2017.csv") %>% clean_names()
    
    
qual_info = function(x){
  
  y =  x %>% filter(care_home == "Y") %>%
    filter(key_question  == "Overall") %>% 
    select(location_id, care_home, location_post_code, latest_rating, publication_date)
  
  return(y)
  
}

# clean data
    qual_2015 =  qual_info(quality2015) %>% 
      mutate_at(vars(publication_date), funs(as.character))
    
    
    qual_2016 =  qual_info(quality2016) 
    
    qual_2016 = qual_2016 %>%
      mutate(date = as.Date(publication_date, "%d/%m/%Y")) %>%
      select(-publication_date) %>%
      rename(publication_date = date) %>% 
      mutate_at(vars(publication_date), funs(as.character))
    
    
    
    qual_2017 =  qual_info(quality2017) %>% 
      mutate(date = as.Date(publication_date, "%d/%m/%Y")) %>%
      select(-publication_date) %>%
      rename(publication_date = date) %>% 
      mutate_at(vars(publication_date), funs(as.character))

# link data and drop repeated observations
      quality_carehome = bind_rows(qual_2015, qual_2016, qual_2017)
      
      quality_carehome = quality_carehome %>% 
        arrange(location_id, publication_date) %>%
        unique()%>%
        mutate_at(vars(publication_date), funs(as.Date)) %>%
        mutate(year_reg = year(publication_date)) 
      
      quality_carehome = quality_carehome %>%
        mutate(postcode = gsub("[[:blank:]]", "",location_post_code))
        
        
        

# link geography 

      # geography 
      ons = import("data/geography/ONSPD_NOV_2017_UK/Data/ONSPD_NOV_2017_UK.csv")
      
      ons = ons %>%  mutate(postcode = gsub("[[:blank:]]", "", pcd)) %>% 
        filter(postcode %in% unique(quality_carehome$postcode)) %>%
        select(pcd, postcode, oslaua) 
      
      quality_carehome_complete = left_join(quality_carehome, ons, by = "postcode")
      
      quality_oslaua = quality_carehome_complete %>%
        group_by(oslaua, year_reg,latest_rating) %>%
        tally() %>%
        arrange(latest_rating, oslaua, year_reg)
      
      write.csv(quality_oslaua, "/Users/Personas/Dropbox/PhD/ch1/market entry/care_homes/data/waves/eight/quality_oslaua.csv", row.names = FALSE)
      
      

