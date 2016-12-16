# Create a calendar heatmap

#     It is necessary to transform the data and obtain information relative to weeks, months, 
#     day of the week, etc.. 
#     
#     Heatmap makes faceting by month and year 


library(rio)
library(dplyr)
library(lubridate)     
library(ggplot2)
library(RColorBrewer) 


# Load data and select locations
       cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc.geolocated.csv")
       cqc = cqc %>% filter(registry == "location.start") %>% select(location.id, date)

# Set the dates and filter for the sample 
       cqc$date = as.POSIXct(cqc$date)

        cqc = cqc %>% filter(date >= "2011-03-01") %>% arrange(date)

# Count occurrences by day 
        cqc = cqc %>% group_by(date) %>% tally 


# Create variables to determine different time indicators
        cqc$day  = day(cqc$date) # dayofweek
        cqc$week_month = stringi::stri_datetime_fields(cqc$date)$WeekOfMonth
        cqc$cdow = wday(cqc$date,label=T)
        cqc$cdow = with(cqc, factor(cdow, levels = c("Mon","Tues", "Wed" ,  "Thurs", "Fri" ,  "Sat", "Sun" ))) # reorder the levels, it´s good for the 
        cqc$DoW = with(cqc, factor(cdow, levels = rev(levels(cdow)))) # label
        cqc$week = week(cqc$date)
        cqc$month = month(cqc$date, label = T)
        cqc$year = year(cqc$date)
        

# Heatmap 
        
        entries <- ggplot(cqc, aes(week_month, DoW, fill = n)) +
          geom_tile(colour = "white") + scale_fill_gradient(limits=c(0,10), low = "white", high = muted("grey") , na.value = "#336699") +
          facet_grid(year ~ month ) + theme(legend.position='none') + 
          labs(title = "Registrations of care homes 2013 - 2016") +
          xlab("Week of Month") +
          ylab("")
        
        # limits  help to determine 
        entries
        
       
        
