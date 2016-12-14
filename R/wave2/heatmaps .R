# Statistical evidence


# calendar heatmaps of time series



cqc = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/two/cqc.geolocated.csv")

cqc = cqc %>% filter(registry == "location.start")


library(lubridate)     # for wday(...)
library(ggplot2)
library(RColorBrewer)  # for brewer.pal(...)

cqc$date <- as.POSIXct(cqc$date, format="%Y-%m-%d")
cqc$dow  <- wday(cqc$date)
cqc$cdow <- wday(cqc$date,label=T)
cqc$tod  <- as.numeric(cqc$date - as.POSIXct(strftime(cqc$date,format="%Y-%m-%d")))/60
cqc$bins <- cut(cqc$tod,breaks=1:24,labels=F)
counts    <- aggregate(ID~bins+dow,data,length)
colnames(counts)[ncol(counts)] <- "Events"

ggplot(counts, aes(x=bins,y=8-dow))+
  geom_tile(aes(fill=Events))+
  scale_fill_gradientn(colours=brewer.pal(9,"YlOrRd"),
                       breaks=seq(0,max(counts$Events),by=2))+
  scale_y_continuous(breaks=7:1,labels=c("Sun","Mon","Tues","Wed","Thurs","Fri","Sat"))+
  labs(x="Time of Day (hours)", y="Day of Week")+
  coord_fixed()