# Entries and exits #
# Insert entries and exits in the geographical information 
# I consider the number of care homes per 65 people  - the accumulated care homes
# Idea: provide what the actual situation is - 


cqc = rio::import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.prices.pop_v1.csv")

cqc_clean = cqc %>% filter(registry %in% niveles) %>% select(-mean.price, -max.price, -min.price, -house.transactions, -beds.provider.la,
                                                             -beds.la, -share, -share2, -hhi)



cqc_geo = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/processed/cqc.geolocated.csv")

vars = cqc_geo %>% select(location.id, postcode2, oslaua)
vars =  unique(vars)


test = left_join(cqc_clean, vars, by = c("location.id", "postcode2"))

cqc_clean = test 
cqc_clean = cqc_clean %>% mutate_each(funs(as.Date), date)

# more than 1 entry 
check = cqc_clean %>% group_by(postcode, registry == "location.start") %>% filter(n()>1)

multientry = check %>% filter() unique(check$postcode2)

# select "novo" entries and "definite" exits 

novo.year = cqc_clean %>% group_by(year.entry, oslaua) %>% filter(flow.entry == "novo") %>% tally() %>% rename(entries = n)
exit= cqc_clean %>% filter(flow.entry == "exit.def" ) %>% mutate(year.exit = year(date))
exit.year = exit %>% group_by(year.exit, oslaua) %>% tally()   %>% rename(exits = n)

novo.year$year.entry = as.numeric(novo.year$year.entry)


test = left_join(novo.year, exit.year, by = c("year.entry" = "year.exit", "oslaua" = "oslaua"))

#rename the missing data

test = test %>% mutate(exits = ifelse(is.na(exits), 0, exits),
                       net = entries - exits) 

# create a new dataset with all the possible years 

osl = rep(unique(test$oslaua), 7)

final = rep(2010:2016, 325)

df = data.frame(osl, final) %>% arrange(osl, final)

final = left_join(df, test, by = c("final" = "year.entry", "osl" = "oslaua"))


# fill the missing data with 0
finaldata = final %>% mutate(exits = ifelse(is.na(exits), 0, exits),
                         entries = ifelse(is.na(entries), 0, entries),
                         net =  entries - exits)
 





# count the cumulative entries per oslaua

finaldata = finaldata %>% arrange(osl, final) 

finaldata = finaldata %>% group_by(osl) %>% mutate(acc = cumsum(net))




# population
pop65 = read.csv("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/data/waves/one/nomis_pop_5yearageband.csv", 
                 sep = ";",
                 header = TRUE)


pop65_long = gather(pop65, year, population, -oslaua) %>% arrange(oslaua) %>% mutate(year = gsub("[[:alpha:]]", "", year))

test = left_join(finaldata, pop)

pop65 = pop65_long %>% group_by(oslaua) %>% summarise(mean.pop = mean(population))
  
test = left_join(finaldata, pop65, by = c("osl" = "oslaua"))


test = test %>% filter(final == 2016) 
test = test %>% mutate(ch.pop = acc/mean.pop)

setdiff(unique(data_mapping$oslaua), unique(test$osl))

chomes = test %>% select(osl, ch.pop)

test1 = left_join(data_mapping, chomes, by = c("oslaua" = "osl"))
data_mapping = test1

