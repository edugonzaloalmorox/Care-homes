# Figures
# -------
library(rio)
library(ggplot2)
library(gridExtra)

# figure: house prices trends 
# --------------------------


prices = read.csv("http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv", quote = "", sep = ",", header = TRUE)



library(rio)
prices = import("http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv")



vars = c("item", "price",	"date", "postcode",	"property.type", "old/new", "duration",	"PAON",	"SAON",	"street",	"locality",	"city",	"district",	"county",	"PPD","record.status")
colnames(prices) = vars


library(dplyr)

library(lubridate)


prices = prices %>% mutate(date1 = as.Date(date, format = "%Y-%m-%d"))
prices$date.1 = with(prices, as.Date(date.1, format = "%Y-%m-%d"))
prices$year = year(prices$date.1)

prices = prices %>% select(-item, -property.type, -`old/new`, -duration, -PAON, -SAON, -PPD, -record.status, -date)

prices = prices %>% filter(postcode != " ")

# select region and county from geo inforamtion

geo = import("/Users/Personas/Downloads/ONSPD_AUG_2016_UK_V2/Data/onspd_aug16.csv")

prueba = geo %>% select(oslaua, streg)
prueba = prueba %>% filter(streg %in% groups)


geo = geo %>% mutate(post = gsub("[[:space:]]", "", pcd)) %>% select(pcd, post, region = gor, region2 = streg, lat, long)
geo = geo %>% mutate(post = trimws(post))


prices = prices %>% mutate(post = gsub("[[:space:]]", "", postcode)) 
  prices = prices %>% mutate(post = trimws(post))

geo_prices = left_join(prices, geo, by = "post")
prices_clean = geo_prices %>% filter(!is.na(region2))

caca = c(0, NA)
first = geo_prices %>% filter(!(region2 %in% caca))

first = first %>% group_by(region2, year) %>% mutate(mean_price = mean(price) )

first_clean = first %>% select(mean_price, region2, year) %>%  arrange(year, region2) %>% unique() %>% ungroup()

write.csv(first_clean, "/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /figures/wave3/figures/prices_regions.csv", row.names = FALSE)

prices = prices %>% select(price, year, gor)
prices = prices %>% group_by(year, gor) %>% mutate(mean_price = mean(price)) %>% select(-price) %>% unique()




# recode regions 

first_clean = import("/Users/Personas/My Cloud/PhD _october_2016/market entry/care_homes/output /figures/wave3/figures/prices_regions.csv")

# deflate 

first_clean = first_clean %>% mutate(real_prices = mean_price/0.8 )

first_clean = first_clean  %>% mutate(stregion = ifelse(region == "E12000001", "North East",
                                                        ifelse(region == "E12000002", "North West",
                                                          ifelse(region == "E12000003", "Yorkshire and the Humber",
                                                            ifelse(region == "E12000004", "East Midlands",
                                                              ifelse(region == "E12000005", "West Midlands",
                                                                      ifelse(region == "E12000006", "East",
                                                                             ifelse(region == "E12000007", "London", 
                                                                                    ifelse(region == "E12000008", "South East",
                                                                                           ifelse(region == "E12000009", "South West", "other"))))))))))
                                                        
                                                        
                                                        
                                                        
             


library(dplyr)
library(ggplot2)

testref = first_clean %>% rename(vargroup = stregion) 
test = first_clean

zp3 <- ggplot(test,
              aes(x = year, y = (mean_price/1000), group = stregion))

zp3 <- zp3 + geom_line(data = testref,  # Plotting the "underlayer"
                       aes(x = year, y = (mean_price/1000), group = vargroup),
                       colour = "GRAY", alpha = 1/2, size = 1/2)
zp3

zp3 <- zp3 + geom_line(size = 0.5, colour = "blue")  # Drawing the "overlayer"


zp3 <- zp3 + facet_wrap(~ stregion)

zp3 = zp3 + ylab("House price (£ x 1000)") + xlab("Year")  
zp3

# ----------------------------



testref = first_clean %>% rename(vargroup = stregion) 
test = first_clean

zp3 <- ggplot(test,
              aes(x = year, y = real_prices, group = stregion))

zp3 <- zp3 + geom_line(data = testref,  # Plotting the "underlayer"
                       aes(x = year, y = real_prices, group = vargroup),
                       colour = "GRAY", alpha = 1/2, size = 1/2)
zp3

zp3 <- zp3 + geom_line(size = 0.5, colour = "blue")  # Drawing the "overlayer"


zp3 <- zp3 + facet_wrap(~ stregion)

zp3 = zp3 + ylab("House prices (£)") + xlab("Year")  
zp3








# figure: first stage
# --------------------

refusal_prices = ggplot(test5, aes(log(mean_price), refusal_maj_7908, colour =  location.region)) +
  geom_point(alpha= 1) + geom_smooth(method = "lm", se = TRUE, alpha = 0.025) + labs(x = "House prices (log)", 
                                                                                     y  = "Refusal rate",
                                                                                     title = "House prices and refusal rate",
                                                                                     colour = "Region")


refusal_prices

labour_prices = ggplot(test5, aes(log(mean_price), labourvotes1983, colour =  location.region)) + 
  geom_point(alpha= 1) + geom_smooth(method = "lm", se = TRUE, alpha = 0.025) + labs(x = "House prices (log)", 
                                                                                     y  = "Share of Labour vote",
                                                                                     title = "House prices and Labour shares",
                                                                                     colour = "Region") + theme(legend.position = "bottom")

labour_prices

labour_prices <- arrangeGrob(grobs = labour_prices)

delay_prices = ggplot(test5, aes(log(mean_price), delchange_maj1, colour =  location.region)) + 
  geom_point(alpha= 1) + geom_smooth(method = "lm", se = TRUE, alpha = 0.025) + labs(x = "House prices (log)", 
                                                                                     y  = "Rate of delay change",
                                                                                     title = "House prices and rate of change in delay", 
                                                                                     colour = "Region") + theme(legend.position = "bottom")



delay_prices

delay_prices <- arrangeGrob(grobs = delay_prices)


ex = grid.arrange(labour_prices, delay_prices, nrow = 1, ncol = 2)





