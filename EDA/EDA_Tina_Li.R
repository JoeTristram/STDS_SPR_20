# load packages ---------
library(tidyverse)
library(dplyr)
library(here)
library(lubridate)


########################################################
#Import 6 month's fuel data --------
library(tidyverse)
library(lubridate)
library(readxl)
library(here)

Fuel_Jul20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_july2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Jun20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_june2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_May20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_may2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Apr20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_apr2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Mar20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_mar2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Feb20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_feb2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Jan20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_jan2020.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Dec19<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_dec2019.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Nov19<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_nov2019.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Oct19<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_oct2019.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Sep19<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_sep2019.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")
Fuel_Aug19<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_service-station-price-history-aug-2019.xlsx",1,skip=2, col_names = TRUE))%>% 
  fill(everything(), .direction = "down")


NSWPublicHoliday<- read.csv("Public_Holiday_2019-2020\\australian_public_holidays_2020.csv")%>% 
  filter(Jurisdiction=="nsw")

# format date to date and rename column
NSWPublicHoliday$Date<- ymd(NSWPublicHoliday$Date)
NSWPublicHoliday<- NSWPublicHoliday %>% 
  rename(date=Date)

## Combine Fuel data Feb to Jul 20
Fuel_RAW<- rbind(Fuel_Aug19,Fuel_Sep19,Fuel_Oct19,Fuel_Nov19,Fuel_Dec19,Fuel_Jan20,Fuel_Feb20,Fuel_Mar20,Fuel_Apr20,Fuel_May20,Fuel_Jun20,Fuel_Jul20)
Fuel_RAW$PriceUpdatedDate<- dmy_hms(Fuel_RAW$PriceUpdatedDate)
Fuel_RAW$date <- as.Date(format(Fuel_RAW$PriceUpdatedDate, "%Y-%m-%d"))

## merge Fuel_RAW with public holiday
Fuel_All<- left_join(Fuel_RAW,NSWPublicHoliday,by="date")
write.csv(Fuel_All, "EDA/fuel_all.csv")

####################################################

# Load data ------
fuel_all <- read.csv(here("EDA","fuel_all.csv"))%>%
  filter(FuelCode=="P98"|FuelCode=="DL"|FuelCode=="LPG")

fuel_all$date<-dmy(fuel_all$date)
standard_unleaded <- list("E10", "U91")
premium_unleaded <- list("P95", "P98")
standard_diesel <-list("DL", "B20")
premium_diesel <- list("PDL")


# Lead in and Lead out public holiday --------------
# HolidayToInspect <- as.Date("2020-06-01"), Queens'Birthday
# FuelCodeToInspect<-"P98"
FiveDaysPrior<- c(format(seq(as.Date("2020-05-31"), length.out=5, by="-1 day"), format="%Y-%m-%d"))
FiveDaysAfter<- c(format(seq(as.Date("2020-06-02"), length.out=5, by="1 day"), format="%Y-%m-%d"))

# create subsets based on date
## FiveDatePriorData

FiveDatePriorData<-fuel_all[fuel_all$date %in% as.Date(FiveDaysPrior),]%>%
  filter(FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character("2020-06-01"), format="%Y-%m-%d")-as.Date(as.character(date), format="%Y-%m-%d"))%>%
  mutate (period="Before")

Avg_daily_fuel_price <- FiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

FiveDatePriorData<- left_join(FiveDatePriorData,Avg_daily_fuel_price,by="date")

## FiveDaysAfterData    
FiveDaysAfterData<-fuel_all[fuel_all$date %in% as.Date(FiveDaysAfter),]%>%
  filter(FuelCode=="P98")%>% 
  select(date,Brand, Postcode, Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character(date), format="%Y-%m-%d")-
           as.Date(as.character("2020-06-01"), format="%Y-%m-%d"))%>%
  mutate (period="After")

## Avg daily fuel price of 5 days after will be different from 5 days before
Avg_daily_fuel_price <- FiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

FiveDaysAfterData<- left_join(FiveDaysAfterData,Avg_daily_fuel_price,by="date")

# create station count by brand and grouping from fuel_all
brand_station_ct <- fuel_all %>% 
  group_by(Brand)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))


# create fuel price data 5 days before and after, then append station count and brand grouping
Before_After_Holiday <-rbind(FiveDaysAfterData, FiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")



# Create charts on 5 days before and after holiday fuel price

# geom_text(aes(day_number[5], daily_avg, label = "Average Daily Fuel Price"),vjust= -3,check_overlap = TRUE, colour = "red", stat = "identity")+

#chart for group_1, brand with less than 40 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)

#chart for group_2, brand with less than 100 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)

#chart for group_3, brand with less than 200 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)  

#chart for group_4, brand with less than 400 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1) 


## average fuel price by postcode -------

### standard_unleaded <- list("E10", "U91")
### premium_unleaded <- list("P95", "P98")

### Standard_unleaded
#### create subset
Postcode_fuel_all<- fuel_all%>% 
  filter(FuelCode==standard_unleaded)%>%
  select(Postcode,Price) %>%
  mutate(overall_fuel_avg=mean(Price))%>%
  group_by(Postcode,overall_fuel_avg)%>%
  summarise(postcode_fuel_avg = mean(Price))%>% 
  arrange(postcode_fuel_avg)


Postcode_fuel_all$Postcode<- as.factor(Postcode_fuel_all$Postcode)

station_ct_pc <- fuel_all%>% 
  select(Postcode,Brand,Price,ServiceStationName, FuelCode)%>%
  mutate(station_id = paste(ServiceStationName,Postcode))%>%
  group_by(Postcode)%>%
  summarise(station_ct = n_distinct(station_id))

station_ct_pc$Postcode<- as.factor(station_ct_pc$Postcode)
Postcode_fuel_all<- left_join(Postcode_fuel_all,station_ct_pc,by="Postcode")
  


### charts- by Fuel price by postcode on average fuel price over all types and all times
Postcode_fuel_all%>%
  tail(5)%>%
  ggplot(aes(x = Postcode, y = postcode_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(postcode_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Postcode_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Post Code")+
  ylab("Average Fuel Price $cent") +
  ggtitle("Most expensive fuel price by Postcode",subtitle = "Standard Unleaded (E10, U91) betw Feb 20 to Jul 20")

Postcode_fuel_all%>%
  head(5)%>%
  ggplot(aes(x = Postcode, y = postcode_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(postcode_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Postcode_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Post Code")+
  ylab("Average Fuel Price $cent") +
  ggtitle("Least expensive fuel price by Postcode",subtitle = "Standard Unleaded (E10, U91) betw Feb 20 to Jul 20")



### premium_unleaded_postcode
Postcode_fuel_all<- fuel_all%>% 
  filter(FuelCode==premium_unleaded)%>%
  select(Postcode,Price) %>%
  mutate(overall_fuel_avg=mean(Price))%>%
  group_by(Postcode,overall_fuel_avg)%>%
  summarise(postcode_fuel_avg = mean(Price))%>% 
  arrange(postcode_fuel_avg)

Postcode_fuel_all$Postcode<- as.factor(Postcode_fuel_all$Postcode)


### charts- by Fuel price by postcode on average fuel price over all types and all times
Postcode_fuel_all%>%
  tail(5)%>%
  ggplot(aes(x = Postcode, y = postcode_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(postcode_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Postcode_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Post Code")+
  ylab("Average Fuel Price $cent") +
  ggtitle("Most expensive fuel price by Postcode",subtitle = "Premium Unleaded (P98, P95) betw Feb 20 to Jul 20")

Postcode_fuel_all%>%
  head(5)%>%
  ggplot(aes(x = Postcode, y = postcode_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(postcode_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Postcode_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Post Code")+
  ylab("Average Fuel Price $cent") +
  ggtitle("Least expensive fuel price by Postcode",subtitle = "Premium Unleaded (P98, P95) betw Feb 20 to Jul 20")


## average fuel price by suburb -------
## standard_diesel <-list("DL", "B20")
## premium_diesel <- list("PDL")

## standard diesel by suburb
Suburb_fuel_all<- fuel_all%>% 
  filter(FuelCode==standard_diesel)%>%
  select(Suburb,Price) %>%
  mutate(overall_fuel_avg=mean(Price))%>%
  group_by(Suburb,overall_fuel_avg)%>%
  summarise(suburb_fuel_avg = mean(Price))%>% 
  arrange(suburb_fuel_avg)
head(Suburb_fuel_all)

## Charts on Fuel price by Suburb on average fuel price over all types and all times
Suburb_fuel_all%>%
  tail(5)%>%
  ggplot(aes(x = Suburb, y = suburb_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(suburb_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Suburb_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Suburb")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylab("Average Fuel Price $cent") +
  ggtitle("Most expensive fuel price by Suburb",subtitle = "Standard Diesel (DL, B20) betw Feb 20 to Jul 20")

Suburb_fuel_all%>%
  head(5)%>%
  ggplot(aes(x = Suburb, y = suburb_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(suburb_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Suburb_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Suburb")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylab("Average Fuel Price $cent") +
  ggtitle("Least expensive fuel price by Suburb",subtitle = "Standard Diesel (DL, B20) betw Feb 20 to Jul 20")



## Premium diesel by suburb
Suburb_fuel_all<- fuel_all%>% 
  filter(FuelCode==premium_diesel)%>%
  select(Suburb,Price) %>%
  mutate(overall_fuel_avg=mean(Price))%>%
  group_by(Suburb,overall_fuel_avg)%>%
  summarise(suburb_fuel_avg = mean(Price))%>% 
  arrange(suburb_fuel_avg)
head(Suburb_fuel_all)

## Charts on Fuel price by Suburb on average fuel price over all types and all times
Suburb_fuel_all%>%
  tail(5)%>%
  ggplot(aes(x = Suburb, y = suburb_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(suburb_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Suburb_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Suburb")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylab("Average Fuel Price $cent") +
  ggtitle("Most expensive fuel price by Suburb",subtitle = "Premium Diesel (PDL) betw Feb 20 to Jul 20")

Suburb_fuel_all%>%
  head(5)%>%
  ggplot(aes(x = Suburb, y = suburb_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(suburb_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Suburb_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Suburb")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylab("Average Fuel Price $cent") +
  ggtitle("Least expensive fuel price by Suburb",subtitle = "Premium Diesel (PDL) betw Feb 20 to Jul 20")



## Fuel code & postcode -----
library(tidyverse)
library(readxl)
library(here)
library(skimr)
library(kableExtra)

fuel_all$Postcode<- as.factor(fuel_all$Postcode)

fuelcode_PC <- fuel_all%>% 
  select(Postcode,Price,FuelCode)%>%
  group_by(Postcode,FuelCode)%>%
  summarise(pc_avg_fuel_price=mean(Price))%>% 
  spread(FuelCode,pc_avg_fuel_price)

## Fuel Station Postcode vs SA2 -----
## geocoded address from Google Cloud Platform "Geocoding API", require payment detail to get API key. Max 2500 addresses per day.
fuel_all <- read.csv(here("EDA","fuel_all.csv"))
fuel_all$Postcode<- as.character(fuel_all$Postcode)
str(fuel_all)

# not all addresses can be geocoded
address_geocoded <- read.csv("Postcode_SA2/address_geocoded.csv")

# one postcode might have 2 suburbs and 2 SA2 codes. eg. postcode 2019, here randomly choose one
SA2<-read.csv("Postcode_SA2/2019 Locality to 2016 SA2 Coding Index.csv")%>%
  select(SA2_MAINCODE,POSTCODE,STATE)
SA2<-SA2[!duplicated(SA2$POSTCODE), ]
names(SA2)[2] <- "Postcode"
SA2$Postcode<- as.character(SA2$Postcode)
str(SA2)


# result in 2325, allow same address has different station name
fuel_station_Aggr <- fuel_all%>%
  mutate(fulladdress=paste(trimws(Address),"AU"))%>%
  select(fulladdress,Brand,ServiceStationName,Postcode, Price,FuelCode)%>%
  group_by(fulladdress,Brand,ServiceStationName,Postcode,FuelCode)%>%
  summarise(pc_avg_fuel_price=mean(Price))%>%
  spread(FuelCode,pc_avg_fuel_price)%>%
  left_join(address_geocoded,by="fulladdress")%>%
  left_join(SA2, by = "Postcode")


fuel_station_Aggr_P98 <- fuel_station_Aggr%>%
  select(SA2_MAINCODE,P98)
fuel_station_Aggr_P98 <-fuel_station_Aggr_P98[complete.cases(fuel_station_Aggr_P98), ]

# Shapefile
#install.packages("sp")
#install.packages("rgdal")
library(sp)
library(rgdal)

SA2<- readOGR("Postcode_SA2\\1270055001_sa2_2016_aust_shape\\SA2_2016_AUST.shp")  
SA2_NSW<- SA2[SA2$STE_NAME16=="New South Wales", ]  
plot(SA2_NSW,main="New South Wales")

# Plot shapefile
## How to plot SA2_NSW with fuel_station_Aggr_P98???????
