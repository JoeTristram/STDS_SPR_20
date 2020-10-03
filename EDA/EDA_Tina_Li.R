# load packages ---------
library(tidyverse)
library(dplyr)
library(here)
library(lubridate)
library(readxl)
#install.packages("sp")
#install.packages("rgdal")
library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(ggpubr)
library(gridExtra)

########################################################
#Import/combine 12 month's fuel data --------
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
  filter(FuelCode=="P98"|FuelCode=="DL"|FuelCode=="LPG")%>%
  filter(date >= '2019-08-01' & date <= '2020-07-31')

fuel_all$date<-ymd(fuel_all$date)
fuel_all$Price<-as.numeric(fuel_all$Price)
fuel_all$Postcode<- as.character(fuel_all$Postcode)

standard_unleaded <- list("E10", "U91")
premium_unleaded <- list("P95", "P98")
standard_diesel <-list("DL", "B20")
premium_diesel <- list("PDL")

brand_station_ct <- fuel_all %>% 
  group_by(Brand)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", 
                                ifelse(brand_station_CT < 100, "Group_2", 
                                       ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))


House_data <- read.csv(here("House_Price_Data","Combined_HousePriceFebToJul20.csv"))

# Lead in and Lead out public holiday --------------
# HolidayToInspect <- as.Date("2020-06-01"), Queens'Birthday
# HolidayToInspect <- as.Date("2020-01-27"), Australia Day
# FuelCodeToInspect<-"P98"
FiveDaysPrior<- c(format(seq(as.Date("2020-05-31"), length.out=5, by="-1 day"), format="%Y-%m-%d"))
FiveDaysAfter<- c(format(seq(as.Date("2020-06-02"), length.out=5, by="1 day"), format="%Y-%m-%d"))
AUFiveDaysPrior<- c(format(seq(as.Date("2020-01-26"), length.out=5, by="-1 day"), format="%Y-%m-%d"))
AUFiveDaysAfter<- c(format(seq(as.Date("2020-01-28"), length.out=5, by="1 day"), format="%Y-%m-%d"))

# create subsets based on date
## FiveDatePriorData - Queen's Birthday
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

Avg_daily_fuel_price <- FiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

FiveDaysAfterData<- left_join(FiveDaysAfterData,Avg_daily_fuel_price,by="date")

Before_After_Holiday <-rbind(FiveDaysAfterData, FiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")


# Create charts on 5 days before and after holiday fuel price
#chart for group_1, brand with less than 40 stations

QB_40 <- Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Queen's Birthday", subtitle = "Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)


#chart for group_2, brand with less than 100 stations

QB_100 <- Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Queen's Birthday", subtitle = "Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)


#chart for group_3, brand with less than 200 stations

QB_200 <- Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Queen's Birthday", subtitle = "Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)  


#chart for group_4, brand with less than 400 stations

QB_400 <- Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Queen's Birthday", subtitle = "Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1) 




## FiveDatePriorData - Australian Day
AUFiveDatePriorData<-fuel_all[fuel_all$date %in% as.Date(AUFiveDaysPrior),]%>%
  filter(FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character("2020-01-27"), format="%Y-%m-%d")-as.Date(as.character(date), format="%Y-%m-%d"))%>%
  mutate (period="Before")

Avg_daily_fuel_price <- AUFiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

AUFiveDatePriorData<- left_join(AUFiveDatePriorData,Avg_daily_fuel_price,by="date")

## AUAUFiveDaysAfterData    
AUFiveDaysAfterData<-fuel_all[fuel_all$date %in% as.Date(AUFiveDaysAfter),]%>%
  filter(FuelCode=="P98")%>% 
  select(date,Brand, Postcode, Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character(date), format="%Y-%m-%d")-
           as.Date(as.character("2020-01-27"), format="%Y-%m-%d"))%>%
  mutate (period="After")

Avg_daily_fuel_price <- AUFiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

AUFiveDaysAfterData<- left_join(AUFiveDaysAfterData,Avg_daily_fuel_price,by="date")

Before_After_Holiday <-rbind(AUFiveDaysAfterData, AUFiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")


# Create charts on 5 days before and after holiday fuel price
#chart for group_1, brand with less than 40 stations

AD_40 <- Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Australian Day", subtitle = "Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)


#chart for group_2, brand with less than 100 stations

AD_100 <- Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Australian Day", subtitle = "Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)


#chart for group_3, brand with less than 200 stations


AD_200 <- Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Australian Day", subtitle = "Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)  


#chart for group_4, brand with less than 400 stations

AD_400 <- Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 Australian Day", subtitle = "Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1) 



# Lead in and Lead out school holiday --------------
# https://education.nsw.gov.au/public-schools/going-to-a-public-school/calendars
# 2019 Term 4, From 14/10/2019 to 20/12/2019
# 2019 Term 4 school holiday, start from 21/12/2019 to 28/1/2020
# 2020 Term 1, From 28/01/2020 to 09/04/2020 
# 2020 Term 1 school holiday, start from 10/04/2020 to 26/4/2020

T4FiveDaysPrior<- c(format(seq(as.Date("2019-12-20"), length.out=5, by="-1 day"), format="%Y-%m-%d"))
T4FiveDaysAfter<- c(format(seq(as.Date("2020-01-29"), length.out=5, by="1 day"), format="%Y-%m-%d"))
T1FiveDaysPrior<- c(format(seq(as.Date("2020-04-09"), length.out=5, by="-1 day"), format="%Y-%m-%d"))
T1FiveDaysAfter<- c(format(seq(as.Date("2020-04-27"), length.out=5, by="1 day"), format="%Y-%m-%d"))



### Prep data for plot - 2019 T4 Lead in

T4FiveDatePriorData<-fuel_all[fuel_all$date %in% as.Date(T4FiveDaysPrior),]%>%
  filter(FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character("2019-12-21"), format="%Y-%m-%d")-as.Date(as.character(date), format="%Y-%m-%d"))%>%
  mutate (period="Before")

Avg_daily_fuel_price <- T4FiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

T4FiveDatePriorData<- left_join(T4FiveDatePriorData,Avg_daily_fuel_price,by="date") 

## T1FiveDaysAfterData    
T4FiveDaysAfterData<-fuel_all[fuel_all$date %in% as.Date(T4FiveDaysAfter),]%>%
  filter(FuelCode=="P98")%>% 
  select(date,Brand, Postcode, Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character(date), format="%Y-%m-%d")-
           as.Date(as.character("2020-01-28"), format="%Y-%m-%d"))%>%
  mutate (period="After")

## Avg daily fuel price of 5 days after will be different from 5 days before
Avg_daily_fuel_price <- T4FiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

T4FiveDaysAfterData<- left_join(T4FiveDaysAfterData,Avg_daily_fuel_price,by="date")

## Combine Before and after
T4Before_After_Holiday <-rbind(T4FiveDaysAfterData, T4FiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")




### Ploting
### lead in lead out 2019 T4 school holiday, chart 2019T4FivedayLessThan40Brands


T4_40 <- T4Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after school holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 T4 school holiday", subtitle = "Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)


#chart for group_2, brand with less than 100 stations, chart 2019T4FivedayLessThan100Brands

T4_100 <- T4Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after school holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 T4 school holiday", subtitle = "Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)



#chart for group_3, brand with less than 200 stations,chart 2019T4FivedayLessThan200Brands


T4_200 <- T4Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 T4 school holiday", subtitle = "Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)  



#chart for group_4, brand with less than 400 stations


T4_400 <- T4Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 T4 school holiday", subtitle = "Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)




### Prep data for plot - 2020 T1 

T1FiveDatePriorData<-fuel_all[fuel_all$date %in% as.Date(T1FiveDaysPrior),]%>%
  filter(FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character("2020-04-10"), format="%Y-%m-%d")-as.Date(as.character(date), format="%Y-%m-%d"))%>%
  mutate (period="Before")

Avg_daily_fuel_price <- T1FiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

T1FiveDatePriorData<- left_join(T1FiveDatePriorData,Avg_daily_fuel_price,by="date") 

## T1FiveDaysAfterData    
T1FiveDaysAfterData<-fuel_all[fuel_all$date %in% as.Date(T1FiveDaysAfter),]%>%
  filter(FuelCode=="P98")%>% 
  select(date,Brand, Postcode, Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character(date), format="%Y-%m-%d")-
           as.Date(as.character("2020-04-26"), format="%Y-%m-%d"))%>%
  mutate (period="After")

## Avg daily fuel price of 5 days after will be different from 5 days before
Avg_daily_fuel_price <- T1FiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

T1FiveDaysAfterData<- left_join(T1FiveDaysAfterData,Avg_daily_fuel_price,by="date")

## Combine Before and after
T1Before_After_Holiday <-rbind(T1FiveDaysAfterData, T1FiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")




### Ploting
### lead in lead out 2020 T1 school holiday, chart 2020T1FivedayLessThan40Brands

T1_40 <- T1Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after school holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 T1 school holiday", subtitle = "Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)

#chart for group_2, brand with less than 100 stations, chart 2020T1FivedayLessThan100Brands

T1_100 <- T1Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after school holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 T1 school holiday", subtitle = "Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)



#chart for group_3, brand with less than 200 stations,chart 2020T1FivedayLessThan200Brands
T1_200 <- T1Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 T1 school holiday", subtitle = "Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)  


#chart for group_4, brand with less than 400 stations
T1_400 <- T1Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2020 T1 school holiday", subtitle = "Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)



# Prep Data for 2019 XMAS 
# 2019 Christmas holiday period = 2019-12-24 to 2020-01-06
XMAS19FiveDaysPrior<- c(format(seq(as.Date("2019-12-23"), length.out=10, by="-1 day"), format="%Y-%m-%d"))
XMAS19FiveDaysAfter<- c(format(seq(as.Date("2020-01-07"), length.out=10, by="1 day"), format="%Y-%m-%d"))

XMAS19FiveDatePriorData<-fuel_all[fuel_all$date %in% as.Date(XMAS19FiveDaysPrior),]%>%
  filter(FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character("2019-12-24"), format="%Y-%m-%d")-as.Date(as.character(date), format="%Y-%m-%d"))%>%
  mutate (period="Before")

Avg_daily_fuel_price <- XMAS19FiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

XMAS19FiveDatePriorData<- left_join(XMAS19FiveDatePriorData,Avg_daily_fuel_price,by="date") 

## XMAS19FiveDaysAfterData    
XMAS19FiveDaysAfterData<-fuel_all[fuel_all$date %in% as.Date(XMAS19FiveDaysAfter),]%>%
  filter(FuelCode=="P98")%>% 
  select(date,Brand, Postcode, Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character(date), format="%Y-%m-%d")-
           as.Date(as.character("2020-01-06"), format="%Y-%m-%d"))%>%
  mutate (period="After")

## Avg daily fuel price of 5 days after will be different from 5 days before
Avg_daily_fuel_price <- XMAS19FiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

XMAS19FiveDaysAfterData<- left_join(XMAS19FiveDaysAfterData,Avg_daily_fuel_price,by="date")

## Combine Before and after
XMAS19Before_After_Holiday <-rbind(XMAS19FiveDaysAfterData, XMAS19FiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")

XMAS19Before_After_Holiday$peri = factor(XMAX19Before_After_Holiday$period, levels=c("Before","After"))


# Create chart for 2019 XMAS 
### lead in lead out 2019 XMAS school holiday, chart 2019XMASFivedayLessThan40Brands

x_40 <- XMAS19Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after school holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~peri,ncol=1)+
  scale_x_continuous(breaks = seq(1, 10, by = 1))+
  theme(legend.position="bottom")


#chart for group_2, brand with less than 100 stations, chart 2019MASFivedayLessThan100Brands

x_100 <- XMAS19Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after school holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)+
  scale_x_continuous(breaks = seq(1, 10, by = 1))



#chart for group_3, brand with less than 200 stations,chart 2019XMASFivedayLessThan200Brands

x_200 <- XMAS19Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)+
  scale_x_continuous(breaks = seq(1, 10, by = 1))



#chart for group_4, brand with less than 400 stations

x_400 <- XMAS19Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("# days before/after holiday")+
  ylab("Average Fuel Price $cent")+
  ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)+
  scale_x_continuous(breaks = seq(1, 10, by = 1))






# average fuel price by fuel type by postcode -------

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


# average fuel price by fuel type by suburb -------
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




# Fuel Station Postcode vs SA2 -----

# not all addresses can be geocoded. Convert street address to lat and lon from Google Cloud Platform "Geocoding API", require payment detail to get API key. Max 2500 addresses per day.
address_geocoded <- read.csv("Postcode_SA2/address_geocoded.csv")

# one postcode might have 1+ suburbs and 1+ SA2 codes. eg. postcode 2019, here randomly choose one, need to decide if we are using this
SA2<-read.csv("Postcode_SA2/2019 Locality to 2016 SA2 Coding Index.csv")%>%
  select(SA2_MAINCODE,POSTCODE,STATE)
SA2<-SA2[!duplicated(SA2$POSTCODE), ]
names(SA2)[2] <- "Postcode"
SA2$Postcode<- as.character(SA2$Postcode)
str(SA2)


# Table for average fuel price on each types by stations, append lat and lon and SA2 code 
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

# Import Shapefile -----

SA2<- readOGR("Postcode_SA2\\1270055001_sa2_2016_aust_shape\\SA2_2016_AUST.shp")  
SA2_NSW<- SA2[SA2$STE_NAME16=="New South Wales", ]  
#plot(SA2_NSW,main="New South Wales")

# Plot Shapefile -----
##Yixin's code - start
register_google(key = "AIzaSyB1Fccl9sZOEUWC0NISGPdW5pclAp6vyo0")

shapefile2<- SA2_NSW 

stationlist<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,ServiceStationName,FuelCode))%>%
  unique()%>%
  filter(FuelCode=="P98")

## Plot stations in NSW
latlon<-mutate_geocode(stationlist, Address)

write.csv(latlon, "EDA/station_latlon.csv") # this is all lat and lon for stations sold P98

latlon <- read.csv(here("EDA","station_latlon.csv"))

leaflet(latlon) %>% 
  addTiles() %>%
  addMarkers(~lon, ~lat,popup=~ServiceStationName,clusterOptions = markerClusterOptions())%>%
  addPolygons(data = shapefile2) ##default map= openstreet map

##Yixin's code - End

## Heat map on station count in NSW

## Heat map on average P98 price in NSW

## Heat map on house median price in NSW


## How to plot SA2_NSW with fuel_station_Aggr_P98???????

#Plots for the assignment T4 vs T1 ----

figure1 <- ggarrange(T4_40, T1_40,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure2 <- ggarrange(T4_100, T1_100,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure3 <- ggarrange(T4_200, T1_200,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure4 <- ggarrange(T4_400, T1_400,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure1
figure2
figure3
figure4

# #Plots for the assignment bank holidays ----

figure5 <- ggarrange(QB_40, AD_40,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure6 <- ggarrange(QB_100, AD_100,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure7 <- ggarrange(QB_200, AD_200,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure8 <- ggarrange(QB_400, AD_400,
                     labels = c("A", "B"),
                     ncol = 2,
                     common.legend = TRUE,
                     legend="bottom")

figure5
figure6
figure7
figure8

figure9 <- ggarrange(x_100, x_200, x_400,
                     labels = c("A", "B", "C"),
                     nrow = 1,
                     common.legend = FALSE,
                     legend="bottom")

figure9
