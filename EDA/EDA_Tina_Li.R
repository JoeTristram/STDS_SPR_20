# load packages ---------
library(tidyverse)
library(dplyr)
library(here)


########################################################
#Import 6 month's fuel data
library(tidyverse)
library(lubridate)
library(readxl)
library(here)

Fuel_Jul20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_july2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")
Fuel_Jun20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_june2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")
Fuel_May20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_may2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")
Fuel_Apr20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_apr2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")
Fuel_Mar20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_mar2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")
Fuel_Feb20<-as.data.frame(read_xlsx("Fuel_2019-2020\\Fuel_price_history_checks_feb2020.xlsx",1,skip=2, col_names = TRUE))%>% fill(everything(), .direction = "down")

NSWPublicHoliday<- read.csv("Public_Holiday_2019-2020\\australian_public_holidays_2020.csv")%>% filter(Jurisdiction=="nsw")

# format date to date and rename column
NSWPublicHoliday$Date<- ymd(NSWPublicHoliday$Date)
NSWPublicHoliday<- NSWPublicHoliday%>% rename(date=Date)

## Combine Fuel data Feb to Jul 20
Fuel_RAW<- rbind(Fuel_Feb20,Fuel_Mar20,Fuel_Apr20,Fuel_May20,Fuel_Jun20,Fuel_Jul20)
Fuel_RAW$PriceUpdatedDate<- dmy_hms(Fuel_RAW$PriceUpdatedDate)
Fuel_RAW$date <- as.Date(format(Fuel_RAW$PriceUpdatedDate, "%Y-%m-%d"))

## merge Fuel_RAW with public holiday
Fuel_All<- left_join(Fuel_RAW,NSWPublicHoliday,by="date")
write.csv(Fuel_All, "EDA/fuel_all.csv")

####################################################

# Load data
fuel_all <- read_csv(here("EDA","fuel_all.csv"))


brand_station_ct <- fuel_all %>% 
  group_by(Brand)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))



# let's inspect P98 price of the 5 days prior and after Queen's birthday 2020-06-01 by different brand
# set variable for subseting data
#HolidayToInspect <- as.Date("2020-06-01")
#FuelCodeToInspect<-"P98"

FiveDaysPrior<- format(seq(as.Date("2020-05-31"), length.out=5, by="-1 day"), format="%Y-%m-%d")
FiveDaysAfter<- format(seq(as.Date("2020-06-02"), length.out=5, by="1 day"), format="%Y-%m-%d")

# create subsets based on date
## FiveDatePriorData
FiveDatePriorData<-fuel_all%>% 
  filter(date==FiveDaysPrior& FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))%>%
  mutate(day_number = as.Date(as.character("2020-06-01"), format="%Y-%m-%d")-as.Date(as.character(date), format="%Y-%m-%d"))%>%
  mutate (period="Before")

Avg_daily_fuel_price <- FiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

FiveDatePriorData<- left_join(FiveDatePriorData,Avg_daily_fuel_price,by="date")

## FiveDateAfterData    

FiveDaysAfterData<-fuel_all%>% 
  filter(date==FiveDaysAfter& FuelCode=="P98")%>%
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


# creata subset of 5 days before and after
Before_After_Holiday <-rbind(FiveDaysAfterData, FiveDatePriorData)%>%
  left_join(brand_station_ct,by="Brand")



# Create charts

# Bug to fix in below chart - how to remove duplicate label "Average Daily Fuel Price" and adjust position on top of red line?
# ggtitle("Average P98 Price Comparison by Brand with less than 100 stations", subtitle = "5 days Before and After Queen's Birthday June 1st 20")
# geom_text(aes(day_number[5], daily_avg, label = "Average Daily Fuel Price"),vjust= -3,check_overlap = TRUE, colour = "red", stat = "identity")+

#chart for group_1, brand with less than 40 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("Day number to holiday")+
  ylab("Average Fuel Price")+
  ggtitle("Brand with less than 40 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)
#chart for group_2, brand with less than 100 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_2")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("Day number to holiday")+
  ylab("Average Fuel Price")+
  ggtitle("Brand with less than 100 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)

#chart for group_1, brand with less than 200 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_3")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("Day number to holiday")+
  ylab("Average Fuel Price")+
  ggtitle("Brand with less than 200 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)  

#chart for group_1, brand with less than 400 stations
Before_After_Holiday %>% 
  filter(station_group=="Group_4")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  xlab("Day number to holiday")+
  ylab("Average Fuel Price")+
  ggtitle("Brand with less than 400 stations")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1) 



## average fuel price by postcode
Postcode_fuel_all<- fuel_all%>% 
  select(Postcode,Price) %>%
  mutate(overall_fuel_avg=mean(Price))%>%
  group_by(Postcode,overall_fuel_avg)%>%
  summarise(postcode_fuel_avg = mean(Price))%>% 
  arrange(postcode_fuel_avg)

Postcode_fuel_all$Postcode<- as.factor(Postcode_fuel_all$Postcode)
head(Postcode_fuel_all)



### charts- by Fuel price by postcode on average fuel price over all types and all times
Postcode_fuel_all%>%tail(5)%>%
  ggplot(aes(x = Postcode, y = postcode_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(postcode_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Postcode_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Post Code")+
  ylab("Average Fuel Price $cent") +
  ggtitle("Most expensive average fuel price by Postcode",subtitle = "Across all fuel types betw Feb 20 to Jul 20")

Postcode_fuel_all%>%head(5)%>%
  ggplot(aes(x = Postcode, y = postcode_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(postcode_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Postcode_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Post Code")+
  ylab("Average Fuel Price $cent") +
  ggtitle("Least expensive average fuel price by Postcode",subtitle = "Across all fuel types betw Feb 20 to Jul 20")


## by Fuel price by Suburb on average fuel price over all types and all times
Suburb_fuel_all<- fuel_all%>% 
  select(Suburb,Price) %>%
  mutate(overall_fuel_avg=mean(Price))%>%
  group_by(Suburb,overall_fuel_avg)%>%
  summarise(suburb_fuel_avg = mean(Price))%>% 
  arrange(suburb_fuel_avg)
head(Suburb_fuel_all)


Suburb_fuel_all%>%tail(5)%>%
  ggplot(aes(x = Suburb, y = suburb_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(suburb_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Suburb_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Suburb")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylab("Average Fuel Price $cent") +
  ggtitle("Most expensive average fuel price by Suburb",subtitle = "Across all fuel types betw Feb 20 to Jul 20")

Suburb_fuel_all%>%head(5)%>%
  ggplot(aes(x = Suburb, y = suburb_fuel_avg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=round(suburb_fuel_avg, digits = 0)), position=position_dodge(width=0.9), vjust=-0.25,digits = 0)+
  geom_line() +
  geom_hline(yintercept = Suburb_fuel_all$overall_fuel_avg, linetype="dashed", 
             color = "red", size=2)+
  xlab("Suburb")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ylab("Average Fuel Price $cent") +
  ggtitle("Least expensive average fuel price by Suburb",subtitle = "Across all fuel types betw Feb 20 to Jul 20")

##
