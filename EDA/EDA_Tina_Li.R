# load packages ---------
library(tidyverse)
library(dplyr)
library(here)

# Reference List
# https://www.greenslips.com.au/fuel/types.html

# Load data
fuel_all <- read_csv(here("EDA","fuel_all.csv"))


# let's inspect P98 price of the 5 days prior and after Queen's birthday 2020-06-01 by different brand
# create subsets based on date
FiveDaysPrior<- format(seq(as.Date("2020-05-31"), length.out=5, by="-1 day"), format="%Y-%m-%d")
FiveDaysAfter<- format(seq(as.Date("2020-06-02"), length.out=5, by="1 day"), format="%Y-%m-%d")

## FiveDatePriorData
FiveDatePriorData<-fuel_all%>% 
  filter(date==FiveDaysPrior& FuelCode=="P98")%>%
  select(date,Brand,Postcode,Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))

FiveDatePriorData$day_number <- as.Date(as.character("2020-06-01"), format="%Y-%m-%d")-as.Date(as.character(FiveDatePriorData$date), format="%Y-%m-%d")

Avg_daily_fuel_price <- FiveDatePriorData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

FiveDatePriorData<- left_join(FiveDatePriorData,Avg_daily_fuel_price,by="date")%>%
  mutate (period="Before")

## FiveDateAfterData    

FiveDaysAfterData<-fuel_all%>% 
  filter(date==FiveDaysAfter& FuelCode=="P98")%>%
  select(date,Brand, Postcode, Price) %>%
  group_by(date,Brand)%>%
  summarise(brand_daily_avg = mean(Price))

Avg_daily_fuel_price <- FiveDaysAfterData%>% 
  group_by(date)%>%
  summarise(daily_avg = mean(brand_daily_avg))

FiveDaysAfterData<- left_join(FiveDaysAfterData,Avg_daily_fuel_price,by="date")%>%
  mutate (period="After")
FiveDaysAfterData$day_number <- as.Date(as.character(FiveDaysAfterData$date), format="%Y-%m-%d")-
  as.Date(as.character("2020-06-01"), format="%Y-%m-%d")

Before_After_Holiday <-rbind(FiveDaysAfterData, FiveDatePriorData)
names(Before_After_Holiday)

# Create charts

# Bug to fix in below chart - how to remove duplicate label "Average Daily Fuel Price" and adjust position on top of red line?
Before_After_Holiday %>%
  ggplot(aes(x = day_number, y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  geom_line(aes(y=daily_avg),color = "red",size=2,stat = "identity")+
  geom_text(aes(day_number[5], daily_avg, label = "Average Daily Fuel Price"),vjust= -3,check_overlap = TRUE, colour = "red", stat = "identity")+
  xlab("Day number to holiday")+
  ylab("Average Fuel Price")+
  ggtitle("Average P98 Price by Brand 5 days Before and After Queen's Birthday June 1st 20")+
  labs(color="Brands")+
  facet_wrap(~period,ncol=1)

##There's larger price variance by brands after holiday compare to before holiday, it will be more expensive to fuel up P98 5 days after Queen's birthday.
