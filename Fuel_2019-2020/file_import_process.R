library(psych) 
library(tidyverse)
library(lubridate)
library(readxl)
library(knitr)
library(data.table)
library(janitor)
library(here)
library(scales)
library(dplyr)
library(plyr)
library(ggplot2)

fuel_files <- list.files(path = "./Fuel_2019-2020/",
                    pattern='*.xlsx',
                    full.names = T)

fuel_data <- ldply(fuel_files, read_excel, sheet = 1, skip = 2, col_names = TRUE) %>%
  fill(everything(), .direction = "down")

holiday_files <- list.files(path = "./Public_Holiday_2019-2020/",
                    pattern='*.csv',
                    full.names = T)

fuel_data$PriceUpdatedDate <- dmy_hms(fuel_data$PriceUpdatedDate)
fuel_data$date <- as.Date(format(fuel_data$PriceUpdatedDate, "%Y-%m-%d"))


## combine .csv start
NSWPublicHoliday <- holiday_files %>%
  map_dfr(~read.csv(.x))%>%
  filter(Jurisdiction == "nsw")
  
colnames(NSWPublicHoliday)[2]<-c("date")


NSWPublicHoliday$date<- ymd(NSWPublicHoliday$date)

fuel_all <- left_join(fuel_data, NSWPublicHoliday, by = "date")

## combine .csv end

## code to reformat fuel_all - start
fuel_all <- fuel_all%>% 
  mutate(isholiday=ifelse(!is.na(Holiday.Name),"IsHoliday","NotHoliday"),
         date=ymd(date),
         fuel_month = format(as.Date(date), "%Y-%m"),
         Mth=month(date),
         Day = Day_of_the_week <- weekdays(date))%>%
  filter(FuelCode=="P98"|FuelCode=="DL"|FuelCode=="LPG")

#  filter(fuel_month >= "2019-08", fuel_month <="2020-07")
## end

## reformat data type start
#fuel_all$PriceUpdatedDate<- dmy_hms(fuel_all$PriceUpdatedDate)

fuel_all$date<-ymd(fuel_all$date)

fuel_all$Price<-as.numeric(fuel_all$Price)

fuel_all$Postcode<- as.character(fuel_all$Postcode)
## end 

detach(package:plyr)

brand_station_ct <- fuel_all %>%
  group_by(Brand) %>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))

fuel_all <- left_join(fuel_all, brand_station_ct, by = "Brand")

address_station_ct <- fuel_all %>%
  select(Address) %>%
  unique()

write.csv(address_station_ct, "Postcode_SA2/station_unique.csv")

address_sa2_lat_long <- read.csv(here("Postcode_SA2","address_geocoded.csv"))

fuel_all <- left_join(fuel_all, address_sa2_lat_long, by = "Address")

write.csv(fuel_all, 'Fuel_2019-2020//fuel_all.csv')

# House price data --------

House_data <- read.csv(here("EDA","Combined_HousePriceAug19ToJul20.csv"))
house_sa2_lat_long <- read.csv(here("Postcode_SA2","houselatlon.csv"))

house_all <- House_data %>%
  select(Property.ID,Property.Unit.Number,Property.House.Number,
         Property.Street.Name,Property.Suburb,Postal.Code,
         Purchase.Price,Settlement.Date)%>%
  mutate(fulladdress = paste(Property.House.Number,
                             Property.Street.Name,Property.Suburb,Postal.Code, "AU",sep=","),
         Settlement_date = dmy(Settlement.Date),
         Settlement_month = format(as.Date(Settlement_date), "%Y-%m"),
         moth= month(Settlement_date),
         Day = Day_of_the_week <- weekdays(Settlement_date))%>%
  rename(date=Settlement.Date,Postcode = Postal.Code)

house_all <- left_join(house_all, house_sa2_lat_long, by = "fulladdress")

write.csv(house_all, 'House_Price_Data//house_all.csv')

house_all %>%
  ggplot(aes(Purchase.Price))+
  geom_histogram(breaks=seq(100, 250000, by=50))

