library(tidyverse)
library(dplyr)
library(readxl)
library(plyr)
library(lubridate)

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
## end

## reformat data type start
#fuel_all$PriceUpdatedDate<- dmy_hms(fuel_all$PriceUpdatedDate)

fuel_all$date<-ymd(fuel_all$date)

fuel_all$Price<-as.numeric(fuel_all$Price)

fuel_all$Postcode<- as.character(fuel_all$Postcode)
## end 
