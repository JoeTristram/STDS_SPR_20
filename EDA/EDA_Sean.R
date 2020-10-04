#Load libraries----
library(tidyverse)
library(lubridate)

# read in data ---------

fuel_all <- read_csv(here("Fuel_2019-2020","fuel_all.csv"))

fuel_all <- fuel_all %>%
  filter(date >= "2019-08-01")

house_all <- read_csv(here("House_Price_Data", "house_all.csv"))


## aggregative fuel price start

fuel_station_Aggr <- fuel_all %>%
  select(Address,Brand, Mth, ServiceStationName,Postcode, Price,FuelCode, SA2) %>%
  group_by(Address,Brand, Mth, ServiceStationName,Postcode,FuelCode, SA2) %>%
  summarise(pc_avg_fuel_price=mean(Price)) %>%
  spread(FuelCode,pc_avg_fuel_price)


## aggregative fuel price start

house_price_aggr <- house_all %>%
  select(month, Postcode, Purchase.Price, SA2) %>%
  group_by(month, Postcode, SA2) %>%
  summarise(pc_med_house_price = median(Purchase.Price))


#Combine files ---------

house_fuel_all_month <- left_join(fuel_station_Aggr, house_price_aggr, by = c("Mth"="month", "Postcode", "SA2"))


#Modelling----

Fuel_type_P98 <- house_fuel_all_month %>%
  select(-DL, -LPG) %>%
  na.omit()

Fuel_type_DL <- house_fuel_all_month %>%
  select(-P98, -LPG) %>%
  na.omit()

Fuel_type_LPG <- house_fuel_all_month %>%
  select(-P98, -DL) %>%
  na.omit()

#Model for P98


lm1 <- lm(P98 ~ pc_med_house_price, data = Fuel_type_P98)
summary(lm1)
plot(lm1)

# build in GGPLOT
hist(Fuel_type_P98$pc_med_house_price, breaks=40) 


#New dataset for DL

lm2 <- lm(DL ~ pc_med_house_price, data = Fuel_type_DL)
summary(lm2)
plot(lm2)


#New dataset for LPG

lm3 <- lm(LPG ~ pc_med_house_price, data = Fuel_type_LPG)
summary(lm3)
plot(lm3)

