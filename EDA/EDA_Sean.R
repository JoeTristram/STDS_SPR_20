#Load libraries----
library(tidyverse)
library(lubridate)

#Working----

glimpse(com_House_fuel)


#Modelling----

Fuel_type_P98<-com_House_fuel%>%
  filter(FuelCode=="P98")
glimpse(Fuel_type_P98)
glimpse(Combined_HousePriceFebToJul20)
House_1 <- Combined_HousePriceFebToJul20 %>%
  mutate(Settlement_date = dmy(`Settlement Date`))
glimpse(House_1)
Average_House <- House_1 %>%
  group_by(Postcode, Settlement_date) %>%
  mutate(avg_house_price=mean(`Purchase Price`, na.rm = TRUE))


#Create monthly average house price per postcode

House_1 <- Combined_HousePriceFebToJul20 %>%
  select(`Settlement Date`, `Purchase Price`, Postcode)%>%
  mutate(Settlement_date = dmy(`Settlement Date`),
         Settlement_month = month(Settlement_date))
View(House_1)
House_2 <- House_1%>%
  group_by(Postcode, Settlement_month) %>%
  summarise(avg_house_price=mean(`Purchase Price`, na.rm = TRUE), n=n()) %>%
  ungroup()
glimpse(fuel_all)
fuel_1 <- fuel_all%>%
  mutate(Price_month=month(date))
fuel_2 <- left_join(fuel_1, House_2, by = c("Price_month"="Settlement_month",
                                            "Postcode" = "Postcode"))

#New dataset for P98

fuel_2_P98 <- fuel_2 %>%
  filter(FuelCode == "P98")
glimpse(fuel_2_P98)
lm1 <- lm(Price ~ avg_house_price, data = fuel_2_P98)
summary(lm1)
plot(lm1)
hist(fuel_2_P98$avg_house_price, breaks=40)


#New dataset for DL

fuel_2_DL <- fuel_2 %>%
  filter(FuelCode == "DL")
glimpse(fuel_2_DL)
lm2 <- lm(Price ~ avg_house_price, data = fuel_2_DL)
summary(lm2)
plot(lm2)


#New dataset for LPG

fuel_2_LPG <- fuel_2 %>%
  filter(FuelCode == "LPG")
glimpse(fuel_2_LPG)
lm3 <- lm(Price ~ avg_house_price, data = fuel_2_LPG)
Summary(lm3)
summary(lm3)
plot(lm3)