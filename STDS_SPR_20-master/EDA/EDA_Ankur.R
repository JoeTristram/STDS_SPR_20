

# Import libraries --------
library(psych)
library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(ggplot2)

fuel_all <- read_csv(here("Fuel_2019-2020","fuel_all.csv"))
fuel_all <- fuel_all %>%filter(date >= "2019-08-01")
house_all <- read_csv(here("House_Price_Data", "house_all.csv"))
brand_station_ct <- fuel_all %>%
  group_by(Brand)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))
# Building dataframes --------
average_fuel_price_P98 <- fuel_all %>%
  filter(FuelCode == "P98") %>%
  select(Postcode, Brand, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_fuel_price = mean(Price))
average_fuel_price_DL <- fuel_all %>%
  filter(FuelCode == "DL") %>%
  select(Postcode, Brand, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_fuel_price = mean(Price))
average_fuel_price_LPG <- fuel_all %>%
  filter(FuelCode == "LPG") %>%
  select(Postcode, Brand, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_fuel_price = mean(Price))
#  grouped Dataset
average_house_price_grouped <- house_all %>%
  select(Postcode, Purchase.Price) %>%
  group_by(Postcode) %>%
  summarise(min_house_price = min(Purchase.Price),
            max_house_price = max(Purchase.Price),
            avg_house_price = mean(Purchase.Price),
            median_house_price = median(Purchase.Price))
#fuel type
com_House_fuel_P98  <- left_join(average_fuel_price_P98, average_house_price_grouped, by = "Postcode")
com_House_fuel_DL <- left_join(average_fuel_price_DL, average_house_price_grouped, by = "Postcode")
com_House_fuel_LPG <- left_join(average_fuel_price_LPG, average_house_price_grouped, by = "Postcode")
com_House_fuel_station_P98<- left_join(com_House_fuel_P98, brand_station_ct, by = "Brand")
com_House_fuel_station_DL  <- left_join(com_House_fuel_DL, brand_station_ct, by = "Brand")
com_House_fuel_station_LPG  <- left_join(com_House_fuel_LPG, brand_station_ct, by = "Brand")
com_House_fuel_station_P98 %>%
  ggplot(aes(x = avg_house_price, y = avg_fuel_price) ) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)


lm(avg_fuel_price ~ avg_house_price, com_House_fuel_station_P98)
lm(avg_fuel_price ~ avg_house_price, com_House_fuel_station_DL)
lm(avg_fuel_price ~ avg_house_price, com_House_fuel_station_LPG)

