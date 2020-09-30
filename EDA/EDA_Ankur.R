# Import libraries --------

library(psych) 
library(tidyverse)
library(lubridate)
library(readxl)
library(knitr)
library(data.table)
library(dplyr)
library(janitor)
library(here)
library(scales)
library(dplyr)
library(ggplot2)

# Import data --------

fuel_all <- read.csv(here("EDA","fuel_all.csv"))

House_data <- read.csv(here("EDA","Combined_HousePriceAug19ToJul20.csv"))

#Data wrangling --------

#Renameing The 'Postal.Code' column name ?
average_house_price <- rename(House_data,  Postcode = Postal.Code)

# Create a datafram to group/bin the stations based on count --------

brand_station_ct <- fuel_all %>%
  group_by(Brand)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))

# Building dataframes for analysis (P98, DL and LPG) --------

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
