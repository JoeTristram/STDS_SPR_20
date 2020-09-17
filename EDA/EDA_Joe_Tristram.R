# load packages ---------
library(tidyverse)
library(here)

# read in data --------

fuel_all <- read_csv(here("EDA","fuel_all.csv"))

fuel_type_list <- levels(factor(fuel_all$FuelCode))
postcode_list <- levels(factor(fuel_all$Postcode))
station_list <- levels(factor(fuel_all$Brand))

average_fuel_price <- fuel_all %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

average_fuel_price %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)
