# load packages ---------
library(tidyverse)
library(here)

# read in data --------

fuel_all <- read_csv(here("EDA","fuel_all.csv"))

# exploring the data --------

View(fuel_all)
dim(fuel_all)
str(fuel_all)
glimpse(fuel_all) #better to use in console relative to 'str' above
head(fuel_all)
tail(fuel_all)
summary(fuel_all) #get an idea of the range of each of the variables
skim(fuel_all)

# tidying columns ---------

## Will come back to this, however Mobil 1 and Mobil appear to be the same.

# new variables for EDA ---------

fuel_type_list <- levels(factor(fuel_all$FuelCode))
postcode_list <- levels(factor(fuel_all$Postcode))
station_list <- levels(factor(fuel_all$Brand))

#Fuel type lists
standard_unleaded <- list("E10", "U91", "E85")
premium_unleaded <- list("P95", "P98")
unleaded <- list(standard_unleaded, premium_unleaded)
standard_diesel <-list("DL", "B20")
premium_diesel <- list("PDL")
diesel <- list(standard_diesel, premium_diesel)
gas <- list("LPG", "CNG")

# write cleaned data to .csv ---------

## Will come back to this

# sub-setting data; sorting and filtering ---------

average_fuel_price <- fuel_all %>%
  filter(Brand != "Costco", Brand != "NRMA", FuelCode != "E85") %>%
  filter(FuelCode == premium_unleaded) %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

average_fuel_price %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

# test area for playing around with the dataset ---------

wrangling <- fuel_all %>%
  filter(FuelCode == "E85") %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

wrangling %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)
