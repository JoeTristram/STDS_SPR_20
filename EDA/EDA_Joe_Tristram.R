# load packages ---------
library(tidyverse)
library(skimr)
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

fuel_type_list <- as.list(levels(factor(fuel_all$FuelCode)))
postcode_list <- as.list(levels(factor(fuel_all$Postcode)))
station_list <- as.list(levels(factor(fuel_all$Brand)))

#Fuel type lists
standard_unleaded <- list("E10", "U91")
premium_unleaded <- list("P95", "P98")
unleaded <- append(standard_unleaded, premium_unleaded)
standard_diesel <-list("DL", "B20")
premium_diesel <- list("PDL")
diesel <- append(standard_diesel, premium_diesel)
gas <- list("LPG", "CNG")

# write cleaned data to .csv ---------

## Will come back to this

# sub-setting data; sorting and filtering ---------

average_fuel_price <- fuel_all %>%
  filter(FuelCode == standard_diesel) %>%
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

# Time series analysis ---------

time_date <- fuel_all %>%
  select(Brand, Postcode, FuelCode, Price, date) %>%
  mutate(Day = Day_of_the_week <- weekdays(fuel_all$date)) %>%
  filter(FuelCode == unleaded) %>%
  group_by(Postcode, FuelCode, Day) %>%
  summarise(avg_price = mean(Price))

time_date %>%
  ggplot(aes(x = Day, y = avg_price)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

# How is the data distrbuted and how do we visualise on a map ---------

Summary_percentage <- fuel_all %>%
  select(Brand) %>%
  group_by(Brand) %>%
  mutate(number_stations = number <- 1) %>%
  summarise(number_stations = sum(number_stations)) %>%
  mutate(percentage = number_stations / 154980 * 100) %>%
  arrange(desc(number_stations))

fuel_all %>%
  count(FuelCode == "E85")
