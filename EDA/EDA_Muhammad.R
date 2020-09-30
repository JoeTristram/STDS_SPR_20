#install.packages("psych") 
#install.packages("scales")

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

# Building dataframe for House price data --------
#Count the Postcode data to understand its limitations

average_house_price_count <- average_house_price %>%
  group_by(Postcode)%>%
  summarise(postcode_count = n_distinct(Purchase.Price))

# Dataset we have grouped by

average_house_price_grouped <- average_house_price %>%
  select(Postcode, Purchase.Price) %>%
  group_by(Postcode) %>%
  summarise(min_house_price = min(Purchase.Price),
            max_house_price = max(Purchase.Price),
            avg_house_price = mean(Purchase.Price), 
            median_house_price = median(Purchase.Price))

#fuel type 

#combing

com_House_fuel_P98          <- left_join(average_fuel_price_P98, average_house_price_grouped, by = "Postcode")
com_House_fuel_DL           <- left_join(average_fuel_price_DL, average_house_price_grouped, by = "Postcode")
com_House_fuel_LPG          <- left_join(average_fuel_price_LPG, average_house_price_grouped, by = "Postcode")
com_House_fuel_station_P98  <- left_join(com_House_fuel_P98, brand_station_ct, by = "Brand")
com_House_fuel_station_DL   <- left_join(com_House_fuel_DL, brand_station_ct, by = "Brand")
com_House_fuel_station_LPG  <- left_join(com_House_fuel_LPG, brand_station_ct, by = "Brand")


###Ploting Graphs --------

# * Average house price--------
# ** P98 ----

com_House_fuel_station_P98 %>%
  ggplot(aes(x = avg_house_price, y = avg_fuel_price, colour = station_group ) ) +
  geom_point() +
  ggtitle("Average House Price Vs Average Fuel Price", subtitle = "(Fuel Type: P98)") +
  labs(y="Average Fuel Price (Cents)", x = "Average House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for P98 - Average House price -----

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_P98)

# *** GLM model for P98 - Average House price -----

glm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_P98, family="gaussian")

# ** DL ----
com_House_fuel_station_DL %>%
  ggplot(aes(x = avg_house_price, y = avg_fuel_price, colour = station_group ) ) +
  geom_point() +
  ggtitle("Average House Price Vs Average Fuel Price", subtitle = "(Fuel Type: DL)") +
  labs(y="Average Fuel Price (Cents)", x = "Average House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for DL - Average House price -----
lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_DL)

# *** GLM model for DL - Average House price -----

glm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_DL, family="gaussian")

# ** LPG ----
com_House_fuel_station_LPG %>%
  ggplot(aes(x = avg_house_price, y = avg_fuel_price, colour = station_group ) ) +
  geom_point() +
  ggtitle("Average House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price (Cents)", x = "Average House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for LPG - Average House price -----

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_LPG)


# * Median house house -------- 

# ** P98 ----

com_House_fuel_station_P98 %>%
  ggplot(aes(x = median_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Median House Price Vs Average Fuel Price", subtitle = "(Fuel Type: P98)") +
  labs(y="Average Fuel Price (Cents)", x = "Median House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)
#+ 
#  facet_wrap(~station_group)

# *** Regression model for P98 - Median House price -------

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_P98)


# ** DL ----
com_House_fuel_station_DL %>%
  ggplot(aes(x = median_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Median House Price Vs Average Fuel Price", subtitle = "(Fuel Type: DL)") +
  labs(y="Average Fuel Price (Cents)", x = "Median House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for DL - Median House price -------

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_DL)



# ** LPG -----
com_House_fuel_station_LPG %>%
  ggplot(aes(x = median_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Median House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price (Cents)", x = "Median House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for LPG - Median House price -------

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_LPG)


# * Minimum house price --------
# ** P98 ----
com_House_fuel_station_P98 %>%
  ggplot(aes(x = min_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Minmum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: P98)") +
  labs(y="Average Fuel Price", x = "Minmum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for P98 - Median House price -----

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_P98)

# ** DL ----
com_House_fuel_station_DL %>%
  ggplot(aes(x = min_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Minmum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: DL)") +
  labs(y="Average Fuel Price", x = "Minmum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for DL - Median House price -----

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_DL)


# ** LPG -----
com_House_fuel_station_LPG %>%
  ggplot(aes(x = min_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Minmum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price", x = "Minmum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for LPG - Median House price -----

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_LPG)

# * Maximum house price --------

# ** P98 ----
com_House_fuel_station_P98 %>%
  ggplot(aes(x = max_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Maximum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: P98)") +
  labs(y="Average Fuel Price", x = "Maximum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# Regression model for P98 - Median House price

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_P98)

# ** DL ----
com_House_fuel_station_DL %>%
  ggplot(aes(x = max_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Maximum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: DL)") +
  labs(y="Average Fuel Price", x = "Maximum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# Regression model for DL - Median House price

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_DL)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_DL)


# ** LPG -----
com_House_fuel_station_LPG %>%
  ggplot(aes(x = max_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Maximum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price", x = "Maximum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# Regression model for LPG - Median House price

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_LPG)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_LPG)
