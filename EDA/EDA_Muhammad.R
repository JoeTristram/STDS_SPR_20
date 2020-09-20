install.packages("psych") 
install.packages("scales")

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




fuel_all <- read.csv(here("Fuel_2019-2020","fuel_all.csv"))


House_data <- read.csv(here("House","Combined_HousePriceFebToJul20.CSV"))

brand_station_ct <- fuel_all %>%
  group_by(Brand)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))


average_fuel_price <- fuel_all %>%
  filter(FuelCode == "LPG") %>%
  select(Postcode, Brand, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_fuel_price = mean(Price))
#Renameing The column name ?
average_house_price <- rename(House_data,  Postcode = Postal.Code)

average_house_price <- average_house_price %>%
  filter(Primary.Propose == "RESIDENCE") %>%
  select(Postcode, Purchase.Price) %>%
  group_by(Postcode) %>%
  summarise(min_house_price = min(Purchase.Price),
            max_house_price = max(Purchase.Price),
            avg_house_price = mean(Purchase.Price), 
            median_house_price = median(Purchase.Price))

#fuel type 

#combing

com_House_fuel<- left_join(average_fuel_price, average_house_price, by = "Postcode")

com_House_fuel_station <- left_join(com_House_fuel, brand_station_ct, by = "Brand")


###Ploting

#Average
#LPG

com_House_fuel_station %>%
  ggplot(aes(x = avg_house_price, y = avg_fuel_price, colour = station_group ) ) +
  geom_point() +
  ggtitle("Average House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price (Cents)", x = "Average House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#E10
com_House_fuel_station %>%
  ggplot(aes(x = avg_house_price, y = avg_fuel_price, colour = station_group ) ) +
  geom_point() +
  ggtitle("Average House Price Vs Average Fuel Price", subtitle = "(Fuel Type: E10)") +
  labs(y="Average Fuel Price (Cents)", x = "Average House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#median 

#LPG

com_House_fuel_station %>%
  ggplot(aes(x = median_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Median House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price (Cents)", x = "Median House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) 
# + facet_wrap(~station_group))

#E10
com_House_fuel_station %>%
  ggplot(aes(x = median_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Median House Price Vs Average Fuel Price", subtitle = "(Fuel Type: E10)") +
  labs(y="Average Fuel Price (Cents)", x = "Median House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))


#MINMUM
#LPG
com_House_fuel_station %>%
  ggplot(aes(x = min_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Minmum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price", x = "Minmum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))
#E10

com_House_fuel_station %>%
  ggplot(aes(x = min_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Minmum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: E10)") +
  labs(y="Average Fuel Price", x = "Minmum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#Maximum

#LPG
com_House_fuel_station %>%
  ggplot(aes(x = max_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Maximum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price", x = "Maximum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

#E10
com_House_fuel_station %>%
  ggplot(aes(x = max_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Maximum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: E10)") +
  labs(y="Average Fuel Price", x = "Maximum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6))

