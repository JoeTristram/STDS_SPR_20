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
library(plyr)


# Import data --------

fuel_all <- read_csv(here("Fuel_2019-2020","fuel_all.csv"))
fuel_all<-filter(fuel_all, fuel_all$date >= '2019-08-01	' & fuel_all$date <= '2020-07-31	')
house_all <- read_csv(here("House_Price_Data", "house_all.csv"))


#Data wrangling --------
## aggregative fuel price start

fuel_station_Aggr <- fuel_all %>%
  select(Address,Brand, Mth, ServiceStationName,Postcode, Price,FuelCode, SA2) %>%
  group_by(Address,Brand, Mth, ServiceStationName,Postcode,FuelCode, SA2) %>%
  summarise(pc_avg_fuel_price=mean(Price)) %>%
  spread(FuelCode,pc_avg_fuel_price)


## aggregative fuel price start

house_price_aggr <- house_all %>%
  select(moth, Postcode, Purchase.Price, SA2) %>%
  group_by(moth, Postcode, SA2) %>%
  summarise(pc_med_house_price = median(Purchase.Price))


#Combine files ---------

house_fuel_all_month <- left_join(fuel_station_Aggr, house_price_aggr, by = c("Mth"="moth", "Postcode", "SA2"))

#Renameing The 'Month' column name ?

house_fuel_all_month <- rename(house_fuel_all_month,  month = Mth)

#Renameing The 'Postal.Code' column name ?
#average_house_price <- rename(House_data,  Postcode = Postal.Code)

# Create a datafram to group/bin the stations based on count --------

brand_station_ct <- house_fuel_all_month %>%
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

average_house_price_grouped <- house_all %>%
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
  geom_jitter() +
  ggtitle("Average House Price Vs Average Fuel Price", subtitle = "(Fuel Type: P98)") +
  labs(y="Average Fuel Price (Cents)", x = "Average House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)

# *** Regression model for P98 - Average House price -----

lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ Brand + 0, com_House_fuel_station_P98)
lm(avg_fuel_price ~ station_group + 0, com_House_fuel_station_P98)




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



# *** GLM model for LPG - Average House price -----

#model1 <- glm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_LPG, family="gaussian")
#predict(model1, type="response") 

model1 <- glm(avg_fuel_price ~ avg_house_price , data=com_House_fuel_station_LPG)
predict(model1, type="response")

glm(as.factor(avg_fuel_price) ~ as.factor(avg_house_price), family=binomial, data=com_House_fuel_station_LPG) %>%
  summary()


# *** Group for GLM ------

brand_and_fuel_price <- fuel_all %>% 
  select(Brand,Price,Postcode) %>%
  mutate(Brand=as.factor(Brand)) %>%
  group_by(Brand, Postcode) %>%
  summarize(avg_fuel_price = mean(Price))

glm(avg_fuel_price ~ Brand,  data=brand_and_fuel_price) %>%
  summary()

# **** Group for GLM By Group Station------

brand_station_ct_price <- fuel_all %>%
  group_by(Brand,Price)%>%
  summarise(brand_station_CT = n_distinct(ServiceStationName)) %>%
  mutate(station_group = ifelse(brand_station_CT < 40, "Group_1", ifelse(brand_station_CT < 100, "Group_2", ifelse(brand_station_CT < 200, "Group_3", "Group_4")))) %>%
  arrange(desc(brand_station_CT))

glm(as.factor(Price + 0) ~ as.factor(station_group), family=binomial, data=brand_station_ct_price) %>%
  summary()


# ******* testing---------------------------

# ** GLM by Type DL ---------------------------

glm(as.factor(avg_house_price + 0) ~ as.factor(Brand), family=binomial, data=com_House_fuel_DL) %>%
  summary()
# ** GLM by Type P98 ---------------------------
glm(as.factor(avg_house_price + 0) ~ as.factor(Brand), family=binomial, data=com_House_fuel_P98) %>%
  summary()
# ** GLM by Type LPG ---------------------------
glm(as.factor(avg_house_price + 0) ~ as.factor(Brand), family=binomial, data=com_House_fuel_LPG) %>%
  summary()


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
  geom_jitter(aes(colour = station_group)) +
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

LM1 <- lm(avg_fuel_price ~ avg_house_price + 0, com_House_fuel_station_LPG)
summary(LM1)
#plot(LM1)

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


#*** GLM model for DL ------------

Fuel_type_DL <- house_fuel_all_month %>%
  select(-P98, -LPG) %>%
  na.omit()

GlmDL <- glm(DL ~ pc_med_house_price, data = Fuel_type_DL) 
  summary(GlmDL)
plot(GlmDL)

#*** GLM model for LPG ------------
Fuel_type_LPG <- house_fuel_all_month %>%
  select(-P98, -DL) %>%
  na.omit()

GlmLPG <- glm(LPG ~ pc_med_house_price, data = Fuel_type_LPG) 
summary(GlmLPG)  

#*** GLM model for LPG ------------
Fuel_type_P98 <- house_fuel_all_month %>%
  select(-DL, -LPG) %>%
  na.omit()

GlmP98 <-glm(P98 ~ Brand, data = Fuel_type_P98) 
summary(GlmP98)
plot(GlmP98)


com_House_fuel_station_LPG %>%
  ggplot(aes(x = min_house_price, y = avg_fuel_price, colour = station_group )  ) +
  geom_point() +
  ggtitle("Minmum House Price Vs Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y="Average Fuel Price", x = "Minmum House Price") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  geom_smooth(method = "lm", se = FALSE)