# load packages ---------
library(tidyverse)
library(skimr)
library(here)
library(scales)

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
day_order <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
month_order <- c('August', 'September', 'October', 'November', 'December', 'January', 'February', 'March', 'April', 'May','June', 'July')

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
  filter(FuelCode == premium_unleaded) %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

fac <- with(average_fuel_price, reorder(Brand, avg_price, median, order = TRUE))
average_fuel_price$Brand <- factor(average_fuel_price$Brand, levels = levels(fac))

average_fuel_price %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

# test area for playing around with the dataset ---------

wrangling <- fuel_all %>%
  filter(FuelCode == "LPG") %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

fac <- with(wrangling, reorder(Brand, avg_price, median, order = TRUE))
wrangling$Brand <- factor(wrangling$Brand, levels = levels(fac))

wrangling %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot()

# Time series analysis ---------

time_date_day <- fuel_all %>%
  select(Brand, Postcode, FuelCode, Price, date) %>%
  mutate(Day = Day_of_the_week <- weekdays(fuel_all$date)) %>%
  filter(FuelCode == unleaded) %>%
  group_by(Postcode, FuelCode, Day) %>%
  summarise(avg_price = mean(Price))

time_date_day$Day <- factor(time_date_day$Day, level = day_order)

time_date_day %>%
  ggplot(aes(x = Day, y = avg_price)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

time_date_month <- fuel_all %>%
  select(Brand, Postcode, FuelCode, Price, date) %>%
  mutate(Month = month <- months(fuel_all$date)) %>%
  filter(FuelCode == unleaded) %>%
  group_by(Postcode, FuelCode, Month) %>%
  summarise(avg_price = mean(Price))

time_date_month$Month <- factor(time_date_month$Month, level = month_order)

time_date_month %>%
  ggplot(aes(x = Month, y = avg_price)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

  mutate(Quarter = quarter <- quarters(fuel_all$date)) %>%

# How is the data distrbuted and how do we visualise on a map ---------

Summary_percentage <- fuel_all %>%
  select(Brand) %>%
  group_by(Brand) %>%
  mutate(number_stations = number <- 1) %>%
  summarise(number_stations = sum(number_stations)) %>%
  mutate(percentage = number_stations / 409434 * 100) %>%
  arrange(desc(number_stations))
  
Summary_percentage_fuel <- fuel_all %>%
    select(FuelCode) %>%
    group_by(FuelCode) %>%
    mutate(number_fuel = number <- 1) %>%
    summarise(number_fuel = sum(number_fuel)) %>%
    mutate(percentage = number_fuel / 409434 * 100) %>%
    arrange(desc(number_fuel))

fuel_all %>%
  count(FuelCode == "E85")

# Yixin visualisation ---------

yixin_fuel_all <- fuel_all %>%
  filter(FuelCode != "B20", FuelCode != "E85", FuelCode != "EV")

ave_price<-aggregate(yixin_fuel_all$Price, by=list(yixin_fuel_all$date), FUN=mean)
colnames(ave_price)<-c("Date","average_fuel_price")                  

ggplot(data = ave_price, aes(x = Date, y = average_fuel_price))+
  geom_line(color = "#00AFBB", size = 2) +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b"))

code<-aggregate(yixin_fuel_all$Price, by=list(yixin_fuel_all$FuelCode,yixin_fuel_all$date), FUN=mean)
colnames(code)<-c("FuelCode","Date","Average_fuel_price")  

ggplot(data = code, aes(x = Date, y = Average_fuel_price,color = FuelCode))+
  geom_line() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b"))

#  facet_wrap(facets=vars(FuelCode))
# ggplot(data = StationsToChk, aes(x = PriceUpdatedDate, y = Price,group = Brand))+
#   geom_line(color = "#00AFBB", size = 0.5)+
#   facet_wrap(facets=vars(Brand))
#
# ggplot(data=StationsToChk,mapping=aes(x=Price))+
#   geom_freqpoly(mapping= aes(colour = Brand), binwidth=3) 

