# load packages ---------
library(tidyverse)
library(skimr)
library(here)
library(scales)
library(ggpubr)
library(Ecdat)
library(forecast)

# read in data --------

fuel_all <- read_csv(here("EDA","fuel_all.csv"))


# new variables for EDA ---------

fuel_type_list <- as.list(levels(factor(fuel_all$FuelCode)))
postcode_list <- as.list(levels(factor(fuel_all$Postcode)))
station_list <- as.list(levels(factor(fuel_all$Brand)))
day_order <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
month_order <- c('August', 'September', 'October', 'November', 'December', 'January', 'February', 'March', 'April', 'May','June', 'July')


# write cleaned data to .csv ---------

# Summary data ---------

Summary_percentage <- fuel_all %>%
  select(Brand) %>%
  group_by(Brand) %>%
  mutate(number_stations = number <- 1) %>%
  summarise(number_stations = sum(number_stations)) %>%
  mutate(percentage = number_stations / 279955 * 100) %>%
  arrange(desc(number_stations))

Summary_percentage_fuel <- fuel_all %>%
  select(FuelCode) %>%
  group_by(FuelCode) %>%
  mutate(number_fuel = number <- 1) %>%
  summarise(number_fuel = sum(number_fuel)) %>%
  mutate(percentage = number_fuel / 279955 * 100) %>%
  arrange(desc(number_fuel))

fuel_all %>%
  count(FuelCode == "P98")


# Fuel price over time visualisation ---------

ave_price<-aggregate(fuel_all$Price, by=list(fuel_all$date), FUN=mean)
colnames(ave_price)<-c("Date","average_fuel_price")                  

fuel_avg_all <- ggplot(data = ave_price, aes(x = Date, y = average_fuel_price))+
  geom_line(color = "#00AFBB", size = 2) +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  ggtitle("Average Fuel Price", subtitle = "(Fuel Type: ALL)") +
  labs(y= "Average Fuel Price (Cents)", x = "Date")

code<-aggregate(fuel_all$Price, by=list(fuel_all$FuelCode,fuel_all$date), FUN=mean)
colnames(code)<-c("FuelCode","Date","Average_fuel_price")  

fuel_avg_sep <- ggplot(data = code, aes(x = Date, y = Average_fuel_price,color = FuelCode))+
  geom_line() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  theme(legend.position = "top") +
  labs(y= "Average Fuel Price (Cents)", x = "Date")

# * Figure 1 ----

figure1 <- ggarrange(fuel_avg_all, fuel_avg_sep,
                     labels = c("A", "B"),
                     nrow = 2)
figure1


# Seasonality of fuel price ---------

# * Daily data ----

time_date_day <- fuel_all %>%
  select(Brand, Postcode, FuelCode, Price, date) %>%
  mutate(Day = Day_of_the_week <- weekdays(fuel_all$date)) %>%
  filter(FuelCode == fuel_type_list) %>%
  group_by(Postcode, FuelCode, Day) %>%
  summarise(avg_price = mean(Price))

time_date_day$Day <- factor(time_date_day$Day, level = day_order)

time_date_day %>%
  ggplot(aes(x = Day, y = avg_price)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

daily_dl <- time_date_day %>%
  filter(FuelCode == "DL") %>%
  ggplot(aes(x = Day, y = avg_price)) +
  geom_boxplot()

daily_p98 <- time_date_day %>%
  filter(FuelCode == "P98") %>%
  ggplot(aes(x = Day, y = avg_price)) +
  geom_boxplot()

daily_lpg <- time_date_day %>%
  filter(FuelCode == "LPG") %>%
  ggplot(aes(x = Day, y = avg_price)) +
  geom_boxplot()

# * Figure 2 ----

figure2 <- ggarrange(daily_p98, daily_dl, daily_lpg,
                     labels = c("A", "B", "C"),
                     ncol = 2, nrow = 2)
figure2

# * Monthly data ----

time_date_month <- fuel_all %>%
  select(Brand, Postcode, FuelCode, Price, date) %>%
  mutate(Month = month <- months(fuel_all$date)) %>%
  filter(FuelCode == fuel_type_list) %>%
  group_by(Postcode, FuelCode, Month) %>%
  summarise(avg_price = mean(Price))

time_date_month$Month <- factor(time_date_month$Month, level = month_order)

time_date_month %>%
  ggplot(aes(x = Month, y = avg_price)) +
  geom_boxplot() +
  facet_wrap(~FuelCode)

monthly_dl <- time_date_month %>%
  filter(FuelCode == "DL") %>%
  ggplot(aes(x = Month, y = avg_price)) +
  geom_boxplot()

monthly_p98 <- time_date_month %>%
  filter(FuelCode == "P98") %>%
  ggplot(aes(x = Month, y = avg_price)) +
  geom_boxplot()

monthly_lpg <- time_date_month %>%
  filter(FuelCode == "LPG") %>%
  ggplot(aes(x = Month, y = avg_price)) +
  geom_boxplot()

mutate(Quarter = quarter <- quarters(fuel_all$date))

# * Figure 3 ----

figure3 <- ggarrange(monthly_p98, monthly_dl, monthly_lpg,
                     labels = c("A", "B", "C"),
                     ncol = 2, nrow = 2)
figure3

# Fuel Price variation ---------

average_fuel_price <- fuel_all %>%
  filter(FuelCode == fuel_type_list) %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

fac <- with(average_fuel_price, reorder(Brand, avg_price, median, order = TRUE))
average_fuel_price$Brand <- factor(average_fuel_price$Brand, levels = levels(fac))

DL <- average_fuel_price %>%
  filter(FuelCode == "DL") %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  ggtitle("Average Fuel Price", subtitle = "(Fuel Type: DL)") +
  labs(y= "Brand", x = "Average Fuel Price (Cents)")

LPG <- average_fuel_price %>%
  filter(FuelCode == "LPG") %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  ggtitle("Average Fuel Price", subtitle = "(Fuel Type: LPG)") +
  labs(y= "Brand", x = "Average Fuel Price (Cents)")

P98 <- average_fuel_price %>%
  filter(FuelCode == "P98") %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot() +
  ggtitle("Average Fuel Price", subtitle = "(Fuel Type: P98)") +
  labs(y= "Brand", x = "Average Fuel Price (Cents)")

figure4 <- ggarrange(P98, DL, LPG,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)
figure4


# Trend analysis ---------

# * Fuel price cycle analysis ----

timeseries_fuel = ave_price
plot(as.ts(timeseries_fuel$average_fuel_price))

trend_fuel = ma(timeseries_fuel$average_fuel_price, order = 13, centre = T)
plot(as.ts(timeseries_fuel$average_fuel_price))
lines(trend_fuel)
plot(as.ts(trend_fuel))

detrend_fuel = timeseries_fuel$average_fuel_price / trend_fuel
plot(as.ts(detrend_fuel))

cycle_fuel = t(matrix(data = detrend_fuel, nrow = 13))
seasonal_fuel = colMeans(cycle_fuel, na.rm = T)
plot(as.ts(rep(seasonal_fuel, 13)))

random_fuel = timeseries_fuel$average_fuel_price / (trend_fuel * seasonal_fuel)
plot(as.ts(random_fuel))

recomposed_fuel = trend_fuel*seasonal_fuel*random_fuel
plot(as.ts(recomposed_fuel))


# Test area for playing around with the dataset ---------

wrangling <- fuel_all %>%
  filter(FuelCode == fuel_type_list) %>%
  select(Brand, Postcode, FuelCode, Price) %>%
  group_by(FuelCode, Brand, Postcode) %>%
  summarise(avg_price = mean(Price))

fac <- with(wrangling, reorder(Brand, avg_price, median, order = TRUE))
wrangling$Brand <- factor(wrangling$Brand, levels = levels(fac))

wrangling %>%
  ggplot(aes(x = avg_price, y = Brand)) +
  geom_boxplot()
