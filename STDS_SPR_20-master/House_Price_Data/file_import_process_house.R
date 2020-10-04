library(dplyr)
library(readxl)
library(plyr)

files <- list.files(path = "./Fuel_2019-2020/",
                    pattern='*.xlsx',
                    full.names = T)

data <- ldply(files, read_excel, sheet = 1, skip = 2, col_names = TRUE) %>%
  fill(everything(), .direction = "down")

## Dataframe daily average house price per postcode
average_daily_house <- house_all %>%
  group_by(Postcode, date) %>%
  mutate(avg_house_price=mean(PurchasePrice, na.rm = TRUE))

## Dataframe monthly average house price per postcode

average_mth_house <- house_all %>%
  select(date, PurchasePrice, Postcode)%>%
  mutate(Settlement_date = dmy(date),
         Settlement_month = 
           format(as.Date(Settlement_date), "%Y-%m"))%>%
  group_by(Postcode, Settlement_month) %>%
  summarise(avg_house_price=mean(PurchasePrice, na.rm = TRUE), SettlementCount=n()) 