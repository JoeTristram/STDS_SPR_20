library(dplyr)
library(readxl)
library(plyr)

files <- list.files(path = "./Fuel_2019-2020/",
                    pattern='*.xlsx',
                    full.names = T)

data <- ldply(files, read_excel, sheet = 1, skip = 2, col_names = TRUE) %>%
  fill(everything(), .direction = "down")
