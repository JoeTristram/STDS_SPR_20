install.packages('ggpubr')
library(ggpubr)
library(leaflet.extras)
library(ggmap)
library(ggpubr)
library(gridExtra)

########################################################
#Import/combine 12 month's fuel data --------
# Create chart for 2019 XMAS 
### lead in lead out 2019 XMAS school holiday, chart 2019XMASFivedayLessThan40Brands

x_40 <- XMAS19Before_After_Holiday %>% 
  filter(station_group=="Group_1")%>%
  ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
  geom_line(aes(color=Brand))+
  #chart for group_2, brand with less than 100 stations, chart 2019MASFivedayLessThan100Brands
  
  x_100 <- XMAS19Before_After_Holiday %>% 
    filter(station_group=="Group_2")%>%
    ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
    geom_line(aes(color=Brand))+
    ylab("Average Fuel Price $cent")+
    ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 100 stations")+
    labs(color="Brands")+facet_wrap(~period,ncol=1)+
    scale_x_continuous(breaks = seq(1, 10, by = 1))
  #chart for group_3, brand with less than 200 stations,chart 2019XMASFivedayLessThan200Brands
  x_200 <- XMAS19Before_After_Holiday %>% 
    filter(station_group=="Group_3")%>%
    ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
    geom_line(aes(color=Brand))+ylab("Average Fuel Price $cent")+
    ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 200 stations")+
    labs(color="Brands")+ facet_wrap(~period,ncol=1)+
    scale_x_continuous(breaks = seq(1, 10, by = 1))
  #chart for group_4, brand with less than 400 stations
  x_400 <- XMAS19Before_After_Holiday %>% 
    filter(station_group=="Group_4")%>%
    ggplot(aes(x = as.numeric(day_number), y = brand_daily_avg)) +
    geom_line(aes(color=Brand))+ ylab("Average Fuel Price $cent")+
    ggtitle("Lead in/out 2019 Christmas holiday", subtitle = "Brand with less than 400 stations")+
    labs(color="Brands")+facet_wrap(~period,ncol=1)+
    scale_x_continuous(breaks = seq(1, 10, by = 1))
  
  