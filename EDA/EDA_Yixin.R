
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

library(leaflet)

library(rgdal)

library(leaflet)

library(leaflet.extras)

library(ggmap)

register_google(key = "AIzaSyB1Fccl9sZOEUWC0NISGPdW5pclAp6vyo0")

library(RColorBrewer)

library(leaflet.providers) 

library(viridis)

library(sp)


#########################################

# House price data --------

House_data <- read.csv(here("House_Price_Data","Combined_HousePriceAug19ToJul20.csv"))

house_sa2_lat_long <- read.csv(here("Postcode_SA2","houselatlon.csv"))

house_all <- House_data %>%
  
  select(Property.ID,Property.Unit.Number,Property.House.Number,
         
         Property.Street.Name,Property.Suburb,Postal.Code,
         
         Purchase.Price,Settlement.Date)%>%
  
  mutate(fulladdress = paste(Property.House.Number,
                             
                             Property.Street.Name,Property.Suburb,Postal.Code, "AU",sep=","),
         
         Settlement_date = dmy(Settlement.Date),
         
         Settlement_month = format(as.Date(Settlement_date), "%Y-%m"),
         
         month= month(Settlement_date),
         
         Day = Day_of_the_week <- weekdays(Settlement_date))%>%
  
  rename(date=Settlement.Date,Postcode = Postal.Code)

house_all <- left_join(house_all, house_sa2_lat_long, by = "fulladdress")

write.csv(house_all, 'House_Price_Data//house_all.csv')

# Creating subset dataframes ---------

# read in data ---------

fuel_all <- read_csv(here("Fuel_2019-2020","fuel_all.csv"))

fuel_all<-filter(fuel_all, fuel_all$date >= '2019-08-01' & fuel_all$date <= '2020-07-31')

house_all <- read_csv(here("House_Price_Data", "house_all.csv"))

## aggregative fuel price start

fuel_station_Aggr <- fuel_all %>%
  
  select(Address,Brand, Mth, ServiceStationName,Postcode, Price,FuelCode, SA2,lat,lon) %>%
  
  group_by(Address,Brand, Mth, ServiceStationName,Postcode,FuelCode, SA2,lat,lon) %>%
  
  summarise(pc_avg_fuel_price=mean(Price)) %>%
  
  spread(FuelCode,pc_avg_fuel_price)

## aggregative fuel price start

house_price_aggr <- house_all %>%
  
  select(month, Postcode, Purchase.Price, SA2) %>%
  
  group_by(month, Postcode, SA2) %>%
  
  summarise(pc_med_house_price = median(Purchase.Price))

#Combine files ---------

house_fuel_all_month <- left_join(fuel_station_Aggr, house_price_aggr, by = c("SA2"))

#########################################

shapefile<-readOGR( dsn="SA2_2016_AUST.shp")

shapefile2<- shapefile[shapefile$STE_NAME16=="New South Wales", ] 

P98<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,FuelCode,Price,station_group,lon,lat,SA2))%>%
  
  filter(FuelCode=="P98")

P98 <-aggregate(P98$Price,by=list(P98$lat,P98$lon),FUN=mean)

colnames(P98)<-c("lat","lon","Ave_price")  


##P98 station count

title="P98 station counts"

leaflet(P98) %>% 
  
  addTiles() %>%
  
  addMarkers(~lon, ~lat,clusterOptions = markerClusterOptions())%>%
  
  addControl(title, position = "topright", className="map-title")%>%
  
  addPolygons(data = shapefile2,
              
              weight = 2, 
              
              color = "teal", 
              
              dashArray = " ", 
              
              ##fillColor = ~pal(SA2$Ave_price), 
              
              smoothFactor = 0.5,
              
              opacity = 1.0,
              
              fillOpacity = 0.5,
              
              highlightOptions = highlightOptions(color = "red", 
                                                  
                                                  weight = 3, 
                                                  
                                                  bringToFront = TRUE))





##LPG station counts

LPG<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,FuelCode,Price,station_group,lon,lat,SA2))%>%
  
  filter(FuelCode=="LPG")

LPG <-aggregate(LPG$Price,by=list(LPG$lat,LPG$lon),FUN=mean)

colnames(LPG)<-c("lat","lon","Ave_price")  

title2="LPG station counts"

leaflet(LPG) %>% 
  
  addTiles() %>%
  
  addMarkers(~lon, ~lat,clusterOptions = markerClusterOptions())%>%
  
  addControl(title2, position = "topright", className="map-title")%>%
  
  addPolygons(data = shapefile2,
              
              weight = 2, 
              
              color = "grey", 
              
              dashArray = " ", 
              
              ##fillColor = ~pal(SA2$Ave_price), 
              
              smoothFactor = 0.5,
              
              opacity = 1.0,
              
              fillOpacity = 0.5,
              
              highlightOptions = highlightOptions(color = "red", 
                                                  
                                                  weight = 3, 
                                                  
                                                  bringToFront = TRUE))



##DL station counts

DL<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,FuelCode,Price,station_group,lon,lat,SA2))%>%
  
  filter(FuelCode=="DL")

DL<-aggregate(DL$Price,by=list(DL$lat,DL$lon),FUN=mean)

colnames(DL)<-c("lat","lon","Ave_price")  

title3="DL station counts"

leaflet(DL) %>% 
  
  addTiles() %>%
  
  addMarkers(~lon, ~lat,clusterOptions = markerClusterOptions())%>%
  
  addControl(title3, position = "topright", className="map-title")%>%
  
  addPolygons(data = shapefile2,
              
              weight = 2, 
              
              color = "violet", 
              
              dashArray = " ", 
              
              ##fillColor = ~pal(SA2$Ave_price), 
              
              smoothFactor = 0.5,
              
              opacity = 1.0,
              
              fillOpacity = 0.5,
              
              highlightOptions = highlightOptions(color = "red", 
                                                  
                                                  weight = 3, 
                                                  
                                                  bringToFront = TRUE))



##P98 ave price

ave_P98<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,FuelCode,Price,station_group,lon,lat,SA2))%>%
  
  filter(FuelCode=="P98")

ave_P98 <-aggregate(ave_P98$Price,by=list(ave_P98$SA2),FUN=mean)

colnames(ave_P98)<-c("SA2_NAME16","Ave_price")  

ave_P98$Ave_price<-as.numeric(ave_P98$Ave_price)

shape3<-merge(shapefile2,ave_P98,by="SA2_NAME16")

##color bins- price range

##bins <- c(135,145,150,155,160,165,170,180)   

##pal function 

pal<-colorBin(viridis::viridis(6), shape3$Ave_price, 6 , pretty = TRUE,reverse = TRUE)

## leaflet add markers and use shape file as polygons

leaflet(shape3) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  ##addMarkers(~lon, ~lat,popup=~Ave_price,clusterOptions = markerClusterOptions())%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape3$Ave_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.5,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape3$Ave_price,
             
             opacity = 0.7,
             
             title = "P98 Average Fuel Price",
             
             position = "bottomright")



##LPG ave price

ave_LPG<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,FuelCode,Price,station_group,lon,lat,SA2))%>%
  
  filter(FuelCode=="LPG")

ave_LPG <-aggregate(ave_LPG$Price,by=list(ave_LPG$SA2),FUN=mean)

colnames(ave_LPG)<-c("SA2_NAME16","Ave_price")  

ave_LPG$Ave_price<-as.numeric(ave_LPG$Ave_price)

shape4<-merge(shapefile2,ave_LPG,by="SA2_NAME16")

##color bins- price range

##bins <- c(135,145,150,155,160,165,170,180)   

##pal function 

pal<-colorBin(viridis::viridis(5), shape4$Ave_price, 5 , pretty = TRUE,reverse = TRUE)

## leaflet add markers and use shape file as polygons

leaflet(shape4) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  ##addMarkers(~lon, ~lat,popup=~Ave_price,clusterOptions = markerClusterOptions())%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape4$Ave_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.5,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape4$Ave_price,
             
             opacity = 0.7,
             
             title = "LPG Average Fuel Price",
             
             position = "bottomright")



##DL ave price

ave_DL<-subset(fuel_all, select = c(Address,Suburb,Postcode,Brand,FuelCode,Price,station_group,lon,lat,SA2))%>%
  
  filter(FuelCode=="DL")

ave_DL <-aggregate(ave_DL$Price,by=list(ave_DL$SA2),FUN=mean)

colnames(ave_DL)<-c("SA2_NAME16","Ave_price")  

ave_DL$Ave_price<-as.numeric(ave_DL$Ave_price)

shape6<-merge(shapefile2,ave_DL,by="SA2_NAME16")

##color bins- price range

##bins <- c(135,145,150,155,160,165,170,180)   

##pal function 

pal<-colorBin(viridis::viridis(7), shape6$Ave_price, 7, pretty = TRUE,reverse = TRUE)

## leaflet add markers and use shape file as polygons

leaflet(shape6) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  ##addMarkers(~lon, ~lat,popup=~Ave_price,clusterOptions = markerClusterOptions())%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape6$Ave_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.5,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape6$Ave_price,
             
             opacity = 0.7,
             
             title = "DL Average Fuel Price",
             
             position = "bottomright")



#### house

colnames(house_price_aggr)<-c("month","Postcode","SA2_NAME16","house_median_price")  



median_house<-subset(house_price_aggr, select = c(SA2_NAME16,house_median_price))

median_house <-aggregate(median_house$house_median_price,by=list(median_house$SA2_NAME16), FUN=median)

colnames(median_house)<-c("SA2_NAME16","house_median_price")  

shape6<-merge(shapefile2,median_house,by="SA2_NAME16")

pal<-colorBin(viridis::viridis(10), shape6$house_median_price, 8, pretty = TRUE,reverse = TRUE)

## leaflet add markers and use shape file as polygons

leaflet(shape6) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  ##addMarkers(~lon, ~lat,popup=~house_median_price,clusterOptions = markerClusterOptions())%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape6$house_median_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.5,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape6$house_median_price,
             
             opacity = 0.7,
             
             title = "House Median Price",
             
             position = "bottomright")


#### house& fuel P98

fuel_station_Aggr <- aggregate(fuel_station_Aggr$P98,by=list(fuel_station_Aggr$lat , fuel_station_Aggr$lon),FUN=mean)

colnames(fuel_station_Aggr)<-c("lat","lon","Ave_fuel_Price" )

fuel_station_Aggr<-na.omit(fuel_station_Aggr)

pal2 <-colorBin("PuRd", fuel_station_Aggr$Ave_fuel_Price, 6, pretty = TRUE)

leaflet(shape6) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  addCircleMarkers(~fuel_station_Aggr$lon, ~fuel_station_Aggr$lat,
                   
                   stroke = FALSE, 
                   
                   fillOpacity = 1,
                   
                   opacity= 1,
                   
                   color = ~pal2(fuel_station_Aggr$Ave_fuel_Price),
                   
                   fill = TRUE, 
                   
                   fillColor = ~pal2(fuel_station_Aggr$Ave_fuel_Price))%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape6$house_median_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.3,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape6$house_median_price,
             
             opacity = 0.7,
             
             title = "House Median Price",
             
             position = "bottomright")%>%
  
  addLegend( pal= pal2, 
             
             values = ~fuel_station_Aggr$Ave_fuel_Price,
             
             opacity = 0.7,
             
             title = "P98 Average Fuel Pice ",
             
             position = "topright")



#### house& fuel LPG

pal2 <-colorBin("PuRd", LPG$Ave_price, 5, pretty = TRUE)

leaflet(shape6) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  addCircleMarkers(~LPG$lon, ~LPG$lat,
                   
                   stroke = FALSE, 
                   
                   fillOpacity = 1,
                   
                   opacity= 1,
                   
                   color = ~pal2(LPG$Ave_price),
                   
                   fill = TRUE, 
                   
                   fillColor = ~pal2(LPG$Ave_price))%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape6$house_median_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.3,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape6$house_median_price,
             
             opacity = 0.7,
             
             title = "House Median Price",
             
             position = "bottomright")%>%
  
  addLegend( pal= pal2, 
             
             values = ~LPG$Ave_price,
             
             opacity = 0.7,
             
             title = "LPG Average Fuel Pice ",
             
             position = "topright")


#### house& fuel DL

pal2 <-colorBin("PuRd", DL$Ave_price, 5, pretty = TRUE)

leaflet(shape6) %>% 
  
  addProviderTiles(providers$CartoDB)  %>%
  
  addCircleMarkers(~DL$lon, ~DL$lat,
                   
                   stroke = FALSE, 
                   
                   fillOpacity = 1,
                   
                   opacity= 1,
                   
                   color = ~pal2(DL$Ave_price),
                   
                   fill = TRUE, 
                   
                   fillColor = ~pal2(DL$Ave_price))%>%
  
  addPolygons(
    
    weight = 2, 
    
    color = "black", 
    
    dashArray = " ", 
    
    fillColor = ~pal(shape6$house_median_price), 
    
    smoothFactor = 0.5,
    
    opacity = 1.0,
    
    fillOpacity = 0.3,
    
    highlightOptions = highlightOptions(color = "red", 
                                        
                                        weight = 3, 
                                        
                                        bringToFront = TRUE)) %>%
  
  addLegend( pal= pal, 
             
             values = ~shape6$house_median_price,
             
             opacity = 0.7,
             
             title = "House Median Price",
             
             position = "bottomright")%>%
  
  addLegend( pal= pal2, 
             
             values = ~DL$Ave_price,
             
             opacity = 0.7,
             
             title = "DL Average Fuel Pice ",
             
             position = "topright")

