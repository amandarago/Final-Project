rm(list = ls())
#To import data use rio package
#https://www.rdocumentation.org/packages/rio/versions/0.5.27
#https://subscription.packtpub.com/book/big-data-and-business-intelligence/9781784391034/1/ch01lvl1sec15/loading-your-data-into-r-with-rio-packages
library(tidyverse)
library(forecast)
library (rio)
library (lubridate)
library (Metrics)

########################################################## Import Data #######################################################

import01 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/MetroBikeShare_2016_Q3_trips-2.zip")
colnames(import01)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")
import01$start_time = mdy_hm(import01$start_time)
import01$end_time = mdy_hm(import01$end_time)

import02 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/09/metro-bike-share-trips-2016-q4.csv.zip")
colnames(import02)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")

import03 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/11/la_metro_gbfs_trips_Q1_2017-2.zip")
colnames(import03)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")
import03$start_time = mdy_hm(import03$start_time)
import03$end_time = mdy_hm(import03$end_time)

import04 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2017/07/la_metro_gbfs_trips_Q2_2017.csv.zip")
colnames(import04)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")

import05 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2016/10/metro-bike-share-trips-2017-q3.csv.zip")
colnames(import05)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")
import05$start_time = mdy_hm(import05$start_time)
import05$end_time = mdy_hm(import05$end_time)

import06 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/02/metro-bike-share-trips-2017-q4-v2.csv.zip")
colnames(import06)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")

import07 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/04/metro-bike-share-trips-2018-q1.csv.zip")
colnames(import07)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")

import08 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/08/metro-bike-share-trips-2018-q2.csv.zip")
colnames(import08)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")

import09 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2018/10/metro-bike-share-trips-2018-q3.csv.zip")
colnames(import09)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type")

import10 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/01/metro-bike-share-trips-2018-q4.csv.zip")
colnames(import10)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import10 = subset(import10, select = -bike_type)

import11 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/04/metro-bike-share-trips-2019-q1.csv.zip")
colnames(import11)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import11 = subset(import11, select = -bike_type)

import12 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/08/metro-bike-share-trips-2019-q2.csv.zip")
colnames(import12)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import12 = subset(import12, select = -bike_type)

import13 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2019/10/metro-bike-share-trips-2019-q3-1.zip")
colnames(import13)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import13 = subset(import13, select = -bike_type)
import13$start_time = mdy_hm(import13$start_time)
import13$end_time = mdy_hm(import13$end_time)

import14 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/01/metro-bike-share-trips-2019-q4.csv.zip")
colnames(import14)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import14 = subset(import14, select = -bike_type)

import15 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/04/metro-bike-share-trips-2020-q1.zip")
colnames(import15)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import15 = subset(import15, select = -bike_type)
import15$start_time = mdy_hm(import15$start_time)
import15$end_time = mdy_hm(import15$end_time)

import16 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/03/metro-trips-2020-q2-v2.zip")
colnames(import16)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import16 = subset(import16, select = -bike_type)
import16$start_time = mdy_hm(import16$start_time)
import16$end_time = mdy_hm(import16$end_time)

import17 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2020/10/metro-trips-2020-q3.zip")
colnames(import17)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import17 = subset(import17, select = -bike_type)
import17$start_time = mdy_hm(import17$start_time)
import17$end_time = mdy_hm(import17$end_time)

import18 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/01/metro-trips-2020-q4.zip")
colnames(import18)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import18 = subset(import18, select = -bike_type)
import18$start_time = mdy_hm(import18$start_time)
import18$end_time = mdy_hm(import18$end_time)

import19 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/04/metro-trips-2021-q1-1.zip")
colnames(import19)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import19 = subset(import19, select = -bike_type)
import19$start_time = mdy_hm(import19$start_time)
import19$end_time = mdy_hm(import19$end_time)

import20 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/07/metro-trips-2021-q2.zip",which=2)
colnames(import20)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type","start_station_name","end_station_name")
import20 = subset(import20, select = -c(bike_type,start_station_name,end_station_name))
import20$start_time = mdy_hm(import20$start_time)
import20$end_time = mdy_hm(import20$end_time)

import21 = import("https://11ka1d3b35pv1aah0c3m9ced-wpengine.netdna-ssl.com/wp-content/uploads/2021/10/metro-trips-2021-q3.zip",which=2)
colnames(import21)=
  c("trip_id","duration","start_time","end_time","start_station","start_lat","start_lon","end_station","end_lat","end_lon",
    "bike_id","plan_duration","trip_route_category","passholder_type","bike_type")
import21 = subset(import21, select = -bike_type)
import21$start_time = mdy_hm(import21$start_time)
import21$end_time = mdy_hm(import21$end_time)

data = rbind(import01,import02,import03,import04,import05,import06,import07,import08,import09,
             import10,import11,import12,import13,import14,import15,import16,import17,import18,import19,
             import20,import21)

rm(import01,import02,import03,import04,import05,import06,import07,import08,import09,
   import10,import11,import12,import13,import14,import15,import16,import17,import18,import19,
   import20,import21)

########################################################## Data Cleansing #######################################################

#Splitting data into separate regions:

import.regions = import("https://bikeshare.metro.net/wp-content/uploads/2021/10/metro-bike-share-stations-2021-10-01.csv")
colnames(import.regions)=
  c("station_id","station_name","station_creation_date","region","status")
import.regions = subset(import.regions, select = -c(station_name,station_creation_date,status))

data = left_join(data,import.regions,by=c("start_station"="station_id"))

rm(import.regions)

#This will turn the list of rentals into hourly demand (for aggregate, regions DTLA, Westside, and North Hollywood)

data = data[c(which(data$region=='DTLA'),which(data$region=='Westside'),which(data$region=='North Hollywood')),]
total_hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))
demand_df = data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())
total_hourly_demand = left_join(total_hourly_demand,demand_df,by="hour")
total_hourly_demand[is.na(total_hourly_demand)] = 0
rm(demand_df)

total_hourly_demand %>% head(20)

DTLA_data = data[ which(data$region=='DTLA'),]
Pasadena_data = data[ which(data$region=='Pasadena'),]
Westside_data = data[ which(data$region=='Westside'),]
NorthHollywood_data = data[ which(data$region=='North Hollywood'),]

DTLA_hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))
DTLA_demand_df = DTLA_data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())
DTLA_hourly_demand = left_join(DTLA_hourly_demand,DTLA_demand_df,by="hour")
DTLA_hourly_demand[is.na(DTLA_hourly_demand)] = 0
rm(DTLA_demand_df)
rm(DTLA_data)

Pasadena_hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))
Pasadena_demand_df = Pasadena_data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())
Pasadena_hourly_demand = left_join(Pasadena_hourly_demand,Pasadena_demand_df,by="hour")
Pasadena_hourly_demand[is.na(Pasadena_hourly_demand)] = 0
rm(Pasadena_demand_df)
rm(Pasadena_data)

Westside_hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))
Westside_demand_df = Westside_data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())
Westside_hourly_demand = left_join(Westside_hourly_demand,Westside_demand_df,by="hour")
Westside_hourly_demand[is.na(Westside_hourly_demand)] = 0
rm(Westside_demand_df)
rm(Westside_data)

NorthHollywood_hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))
NorthHollywood_demand_df = NorthHollywood_data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())
NorthHollywood_hourly_demand = left_join(NorthHollywood_hourly_demand,NorthHollywood_demand_df,by="hour")
NorthHollywood_hourly_demand[is.na(NorthHollywood_hourly_demand)] = 0
rm(NorthHollywood_demand_df)
rm(NorthHollywood_data)

##################################################### Data Visualization ####################################################

#Aggregate (Total)
total_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()

#DTLA
DTLA_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()

#Pasadena
Pasadena_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()
#It seems like all Pasadena stations are inactive, so no need to forecast this region

#Westside
Westside_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()
#Looks like data starts about halfway through 2017

Westside_hourly_demand %>% 
  slice(which(Westside_hourly_demand$hour == mdy_hm("06-01-2017 01:00")):dim(Westside_hourly_demand)[1]) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()

#North Hollywood
NorthHollywood_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()
#Looks like data starts about halfway through 2019

NorthHollywood_hourly_demand %>% 
  slice(which(NorthHollywood_hourly_demand$hour == mdy_hm("08-05-2019 01:00")):dim(NorthHollywood_hourly_demand)[1]) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+theme_bw()

############################################## Creating Time Series Objects ###################################################

#Aggregate(Total) - Testing: July 1 2021 - Sep 30 2021

total.y.train = msts(total_hourly_demand$actual_demand[total_hourly_demand$hour >= as.Date("2016-07-07",format=c("%Y-%m-%d")) &
     total_hourly_demand$hour <= as.Date("2021-06-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7,365.25*24))

total.y.test = msts(total_hourly_demand$actual_demand[total_hourly_demand$hour >= as.Date("2021-07-01",format=c("%Y-%m-%d")) &
      total_hourly_demand$hour <= as.Date("2021-09-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7,365.25*24))

#DTLA - Testing: July 1 2021 - Sep 30 2021

DTLA.y.train = msts(DTLA_hourly_demand$actual_demand[DTLA_hourly_demand$hour >= as.Date("2016-07-07",format=c("%Y-%m-%d")) &
      DTLA_hourly_demand$hour <= as.Date("2021-06-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7,365.25*24))

DTLA.y.test = msts(DTLA_hourly_demand$actual_demand[DTLA_hourly_demand$hour >= as.Date("2021-07-01",format=c("%Y-%m-%d")) &
      DTLA_hourly_demand$hour <= as.Date("2021-09-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7,365.25*24))

#Westside - Training: Sep 1 2017 - June 30 2021 Testing: July 1 2021 - Sep 30 2021

Westside.y.train = msts(Westside_hourly_demand$actual_demand[Westside_hourly_demand$hour >= as.Date("2017-09-01",format=c("%Y-%m-%d")) &
      Westside_hourly_demand$hour <= as.Date("2021-06-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7,365.25*24))

Westside.y.test = msts(Westside_hourly_demand$actual_demand[Westside_hourly_demand$hour >= as.Date("2021-07-01",format=c("%Y-%m-%d")) &
      Westside_hourly_demand$hour <= as.Date("2021-09-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7,365.25*24))

#North Hollywood - Training Aug 5 2019- June 30 2021 Testing: July 1 2021 - Sep 30 2021

NorthHollywood.y.train = msts(NorthHollywood_hourly_demand$actual_demand[NorthHollywood_hourly_demand$hour >= as.Date("2019-08-05",format=c("%Y-%m-%d")) &
      NorthHollywood_hourly_demand$hour <= as.Date("2021-06-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7))

NorthHollywood.y.test = msts(NorthHollywood_hourly_demand$actual_demand[NorthHollywood_hourly_demand$hour >= as.Date("2021-07-01",format=c("%Y-%m-%d")) &
      NorthHollywood_hourly_demand$hour <= as.Date("2021-09-30",format=c("%Y-%m-%d"))],seasonal.periods = c(24,24*7))

#Yearly seasonality removed because we have less than two years of training data.

##################################################### Model Building ####################################################

M1.total = mstl(total.y.train)
M1.DTLA = mstl(DTLA.y.train)
M1.Westside = mstl(Westside.y.train)
M1.NorthHollywood = mstl(NorthHollywood.y.train)

##################################################### ARIMA ####################################################

M1.totalF = forecast(M1.total,method="arima",
               h=length(total.y.test))
#               xreg = TempData$Temperature[1:length(total.y.train)], 
#               newxreg = TempData$Temperature[length(total.y.train)+1:length(total.y.test)])

autoplot(M1.totalF,PI=F)+autolayer(M1.totalF$fitted)

accuracy(M1.totalF)

length(as.numeric(total.y.test))
length(as.numeric(M1.totalF[["mean"]]))

#Random Weekly Forecast vs. Actual for M1.total

data.frame(Time = 1:length(as.numeric(total.y.test)),
           total.y.test = as.numeric(total.y.test),
           M1.total.prediction = as.numeric(M1.totalF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=total.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M1.total.prediction),col="red")+theme_bw()

smape(as.numeric(total.y.test),as.numeric(M1.totalF[["mean"]]))
#SMAPE: 0.61%

################################################### Exponential Smoothing ####################################################


##################################################### Neural Network ####################################################




