rm(list = ls())
#To import data use rio package
#https://www.rdocumentation.org/packages/rio/versions/0.5.27
#https://subscription.packtpub.com/book/big-data-and-business-intelligence/9781784391034/1/ch01lvl1sec15/loading-your-data-into-r-with-rio-packages
library(tidyverse)
library(forecast)
library(rio)
library(lubridate)
library(Metrics)
library(prophet)

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

DTLA_data = data[ which(data$region=='DTLA'),]
Pasadena_data = data[ which(data$region=='Pasadena'),]
Westside_data = data[ which(data$region=='Westside'),]
NorthHollywood_data = data[ which(data$region=='North Hollywood'),]

#This will turn the list of rentals into hourly demand (for aggregate, regions DTLA, Westside, and North Hollywood)
data = data[c(which(data$region=='DTLA'),which(data$region=='Westside'),which(data$region=='North Hollywood')),]
total_hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))
demand_df = data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())
total_hourly_demand = left_join(total_hourly_demand,demand_df,by="hour")
total_hourly_demand[is.na(total_hourly_demand)] = 0
rm(demand_df)

total_hourly_demand %>% head(20)

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

total.y.train = msts(total_hourly_demand$actual_demand[total_hourly_demand$hour >= ymd_hm("2016-07-07 04:00") &
                                                         total_hourly_demand$hour <= ymd_hm("2021-06-30 23:00")],seasonal.periods = c(24,24*7,365.25*24))

total.y.test = msts(total_hourly_demand$actual_demand[total_hourly_demand$hour >= ymd_hm("2021-07-01 00:00") &
                                                        total_hourly_demand$hour <= ymd_hm("2021-09-30 23:00")],seasonal.periods = c(24,24*7,365.25*24))

#DTLA - Testing: July 1 2021 - Sep 30 2021

DTLA.y.train = msts(DTLA_hourly_demand$actual_demand[DTLA_hourly_demand$hour >= ymd_hm("2016-07-07 04:00") &
                                                       DTLA_hourly_demand$hour <= ymd_hm("2021-06-30 23:00")],seasonal.periods = c(24,24*7,365.25*24))

DTLA.y.test = msts(DTLA_hourly_demand$actual_demand[DTLA_hourly_demand$hour >= ymd_hm("2021-07-01 00:00") &
                                                      DTLA_hourly_demand$hour <= ymd_hm("2021-09-30 23:00")],seasonal.periods = c(24,24*7,365.25*24))

#Westside - Training: Sep 1 2017 - June 30 2021 Testing: July 1 2021 - Sep 30 2021

Westside.y.train = msts(Westside_hourly_demand$actual_demand[Westside_hourly_demand$hour >= ymd_hm("2017-09-01 00:00") &
                                                               Westside_hourly_demand$hour <= ymd_hm("2021-06-30 23:00")],seasonal.periods = c(24,24*7,365.25*24))

Westside.y.test = msts(Westside_hourly_demand$actual_demand[Westside_hourly_demand$hour >= ymd_hm("2021-07-01 00:00") &
                                                              Westside_hourly_demand$hour <= ymd_hm("2021-09-30 23:00")],seasonal.periods = c(24,24*7,365.25*24))

#North Hollywood - Training Aug 5 2019- June 30 2021 Testing: July 1 2021 - Sep 30 2021

NorthHollywood.y.train = msts(NorthHollywood_hourly_demand$actual_demand[NorthHollywood_hourly_demand$hour >= ymd_hm("2019-08-05 00:00") &
                                                                           NorthHollywood_hourly_demand$hour <= ymd_hm("2021-06-30 23:00")],seasonal.periods = c(24,24*7))

NorthHollywood.y.test = msts(NorthHollywood_hourly_demand$actual_demand[NorthHollywood_hourly_demand$hour >= ymd_hm("2021-07-01 00:00") &
                                                                          NorthHollywood_hourly_demand$hour <= ymd_hm("2021-09-30 23:00")],seasonal.periods = c(24,24*7))

#Yearly seasonality removed because we have less than two years of training data.

##################################################### Model Building ####################################################

M.total = mstl(total.y.train)
M.DTLA = mstl(DTLA.y.train)
M.Westside = mstl(Westside.y.train)
M.NorthHollywood = mstl(NorthHollywood.y.train)

########################################################## ARIMA ####################################################


######## Aggregate (Total)

M1.totalF = forecast(M.total,method="arima",
                     h=length(total.y.test))

autoplot(M1.totalF,PI=F)+autolayer(M1.totalF$fitted)

length(as.numeric(total.y.test))
length(as.numeric(M1.totalF[["mean"]]))

#Random Weekly Forecast vs. Actual for M1.total

data.frame(Time = 1:length(as.numeric(total.y.test)),
           total.y.test = as.numeric(total.y.test),
           M1.total.prediction = as.numeric(M1.totalF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=total.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M1.total.prediction),col="red")+theme_bw()

smape(as.numeric(total.y.test),as.numeric(M1.totalF[["mean"]]))
#sMAPE = 0.53

####### DTLA

M1.DTLAF = forecast(M.DTLA,method="arima",
                    h=length(DTLA.y.test))

autoplot(M1.DTLAF,PI=F)+autolayer(M1.DTLAF$fitted)

length(as.numeric(DTLA.y.test))
length(as.numeric(M1.DTLAF[["mean"]]))

#Random Weekly Forecast vs. Actual for M1.DTLA

data.frame(Time = 1:length(as.numeric(DTLA.y.test)),
           DTLA.y.test = as.numeric(DTLA.y.test),
           M1.DTLA.prediction = as.numeric(M1.DTLAF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=DTLA.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M1.DTLA.prediction),col="red")+theme_bw()

smape(as.numeric(DTLA.y.test),as.numeric(M1.DTLAF[["mean"]]))
#sMAPE = 0.60

####### Westside

M1.WestsideF = forecast(M.Westside,method="arima",
                        h=length(Westside.y.test))

autoplot(M1.WestsideF,PI=F)+autolayer(M1.WestsideF$fitted)

length(as.numeric(Westside.y.test))
length(as.numeric(M1.WestsideF[["mean"]]))

#Random Weekly Forecast vs. Actual for M1.Westside

data.frame(Time = 1:length(as.numeric(Westside.y.test)),
           Westside.y.test = as.numeric(Westside.y.test),
           M1.Westside.prediction = as.numeric(M1.WestsideF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=Westside.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M1.Westside.prediction),col="red")+theme_bw()

smape(as.numeric(Westside.y.test),as.numeric(M1.WestsideF[["mean"]]))
#sMAPE = 0.79


####### North Hollywood

M1.NorthHollywoodF = forecast(M.NorthHollywood,method="arima",
                              h=length(NorthHollywood.y.test))

autoplot(M1.NorthHollywoodF,PI=F)+autolayer(M1.NorthHollywoodF$fitted)

length(as.numeric(NorthHollywood.y.test))
length(as.numeric(M1.NorthHollywoodF[["mean"]]))

#Random Weekly Forecast vs. Actual for M1.NorthHollywood

data.frame(Time = 1:length(as.numeric(NorthHollywood.y.test)),
           NorthHollywood.y.test = as.numeric(NorthHollywood.y.test),
           M1.NorthHollywood.prediction = as.numeric(M1.NorthHollywoodF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=NorthHollywood.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M1.NorthHollywood.prediction),col="red")+theme_bw()

smape(as.numeric(NorthHollywood.y.test),as.numeric(M1.NorthHollywoodF[["mean"]]))
#sMAPE = 1.94

################################################### Exponential Smoothing ####################################################

######## Aggregate (Total)

M2.totalF = forecast(M.total,method="ets",
                     h=length(total.y.test))

autoplot(M2.totalF,PI=F)+autolayer(M2.totalF$fitted)

length(as.numeric(total.y.test))
length(as.numeric(M2.totalF[["mean"]]))

#Random Weekly Forecast vs. Actual for M2.total
data.frame(Time = 1:length(as.numeric(total.y.test)),
           total.y.test = as.numeric(total.y.test),
           M2.total.prediction = as.numeric(M2.totalF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=total.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M2.total.prediction),col="red")+theme_bw()

smape(as.numeric(total.y.test),as.numeric(M2.totalF[["mean"]]))
#sMAPE = 0.54

####### DTLA

M2.DTLAF = forecast(M.DTLA,method="ets", h=length(DTLA.y.test))

autoplot(M2.DTLAF,PI=F)+autolayer(M2.DTLAF$fitted)

length(as.numeric(DTLA.y.test))
length(as.numeric(M2.DTLAF[["mean"]]))

#Random Weekly Forecast vs. Actual for M2.DTLA
data.frame(Time = 1:length(as.numeric(DTLA.y.test)), DTLA.y.test = as.numeric(DTLA.y.test), M2.DTLA.prediction = as.numeric(M2.DTLAF[["mean"]])) %>% 
  slice(1:(7*24)+100) %>% ggplot(aes(x=Time,y=DTLA.y.test))+geom_line() + 
  geom_line(aes(x=Time,y=M2.DTLA.prediction),col="red") + theme_bw()

smape(as.numeric(DTLA.y.test),as.numeric(M2.DTLAF[["mean"]]))
#sMAPE = 0.60

####### Westside

M2.WestsideF = forecast(M.Westside,method="ets", h=length(Westside.y.test))

autoplot(M2.WestsideF,PI=F)+autolayer(M2.WestsideF$fitted)

length(as.numeric(Westside.y.test))
length(as.numeric(M2.WestsideF[["mean"]]))

#Random Weekly Forecast vs. Actual for M2.Westside
data.frame(Time = 1:length(as.numeric(Westside.y.test)),
           Westside.y.test = as.numeric(Westside.y.test),
           M2.Westside.prediction = as.numeric(M2.WestsideF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=Westside.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M2.Westside.prediction),col="red") + theme_bw()

smape(as.numeric(Westside.y.test),as.numeric(M2.WestsideF[["mean"]]))
#sMAPE = 0.77

####### North Hollywood

M2.NorthHollywoodF = forecast(M.NorthHollywood,method="ets",
                              h=length(NorthHollywood.y.test))

autoplot(M2.NorthHollywoodF,PI=F)+autolayer(M2.NorthHollywoodF$fitted)

length(as.numeric(NorthHollywood.y.test))
length(as.numeric(M2.NorthHollywoodF[["mean"]]))

#Random Weekly Forecast vs. Actual for M2.NorthHollywood
data.frame(Time = 1:length(as.numeric(NorthHollywood.y.test)),
           NorthHollywood.y.test = as.numeric(NorthHollywood.y.test),
           M2.NorthHollywood.prediction = as.numeric(M2.NorthHollywoodF[["mean"]])) %>% slice(1:(7*24)+100) %>% 
  ggplot(aes(x=Time,y=NorthHollywood.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M2.NorthHollywood.prediction),col="red")+theme_bw()

smape(as.numeric(NorthHollywood.y.test),as.numeric(M2.NorthHollywoodF[["mean"]]))
#sMAPE = 1.93

##################################################### Neural Network ####################################################

######## Aggregate (Total)

M3.total = nnetar(total.y.train,MaxNWts=1177)

M3.totalF = forecast(M3.total,h=length(total.y.test))

autoplot(M3.totalF,PI=F)+autolayer(M3.totalF$fitted)

data.frame(Time = 1:length(as.numeric(total.y.test)),
           total.y.test = as.numeric(total.y.test),
           M3.total.prediction = as.numeric(M3.totalF[["mean"]])) %>% slice(1:(7*24)+1000) %>% 
  ggplot(aes(x=Time,y=total.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M3.total.prediction),col="red")+theme_bw()

smape(as.numeric(total.y.test),as.numeric(M3.totalF[["mean"]]))

#sMAPE:0.41

######## DTLA

M3.DTLA = nnetar(DTLA.y.train,MaxNWts=1177)

M3.DTLAF = forecast(M3.DTLA,h=length(DTLA.y.test))

autoplot(M3.DTLAF,PI=F)+autolayer(M3.DTLAF$fitted)

data.frame(Time = 1:length(as.numeric(DTLA.y.test)),
           DTLA.y.test = as.numeric(DTLA.y.test),
           M3.DTLA.prediction = as.numeric(M3.DTLAF[["mean"]])) %>% slice(1:(7*24)+1000) %>% 
  ggplot(aes(x=Time,y=DTLA.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M3.DTLA.prediction),col="red")+theme_bw()

smape(as.numeric(DTLA.y.test),as.numeric(M3.DTLAF[["mean"]]))

#sMAPE: 0.63

######## Westside

M3.Westside = nnetar(Westside.y.train,MaxNWts=1153)

M3.WestsideF = forecast(M3.Westside,h=length(Westside.y.test))

autoplot(M3.WestsideF,PI=F)+autolayer(M3.WestsideF$fitted)

data.frame(Time = 1:length(as.numeric(Westside.y.test)),
           Westside.y.test = as.numeric(Westside.y.test),
           M3.Westside.prediction = as.numeric(M3.WestsideF[["mean"]])) %>% slice(1:(7*24)+1000) %>% 
  ggplot(aes(x=Time,y=Westside.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M3.Westside.prediction),col="red")+theme_bw()

smape(as.numeric(Westside.y.test),as.numeric(M3.WestsideF[["mean"]]))

#sMAPE: 1.38

######## North Hollywood

M3.NorthHollywood = nnetar(NorthHollywood.y.train)

M3.NorthHollywoodF = forecast(M3.NorthHollywood,h=length(NorthHollywood.y.test))

autoplot(M3.NorthHollywoodF,PI=F)+autolayer(M3.NorthHollywoodF$fitted)

data.frame(Time = 1:length(as.numeric(NorthHollywood.y.test)),
           NorthHollywood.y.test = as.numeric(NorthHollywood.y.test),
           M3.NorthHollywood.prediction = as.numeric(M3.NorthHollywoodF[["mean"]])) %>% slice(1:(7*24)+1000) %>% 
  ggplot(aes(x=Time,y=NorthHollywood.y.test))+geom_line()+
  geom_line(aes(x=Time,y=M3.NorthHollywood.prediction),col="red")+theme_bw()

smape(as.numeric(NorthHollywood.y.test),as.numeric(M3.NorthHollywoodF[["mean"]]))

#sMAPE: 1.93

##################################################### Linear Regression ####################################################

######## Aggregate (Total)

total_hourly_demand$train_demand = c(total_hourly_demand$actual_demand[1:length(total.y.train)], rep(NA,length(total.y.test)))

total_hourly_demand$HourOfDay = factor(c(rep(1:24, 45884/24), 1:20))
total_hourly_demand$DoW = factor(wday(total_hourly_demand$hour,label=TRUE))
total_hourly_demand$Month = factor(month(total_hourly_demand$hour,label=TRUE))
total_hourly_demand$Trend = (1:45884)
total_hourly_demand$demandLag1 = lag(total_hourly_demand$train_demand,1)
total_hourly_demand$demandLag2 = lag(total_hourly_demand$train_demand,2)
total_hourly_demand$demandLag24 = lag(total_hourly_demand$train_demand,24)
total_hourly_demand$demandLag168 = lag(total_hourly_demand$train_demand,168)

total_hourly_demand %>% tail(30)
glimpse(total_hourly_demand)

M4.total = lm(train_demand ~ HourOfDay + DoW + Month+ Trend+
              demandLag1 + demandLag2 + demandLag24 + demandLag168 +
              DoW:Month + HourOfDay:Month + DoW:demandLag24,
              data= total_hourly_demand)

total_hourly_demand$M4.total = predict(M4.total,newdata=total_hourly_demand)

for(i in (length(total.y.train):dim(total_hourly_demand)[1])){
  total_hourly_demand[i,"demandLag1"]=ifelse(is.na(total_hourly_demand[i-1,"train_demand"]),total_hourly_demand[i-1,"M4.total"],total_hourly_demand[i-1,"train_demand"])
  total_hourly_demand[i,"demandLag2"]=ifelse(is.na(total_hourly_demand[i-2,"train_demand"]),total_hourly_demand[i-2,"M4.total"],total_hourly_demand[i-2,"train_demand"])
  total_hourly_demand[i,"demandLag24"]=ifelse(is.na(total_hourly_demand[i-24,"train_demand"]),total_hourly_demand[i-24,"M4.total"],total_hourly_demand[i-24,"train_demand"])
  total_hourly_demand[i,"demandLag168"]=ifelse(is.na(total_hourly_demand[i-168,"train_demand"]),total_hourly_demand[i-168,"M4.total"],total_hourly_demand[i-168,"train_demand"])
  total_hourly_demand[i,"M4.total"]=predict(M4.total,total_hourly_demand[i,])
}

total_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand)) +geom_line() + geom_line(aes(x=hour,y=M4.total),col="blue") + theme_bw()

total_hourly_demand %>% slice(length(total.y.train):dim(total_hourly_demand)[1]) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.total),col="red")+theme_bw()

total_hourly_demand %>% slice(length(total.y.train):(length(total.y.train)+(7*24))+200) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.total),col="red")+theme_bw()

smape(total_hourly_demand$actual_demand[(length(total.y.train)+1):dim(total_hourly_demand)[1]],
      total_hourly_demand$M4.total[(length(total.y.train)+1):dim(total_hourly_demand)[1]])

#sMAPE: 0.52

######## DTLA

DTLA_hourly_demand$train_demand = c(DTLA_hourly_demand$actual_demand[1:length(DTLA.y.train)], rep(NA,length(DTLA.y.test)))

DTLA_hourly_demand$HourOfDay = factor(c(rep(1:24, 45884/24), 1:20))
DTLA_hourly_demand$DoW = factor(wday(DTLA_hourly_demand$hour,label=TRUE))
DTLA_hourly_demand$Month = factor(month(DTLA_hourly_demand$hour,label=TRUE))
DTLA_hourly_demand$Trend = (1:45884)
DTLA_hourly_demand$demandLag1 = lag(DTLA_hourly_demand$train_demand,1)
DTLA_hourly_demand$demandLag2 = lag(DTLA_hourly_demand$train_demand,2)
DTLA_hourly_demand$demandLag24 = lag(DTLA_hourly_demand$train_demand,24)
DTLA_hourly_demand$demandLag168 = lag(DTLA_hourly_demand$train_demand,168)

DTLA_hourly_demand %>% tail(30)
glimpse(DTLA_hourly_demand)

M4.DTLA = lm(train_demand ~ HourOfDay + DoW + Month+ Trend+
                demandLag1 + demandLag2 + demandLag24 + demandLag168 +
                DoW:Month + HourOfDay:Month + DoW:demandLag24,
                data= DTLA_hourly_demand)

DTLA_hourly_demand$M4.DTLA = predict(M4.DTLA,newdata=DTLA_hourly_demand)

for(i in (length(DTLA.y.train):dim(DTLA_hourly_demand)[1])){
  DTLA_hourly_demand[i,"demandLag1"]=ifelse(is.na(DTLA_hourly_demand[i-1,"train_demand"]),DTLA_hourly_demand[i-1,"M4.DTLA"],DTLA_hourly_demand[i-1,"train_demand"])
  DTLA_hourly_demand[i,"demandLag2"]=ifelse(is.na(DTLA_hourly_demand[i-2,"train_demand"]),DTLA_hourly_demand[i-2,"M4.DTLA"],DTLA_hourly_demand[i-2,"train_demand"])
  DTLA_hourly_demand[i,"demandLag24"]=ifelse(is.na(DTLA_hourly_demand[i-24,"train_demand"]),DTLA_hourly_demand[i-24,"M4.DTLA"],DTLA_hourly_demand[i-24,"train_demand"])
  DTLA_hourly_demand[i,"demandLag168"]=ifelse(is.na(DTLA_hourly_demand[i-168,"train_demand"]),DTLA_hourly_demand[i-168,"M4.DTLA"],DTLA_hourly_demand[i-168,"train_demand"])
  DTLA_hourly_demand[i,"M4.DTLA"]=predict(M4.DTLA,DTLA_hourly_demand[i,])
}

DTLA_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand)) +geom_line() + geom_line(aes(x=hour,y=M4.DTLA),col="blue") + theme_bw()

DTLA_hourly_demand %>% slice(length(DTLA.y.train):dim(DTLA_hourly_demand)[1]) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.DTLA),col="red")+theme_bw()

DTLA_hourly_demand %>% slice(length(DTLA.y.train):(length(DTLA.y.train)+(7*24))+200) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.DTLA),col="red")+theme_bw()

smape(DTLA_hourly_demand$actual_demand[(length(DTLA.y.train)+1):dim(DTLA_hourly_demand)[1]],
      DTLA_hourly_demand$M4.DTLA[(length(DTLA.y.train)+1):dim(DTLA_hourly_demand)[1]])

#sMAPE: 0.72

######## Westside

Westside_hourly_demand$actual_demand[Westside_hourly_demand$hour < ymd_hm("2017-09-01 00:00")]=NA

Westside_hourly_demand$train_demand = c(Westside_hourly_demand$actual_demand[1:43676], rep(NA,length(Westside.y.test)))

Westside_hourly_demand$HourOfDay = factor(c(rep(1:24, 45884/24), 1:20))
Westside_hourly_demand$DoW = factor(wday(Westside_hourly_demand$hour,label=TRUE))
Westside_hourly_demand$Month = factor(month(Westside_hourly_demand$hour,label=TRUE))
Westside_hourly_demand$Trend = (1:45884)
Westside_hourly_demand$demandLag1 = lag(Westside_hourly_demand$train_demand,1)
Westside_hourly_demand$demandLag2 = lag(Westside_hourly_demand$train_demand,2)
Westside_hourly_demand$demandLag24 = lag(Westside_hourly_demand$train_demand,24)
Westside_hourly_demand$demandLag168 = lag(Westside_hourly_demand$train_demand,168)

Westside_hourly_demand %>% tail(30)
glimpse(Westside_hourly_demand)

M4.Westside = lm(train_demand ~ HourOfDay + DoW + Month+ Trend+
               demandLag1 + demandLag2 + demandLag24 + demandLag168 +
               DoW:Month + HourOfDay:Month + DoW:demandLag24,
             data= Westside_hourly_demand)

Westside_hourly_demand$M4.Westside = predict(M4.Westside,newdata=Westside_hourly_demand)

for(i in (43676:dim(Westside_hourly_demand)[1])){
  Westside_hourly_demand[i,"demandLag1"]=ifelse(is.na(Westside_hourly_demand[i-1,"train_demand"]),Westside_hourly_demand[i-1,"M4.Westside"],Westside_hourly_demand[i-1,"train_demand"])
  Westside_hourly_demand[i,"demandLag2"]=ifelse(is.na(Westside_hourly_demand[i-2,"train_demand"]),Westside_hourly_demand[i-2,"M4.Westside"],Westside_hourly_demand[i-2,"train_demand"])
  Westside_hourly_demand[i,"demandLag24"]=ifelse(is.na(Westside_hourly_demand[i-24,"train_demand"]),Westside_hourly_demand[i-24,"M4.Westside"],Westside_hourly_demand[i-24,"train_demand"])
  Westside_hourly_demand[i,"demandLag168"]=ifelse(is.na(Westside_hourly_demand[i-168,"train_demand"]),Westside_hourly_demand[i-168,"M4.Westside"],Westside_hourly_demand[i-168,"train_demand"])
  Westside_hourly_demand[i,"M4.Westside"]=predict(M4.Westside,Westside_hourly_demand[i,])
}

Westside_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand)) +geom_line() + geom_line(aes(x=hour,y=M4.Westside),col="blue") + theme_bw()

Westside_hourly_demand %>% slice(length(Westside.y.train):dim(Westside_hourly_demand)[1]) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.Westside),col="red")+theme_bw()

Westside_hourly_demand %>% slice((43676+1):((43676+1)+(7*24))+200) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.Westside),col="red")+theme_bw()

smape(Westside_hourly_demand$actual_demand[(43676+1):dim(Westside_hourly_demand)[1]],
      Westside_hourly_demand$M4.Westside[(43676+1):dim(Westside_hourly_demand)[1]])

#sMAPE: 0.97

######## North Hollywood

NorthHollywood_hourly_demand$actual_demand[NorthHollywood_hourly_demand$hour < ymd_hm("2019-08-05 00:00")]=NA

NorthHollywood_hourly_demand$train_demand = c(NorthHollywood_hourly_demand$actual_demand[1:43676], rep(NA,length(NorthHollywood.y.test)))

NorthHollywood_hourly_demand$HourOfDay = factor(c(rep(1:24, 45884/24), 1:20))
NorthHollywood_hourly_demand$DoW = factor(wday(NorthHollywood_hourly_demand$hour,label=TRUE))
NorthHollywood_hourly_demand$Month = factor(month(NorthHollywood_hourly_demand$hour,label=TRUE))
NorthHollywood_hourly_demand$Trend = (1:45884)
NorthHollywood_hourly_demand$demandLag1 = lag(NorthHollywood_hourly_demand$train_demand,1)
NorthHollywood_hourly_demand$demandLag2 = lag(NorthHollywood_hourly_demand$train_demand,2)
NorthHollywood_hourly_demand$demandLag24 = lag(NorthHollywood_hourly_demand$train_demand,24)
NorthHollywood_hourly_demand$demandLag168 = lag(NorthHollywood_hourly_demand$train_demand,168)

NorthHollywood_hourly_demand %>% tail(30)
glimpse(NorthHollywood_hourly_demand)

M4.NorthHollywood = lm(train_demand ~ HourOfDay + DoW + Month+ Trend+
               demandLag1 + demandLag2 + demandLag24 + demandLag168 +
               DoW:Month + HourOfDay:Month + DoW:demandLag24,
             data= NorthHollywood_hourly_demand)

NorthHollywood_hourly_demand$M4.NorthHollywood = predict(M4.NorthHollywood,newdata=NorthHollywood_hourly_demand)

for(i in (43676:dim(NorthHollywood_hourly_demand)[1])){
  NorthHollywood_hourly_demand[i,"demandLag1"]=ifelse(is.na(NorthHollywood_hourly_demand[i-1,"train_demand"]),NorthHollywood_hourly_demand[i-1,"M4.NorthHollywood"],NorthHollywood_hourly_demand[i-1,"train_demand"])
  NorthHollywood_hourly_demand[i,"demandLag2"]=ifelse(is.na(NorthHollywood_hourly_demand[i-2,"train_demand"]),NorthHollywood_hourly_demand[i-2,"M4.NorthHollywood"],NorthHollywood_hourly_demand[i-2,"train_demand"])
  NorthHollywood_hourly_demand[i,"demandLag24"]=ifelse(is.na(NorthHollywood_hourly_demand[i-24,"train_demand"]),NorthHollywood_hourly_demand[i-24,"M4.NorthHollywood"],NorthHollywood_hourly_demand[i-24,"train_demand"])
  NorthHollywood_hourly_demand[i,"demandLag168"]=ifelse(is.na(NorthHollywood_hourly_demand[i-168,"train_demand"]),NorthHollywood_hourly_demand[i-168,"M4.NorthHollywood"],NorthHollywood_hourly_demand[i-168,"train_demand"])
  NorthHollywood_hourly_demand[i,"M4.NorthHollywood"]=predict(M4.NorthHollywood,NorthHollywood_hourly_demand[i,])
}

NorthHollywood_hourly_demand %>% ggplot(aes(x=hour,y=actual_demand)) +geom_line() + geom_line(aes(x=hour,y=M4.NorthHollywood),col="blue") + theme_bw()

NorthHollywood_hourly_demand %>% slice(43676:dim(NorthHollywood_hourly_demand)[1]) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.NorthHollywood),col="red")+theme_bw()

NorthHollywood_hourly_demand %>% slice(43676:(43676+(7*24))) %>% 
  ggplot(aes(x=hour,y=actual_demand))+geom_line()+
  geom_line(aes(x=hour,y=M4.NorthHollywood),col="red")+theme_bw()

smape(NorthHollywood_hourly_demand$actual_demand[(43676+1):dim(NorthHollywood_hourly_demand)[1]],
      NorthHollywood_hourly_demand$M4.NorthHollywood[(43676+1):dim(NorthHollywood_hourly_demand)[1]])

#sMAPE: 1.93

##################################################### Prophet ####################################################
#NOTE:
#I decided to run the prophet model on just the aggregate demand just to see if it produces anything as good as the other models.
#This was just an experiment because prophet is meant for predicting daily growing revenue.

total.prophet = total_hourly_demand

total.prophet = subset(total.prophet, select = -c(actual_demand,HourOfDay,DoW,Month,Trend,
                                                  demandLag1,demandLag2,demandLag24,demandLag168,M4.total))

colnames(total.prophet) = c("ds","y")

M5 = prophet(daily.seasonality = "auto", weekly.seasonality = "auto", yearly.seasonality = "auto")

M5 = fit.prophet(M5,total.prophet)

fitted_prophet = predict(M5,total.prophet)

plot(M5,fitted_prophet)
prophet_plot_components(M5,fitted_prophet)

prophet_forecast = data.frame("Prediction"=fitted_prophet$yhat)

smape(total_hourly_demand$actual_demand[(length(total.y.train)+1):dim(total_hourly_demand)[1]],
      fitted_prophet$yhat[(length(total.y.train)+1):dim(total_hourly_demand)[1]])

#sMAPE: 0.51

##################################################### lm Using Daily Data #########################################################

daily.total = total_hourly_demand %>% group_by(day = floor_date(hour,"1 day")) %>% summarize(actual_demand=sum(actual_demand),train_demand = sum(train_demand))

which(daily.total$actual_demand == 0)  

daily.total$DoW = factor(wday(daily.total$day,label=TRUE))
daily.total$Month = factor(month(daily.total$day,label=TRUE))
daily.total$Trend = (1:dim(daily.total)[1])
daily.total$demandLag1 = lag(daily.total$train_demand,1)
daily.total$demandLag2 = lag(daily.total$train_demand,2)
daily.total$demandLag6 = lag(daily.total$train_demand,6)
daily.total$demandLag7 = lag(daily.total$train_demand,7)

daily.total %>% tail(30)
glimpse(daily.total)

M6.total = lm(train_demand ~ DoW + Month+
                demandLag7 +
                DoW:Month,
              data= daily.total)

daily.total$M6.total = predict(M6.total,newdata=daily.total)

for(i in (1821:dim(daily.total)[1])){
  daily.total[i,"demandLag1"]=ifelse(is.na(daily.total[i-1,"train_demand"]),daily.total[i-1,"M6.total"],daily.total[i-1,"train_demand"])
  daily.total[i,"demandLag6"]=ifelse(is.na(daily.total[i-6,"train_demand"]),daily.total[i-6,"M6.total"],daily.total[i-6,"train_demand"])
  daily.total[i,"demandLag7"]=ifelse(is.na(daily.total[i-7,"train_demand"]),daily.total[i-7,"M6.total"],daily.total[i-7,"train_demand"])
  daily.total[i,"M6.total"]=predict(M6.total,daily.total[i,])
}

daily.total %>% ggplot(aes(x=day,y=actual_demand)) +geom_line() + geom_line(aes(x=day,y=M6.total),col="blue") + theme_bw()

daily.total %>% slice(1821:dim(daily.total)[1]) %>% 
  ggplot(aes(x=day,y=actual_demand))+geom_line()+
  geom_line(aes(x=day,y=M6.total),col="red")+theme_bw()

daily.total %>% slice(1821:(1821+7)+20) %>% 
  ggplot(aes(x=day,y=actual_demand))+geom_line()+
  geom_line(aes(x=day,y=M6.total),col="red")+theme_bw()

smape(daily.total$actual_demand[(length(total.y.train)+1):dim(total_hourly_demand)[1]],
      total_hourly_demand$M4.total[(length(total.y.train)+1):dim(total_hourly_demand)[1]])

#MAPE on Testing:

mean(abs((daily.total$actual_demand[1821:dim(daily.total)[1]] - 
            daily.total$M6.total[1821:dim(daily.total)[1]]) / 
           daily.total$actual_demand[1821:dim(daily.total)[1]]))*100

#19.19%

tsdisplay(M6.total$residuals)

##################################################### Prophet Using Daily Data #########################################################

daily.total.prophet = total_hourly_demand %>% group_by(ds = floor_date(hour,"1 day")) %>% summarize(y = sum(train_demand))

M7 = prophet()

M7 = fit.prophet(M7,daily.total.prophet)

daily_fitted_prophet = predict(M7,daily.total.prophet)

plot(M7,daily_fitted_prophet)
prophet_plot_components(M7,daily_fitted_prophet)

#MAPE on Testing:

mean(abs((daily.total$actual_demand[1821:dim(daily.total)[1]] - 
            daily_fitted_prophet$yhat[1821:dim(daily.total)[1]]) / 
           daily.total$actual_demand[1821:dim(daily.total)[1]]))*100

#20.05%

##################################################### Hourly Model Selection ####################################################

#Aggregate (Total): Lowest sMAPE: Neural Network (0.41)

#DTLA: Lowest sMAPE: ARIMA (0.60)

#Westside: Lowest sMAPE: Exponential Smoothing (0.77)

#North Hollywood: Lowest sMAPE: Linear Regression (1.93)

##################################################### Reflection #########################################################

#It appears that our more complicated models excelled for data sets where there was more data given. For the total and DTLA
#regions, the Neural Network and ARIMA models performed better because there was the most amount of data possible. For the
#regions where there was less data, exponential smoothing and linear regression did better, potentially because they were less
#overfit. The accuracy on the last two models are lower also because when there are less bike rides to model the data off of,
#it is more difficult to predict reliable patterns.

######### Some things to consider for future research:

#Changing the testing set could reveal which models are overfit, or those whose predictive ability decreases more sharply over
#time. One quarter was chosen as the testing set time because the data set was somewhat small, but particularly small for
#certain regions.

#We attempted to use covariates in our models, but the variable we wanted to find we were unable to. We could not find hourly 
#temperature data for these years, as we could only find 2010 and earlier. Some other covariates we would use as regressors
#would potentially be air quality, rain, or holidays (especially local LA events such as parades and marathons). For the North
#Hollywood region this would have been particularly useful as we had to remove the yearly seasonality since there was not
#enough data to model it. Temperature would be a great regressor to replace this missing data as temperature is heavily related
#to time of the year.

#There seems to be some sort of intervention for the Westside in late 2020. Although not very much data can be used to model
#after this time, it might produce a more accurate result if it was. Our best guess is that more bikes were put in the Westside
#at this time which dramatically increased the number of rides.
