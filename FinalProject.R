#To import data use rio package
#https://www.rdocumentation.org/packages/rio/versions/0.5.27
#https://subscription.packtpub.com/book/big-data-and-business-intelligence/9781784391034/1/ch01lvl1sec15/loading-your-data-into-r-with-rio-packages
library(tidyverse)
library(forecast)
library (rio)
library (lubridate)

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

#This will turn the list of rentals into hourly demand

hourly_demand = data.frame(hour = seq(ymd_hm("2016-07-07 4:00"), ymd_hm("2021-09-30 23:00"), by = "hour"))

hourly_demand %>% head()

demand_df = data %>% group_by(hour = floor_date(start_time,"1 hour")) %>% summarize(actual_demand=n())

hourly_demand = left_join(hourly_demand,demand_df,by="hour")

hourly_demand[is.na(hourly_demand)] = 0

rm(demand_data,demand_df)

hourly_demand %>% head(20)
