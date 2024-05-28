# This script merges annual maximum peak flows (AMPF) at a reach with basin-average watershed characteristics with flow-generating (FG) process columns from a decision tree to
# qualify flows as 1) Extreme Precipitation Events, 2) Rain on Snow (ROS) events, 3) Snowmelt events, or 4) other. The scrip reads in two .RDS files - one of ranked daily average
# flows from the script "Roll_Avg_Annual_Max_Peak_Flows.R and another of basin-avg characteristics from "Basin_Avg_FG_Mechs.R". The output 
# Created by Evan Paul at WWU, last modified May 2022


library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)

rm(list=ls())

basin <- "SF"
wd <- paste0("D:\\projected_sims\\", basin)

setwd(wd)

startnormal1<-'1980-10-01 00:00'
endnormal1<-'2010-09-30 23:00'
normal1name<- "1990s"

startnormal2<-'2039-10-01 00:00'
endnormal2<-'2069-09-30 23:00'
normal2name<- "2050s"

startnormal3<-'2069-10-01 00:00'
endnormal3<-'2099-09-30 23:00'
normal3name<- "2080s"

Basin_avg<- readRDS("Basin_avg_FG_mech_1990s_2050s_2080s", refhook = NULL)

Peak_Flows<- readRDS(paste0(basin, "_24_hr_flows_AMF", refhook = NULL))

#Join yearly peak flow with corresponding datatime row for basin-avg char
AMF_FG_Mechs <- merge(Peak_Flows, Basin_avg, by = c("model", "normal", "datatime"), all.x=FALSE) #eliminate rows with no corresponding AMF

#arrange for easy viewing
AMF_FG_Mechs<- AMF_FG_Mechs %>%
  arrange(model, normal, rank_num, datatime)

#sometimes the script will return AMFs ranked below top 30, remove them
AMF_FG_Mechs<- subset(AMF_FG_Mechs, rank_num < 31 )

#logic: Events are ROS event if they have 1 in column "ROS_Event", Events are EP events if they have >=1 in "EP_evnt" and <1 in "ROS_Event" col, Events are snowmelt events if they have >1 in Snwmelt_event col

#output a .csv with columns for: global climate model forcing, AMF, normal, rank, date, time
write.csv(AMF_FG_Mechs, file = paste0(basin, "_AMF_FG_Mechs_",normal1name, "_",normal2name, "_",normal3name, ".csv"), row.names = FALSE)

#output AMF as an RDS file 
saveRDS(AMF_FG_Mechs, file = paste0(basin, "_AMF_FG_Mechs_",normal1name, "_",normal2name, "_",normal3name))
