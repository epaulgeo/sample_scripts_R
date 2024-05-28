# This script rolls through a flood generating (FG) mechanism decision tree using a DHSVM-generated Aggregated.Values file. 
# The possible FG mechanisms are: Extreme Precipitation Event, Rain on Snow (ROS) event, or Snowmelt, or other.
# Annual maximum peak flows (AMPF) occuring within 4 days of a precipitation event greater than or equal to the 99% percentile in a given year are extreme precip events. 
# AMPF mech is ROS if basin-avg SWE > 0.1 m at the time of a rain event (if Tair >= 1 °C) greater than 0.1 m in depth in the 4 days prior to an AMPF AND snowmelt accounted for > 0.2 of the sum of precip and snowmelt in the period. 
# if AMPF FG qualifies as both Extreme Precip and ROS, it should be qualified as ROS
# AMPF FG mech is Snowmelt if basin-avg SWE > 0.1 m and snowpack reduced by at least 10% in the week prior to the AMPF but the AMPF did not qualify as an extreme precip or ROS event.
# AMPF FG mech is only Snowmelt if it did not qualify as extreme precip or ROS event

# The output file from this script includes .RDS and .csv files with the following columns:
# model (GCM), normal (user defined), datatime (POSIXct YYYY-MM-DD hh:mm:ss) `Precip(m)` (precip depth in m), 
# Swq (SWE in m), Melt (melt in m/ timestep), Tair (air temp °C), roll_1d_precip (precedent 24-hr precip [m]), 
# p99 (99th percentile of daily precip for a given model-year), EP_event_1d (1 if 24-hr precip is >= 99th percentile, else 0),
# EP_event (4-day rolled sum for EP_event_1d [>=1 if qualifying EP event timestep]), roll_4d_precip (precedant 4-day total precip in m),
# roll_4d_melt (precedant 4-day total melt in m), prop_melt (melt proportion of 4 day melt+ 4 day precip), qual_ROS_12 (1 if SWE >=0.1 m at time of rain event [Tair >= 1° C] greater than 0.1 m in single day) else 0), 
# qual_ROS_12_roll_4d (4-day rolled sum of prior col (>= 1 if qualifying as ROS, else 0)), ROS_event (1 if >=1 in prior column and prop_melt >= 0.2), 
# Swe_7d_lag (SWE (m) lagged by 7 days [SWE in basin 7-days earlier]), SWE_7d_change ([Present SWE minus 7-dprior SWE] / 7-d prior SWE), Snwmlt_event (1 if SWE_7d_change >= 0.1, else 0)

# The purpose of this file is to to qualify each timestep's respective FG mech based on precedent watershed characteristics
# Each row in the joined file will provide the FGM for the related AMPF
# Created by Evan Paul at WWU, last modified May 2022

library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(RcppRoll)

rm(list=ls())

wd <- "D:\\projected_sims\\SF"
snow_threshold <- 1.5

setwd(wd)

#select the columns of interest (must match column name in Aggregated.Values file exactly):
Cols <- c("Date", "Precip(m)", "Swq", "Tair")

#start date and end date of normals
startnormal1<-'1980-10-01 00:00'
endnormal1<-'2010-09-30 23:00'
normal1name<- "1990s"

startnormal2<-'2039-10-01 00:00'
endnormal2<-'2069-09-30 23:00'
normal2name<- "2050s"

startnormal3<-'2069-10-01 00:00'
endnormal3<-'2099-09-30 23:00'
normal3name<- "2080s"

#pull hourly timeseries for the above gauge from all projections and append a column with the model name
ACCESS1.0<-as.data.table(read.table("access1.0.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
ACCESS1.0[,model:="access1.0"]
ACCESS1.3<-as.data.table(read.table("access1.3.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
ACCESS1.3[,model:="access1.3"]
BCC_CSM1.1<-as.data.table(read.table("bcc-csm1.1.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
BCC_CSM1.1[,model:="bcc_csm1.1"]
CanESM2<-as.data.table(read.table("canesm2.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
CanESM2[,model:="canesm2"]
CCSM4<-as.data.table(read.table("ccsm4.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
CCSM4[,model:="ccsm4"]
CSIRO_Mk3.6.0<-as.data.table(read.table("csiro-mk3.6.0.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
CSIRO_Mk3.6.0[,model:="csiro_mk3.6.0"]
FGOALS_g2<-as.data.table(read.table("fgoals-g2.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
FGOALS_g2[,model:="fgoals_g2"]
GFDL_CM3<-as.data.table(read.table("gfdl-cm3.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
GFDL_CM3[,model:="gfdl_cm3"]
GISS_E2_H<-as.data.table(read.table("giss-e2-h.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
GISS_E2_H[,model:="giss_e2_h"]
MIROC5<-as.data.table(read.table("miroc5.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
MIROC5[,model:="miroc5"]
MRI_CGCM3<-as.data.table(read.table("mri-cgcm3.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
MRI_CGCM3[,model:="mri_cgcm3"]
NorESM1_M<-as.data.table(read.table("noresm1-m.SFSNOTEL",sep = "", header=T, fill=TRUE, check.names = F)[,Cols]) #pull time series index and data columns of DHSVM output
NorESM1_M[,model:="noresm1"]

l=list(ACCESS1.0, ACCESS1.3, BCC_CSM1.1, CanESM2, CCSM4, CSIRO_Mk3.6.0, FGOALS_g2, GFDL_CM3, GISS_E2_H, MIROC5, MRI_CGCM3, NorESM1_M)

#make bound object lp for longprojections
lp<- rbindlist(l, use.names = TRUE, fill = TRUE)

lp[,datatime:=as.POSIXct(Date, format= "%m/%d/%Y-%H:%M:%S", tz = "UTC")] #add datatime column

lp[,year:=year(datatime)]

#used for SNOTEL files; creates Melt column by subtracting Swq from previous timestep
lp <- lp %>%
  group_by(model) %>%
  mutate(Melt = Swq - lag(Swq, default = first(Swq)))

setDT(lp)

#filter by date and assign normal names
lp[datatime >= startnormal1 & datatime <= endnormal1, normal :=normal1name]
lp[datatime >= startnormal2 & datatime <= endnormal2, normal :=normal2name]
lp[datatime >= startnormal3 & datatime <= endnormal3, normal :=normal3name]

normals<-c(normal1name, normal2name, normal3name)

lp<-lp %>%
  filter(normal %in% normals)

#compute FG threshold values arranged sequentially by datatime and grouped by model and normal
lproll<-lp %>%
  arrange(model, normal, datatime) %>%
  group_by(model, normal) %>%
  #1-day precip rolled aligned right (for Extreme precip logic)
  mutate(roll_1d_precip = roll_sum(`Precip(m)`, 24, align = c("right"), fill = NA, partial = FALSE)) %>% #by aligning right, the rolled value corresponds to the prior period; 96 corresponds to 96 hrly timesteps in 4 days
  #1-day precip rolled aligned left (for ROS logic -> qualifying precip amount to correspond to SWE at start of rain event)
  mutate(roll_1d_precip_l = roll_sum(`Precip(m)`, 24, align = c("left"), fill = NA, partial = FALSE)) %>% 
  #4-precip aligned left
  mutate(roll_4d_precip = roll_sum(`Precip(m)`, 96, align = c("right"), fill = NA, partial = FALSE)) %>%
  #4-day melt
  mutate(roll_4d_melt = roll_sum(Melt, 96, align = c("right"), fill = NA, partial = FALSE)) %>%
  #proportion melt
  mutate(prop_melt = (roll_4d_melt/(roll_4d_melt + roll_4d_precip))) %>% #calculates relative proportion of  change in SWE over 7 days
  #7-day lagged SWE
  mutate(Swe_7d_lag = lag(Swq, n=167)) %>% #column of 7 day lagged SWE -> for computing 7-day change in SWE
  #Percent change in SWE over 7 days
  mutate(SWE_7d_change = ((Swq-Swe_7d_lag)/Swe_7d_lag))  #calculates relative change in SWE over prior 7 days
 
#Find 99th percentile of roll_1d_precip for all models/normals/years
Yr_99_Precip <- lproll %>%
  group_by(model, normal, year) %>%
  summarise(p99 = quantile(roll_1d_precip, probs = 0.99, na.rm=T, type = 5))

#Join yearly 1-day 99th percentile precip with lproll by model, normal, year
lproll <- merge(lproll, Yr_99_Precip, by = c("model", "normal", "year"), all.x=TRUE)
setDT(lproll)
lproll <- lproll %>%
  arrange(model, normal, datatime) %>%
  group_by(model, normal) %>%
  mutate(EP_event_1d = if_else(roll_1d_precip >= p99, 1, 0)) %>% #add column checking if 1-day precip exceeds 99th percentile
  mutate(EP_event = roll_sum(EP_event_1d, 96, align = c("right"), fill = NA, partial = FALSE)) %>% # roll 4-day sums -> anything above 0 qualifies as EP event
  mutate_at("roll_1d_precip_l", ~replace(., is.na(.), 0) ) %>% #replace NA values with zero
  #if 24 hr precip > 0.1 and Tair > 1.0, assign row value of 1
  mutate(qual_ROS_12 = if_else(roll_1d_precip_l >= 0.1 & Swq >= 0.1 & Tair >= snow_threshold, 1, 0)) %>%
  mutate(qual_ROS_12_roll_4d = roll_sum(qual_ROS_12, 96, align = c("right"), fill = NA, partial = FALSE)) %>% #roll 1-day precip, SWE and Tair qualifiers into 4-day brackets aligned right to correspond to peak flow times
  #combine ROS qualifiers -> 24 hr precip, Tair, and proportion of melt compared to melt + precip
  mutate(ROS_event = if_else(qual_ROS_12_roll_4d >= 1 & prop_melt >= 0.2, 1, 0)) %>%
  #determine if AMPF is a snowmelt event
  mutate(Snwmlt_event = if_else(Swe_7d_lag >= 0.1 & SWE_7d_change <= -0.1, 1, 0)) #initial SWE > 0.1 m and change in SWE -0.1 or less

#select columns of interest and reorder them as described above
Basin_avg_char<- lproll %>%
  select(model, normal, datatime, `Precip(m)`, Swq, Melt, Tair, roll_1d_precip, p99, EP_event_1d, EP_event, roll_4d_precip, roll_4d_melt, prop_melt, qual_ROS_12, qual_ROS_12_roll_4d, ROS_event, Swe_7d_lag, SWE_7d_change, Snwmlt_event)

#output a .csv with columns for: global climate model forcing, AMPF, normal, rank, date, time
#write.csv(Basin_avg_char, file = paste0("Basin_avg_FG_mech_",normal1name, "_",normal2name, "_",normal3name, ".csv"), row.names = FALSE)

#output AMPF as an RDS file 
saveRDS(Basin_avg_char, file = paste0("Basin_avg_FG_mech_",normal1name, "_",normal2name, "_",normal3name))