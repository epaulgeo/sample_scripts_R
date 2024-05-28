# This script reads in streamflow.only files and finds the annual maximum flows (AMF) within user-defined normals.
# It then ranks the flows and outputs .csv and RDS files with columns for: global climate model forcing, AMF (in m^3/s), normal, rank, and datetime
# This script was designed to be used in concert with the scripts "Basin_Avg_FG_Mechs.R" and "Bind_Basin_FG_Mech_to_Flows.R" to 
# associate AMFs with flow generating (FG) mechanisms.
# Created by Evan Paul at WWU, last modified May 2022

library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(RcppRoll)

rm(list=ls())
cat("\014") #clears console

wd <- "D:\\projected_sims\\NF"
setwd(wd)

#fd<-c(1, 3, 24, 72, 168) #set flow durations of interest (hours)

fd<-c(24) #set flow durations of interest (hours)

for (d in fd) { #loops through each position in the vector above 
  
  Save_points<-c("NF")
  
  for (g in unique(Save_points)) {
    
    #select the DHSVM output location you're interested in (must match column name in DHSVM output file exactly):
    gauge <- g
    
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
    # ACCESS1.0<-as.data.table(read.table("access1.0.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # ACCESS1.0[,model:="ACCESS1.0"]
    # ACCESS1.3<-as.data.table(read.table("access1.3.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # ACCESS1.3[,model:="ACCESS1.3"]
    # BCC_CSM1.1<-as.data.table(read.table("bcc-csm1.1.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # BCC_CSM1.1[,model:="BCC_CSM1.1"]
    # CanESM2<-as.data.table(read.table("canesm2.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # CanESM2[,model:="CanESM2"]
    # CCSM4<-as.data.table(read.table("ccsm4.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # CCSM4[,model:="CCSM4"]
    # CSIRO_Mk3.6.0<-as.data.table(read.table("csiro-mk3.6.0.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # CSIRO_Mk3.6.0[,model:="CSIRO_Mk3.6.0"]
    # FGOALS_g2<-as.data.table(read.table("fgoals-g2.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # FGOALS_g2[,model:="FGOALS_g2"]
    # GFDL_CM3<-as.data.table(read.table("gfdl-cm3.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # GFDL_CM3[,model:="GFDL_CM3"]
    # GISS_E2_H<-as.data.table(read.table("giss-e2-h.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # GISS_E2_H[,model:="GISS_E2_H"]
    # MIROC5<-as.data.table(read.table("miroc5.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # MIROC5[,model:="MIROC5"]
    # MRI_CGCM3<-as.data.table(read.table("mri-cgcm3.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # MRI_CGCM3[,model:="MRI_CGCM3"]
    # NorESM1_M<-as.data.table(read.table("noresm1-m.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # NorESM1_M[,model:="NorESM1_M"]
    # 
    # l=list(ACCESS1.0, ACCESS1.3, BCC_CSM1.1, CanESM2, CCSM4, CSIRO_Mk3.6.0, FGOALS_g2, GFDL_CM3, GISS_E2_H, MIROC5, MRI_CGCM3, NorESM1_M)
    
    access1.0<-as.data.table(read.table("access1.0.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    access1.0[,model:="access1.0"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("access1.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      access1.0 <- access1.0[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("access1.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      access1.0 <- access1.0[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    access1.3<-as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    access1.3[,model:="access1.3"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      access1.3 <- access1.3[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      access1.3 <- access1.3[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    bcc_csm1.1<-as.data.table(read.table("bcc-csm1.1.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    bcc_csm1.1[,model:="bcc_csm1.1"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("bcc-csm1.1.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      bcc_csm1.1 <- bcc_csm1.1[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("bcc-csm1.1.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      bcc_csm1.1 <- bcc_csm1.1[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    canesm2<-as.data.table(read.table("canesm2.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    canesm2[,model:="canesm2"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("canesm2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      canesm2 <- canesm2[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("canesm2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      canesm2 <- canesm2[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    ccsm4<-as.data.table(read.table("ccsm4.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    ccsm4[,model:="ccsm4"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("ccsm4.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      ccsm4 <- ccsm4[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("ccsm4.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      ccsm4 <- ccsm4[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    csiro_mk3.6.0<-as.data.table(read.table("csiro-mk3.6.0.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    csiro_mk3.6.0[,model:="csiro_mk3.6.0"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("csiro-mk3.6.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      csiro_mk3.6.0 <- csiro_mk3.6.0[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("csiro-mk3.6.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      csiro_mk3.6.0 <- csiro_mk3.6.0[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    fgoals_g2<-as.data.table(read.table("fgoals-g2.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    fgoals_g2[,model:="fgoals_g2"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("fgoals-g2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      fgoals_g2 <- fgoals_g2[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("fgoals-g2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      fgoals_g2 <- fgoals_g2[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    gfdl_cm3<-as.data.table(read.table("gfdl-cm3.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    gfdl_cm3[,model:="gfdl_cm3"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("gfdl-cm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      gfdl_cm3 <- gfdl_cm3[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("gfdl-cm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      gfdl_cm3 <- gfdl_cm3[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    giss_e2_h<-as.data.table(read.table("giss-e2-h.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    giss_e2_h[,model:="giss_e2_h"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("giss-e2-h.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      giss_e2_h <- giss_e2_h[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("giss-e2-h.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      giss_e2_h <- giss_e2_h[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    miroc5<-as.data.table(read.table("miroc5.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    miroc5[,model:="miroc5"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("miroc5.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      miroc5 <- miroc5[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("miroc5.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      miroc5 <- miroc5[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    mri_cgcm3<-as.data.table(read.table("mri-cgcm3.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    mri_cgcm3[,model:="mri_cgcm3"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("mri-cgcm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      mri_cgcm3 <- mri_cgcm3[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("mri-cgcm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      mri_cgcm3 <- mri_cgcm3[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    noresm1<-as.data.table(read.table("noresm1-m.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    noresm1[,model:="noresm1"]
    if (gauge == 'CEDARVILLE'){
      setwd("D:\\projected_sims\\MF")
      MF_flow <- as.data.table(read.table("noresm1-m.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
      noresm1 <- noresm1[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
      
      setwd("D:\\projected_sims\\SF")
      SF_flow <- as.data.table(read.table("noresm1-m.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
      noresm1 <- noresm1[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
    }
    
    setwd(wd)
    
    l=list(access1.0, access1.3, bcc_csm1.1, canesm2, ccsm4, csiro_mk3.6.0, fgoals_g2, gfdl_cm3, giss_e2_h, miroc5, mri_cgcm3, noresm1)
    
#make bound object lp for longprojections
    
lp<- rbindlist(l, use.names = TRUE, fill = TRUE)
    
#rename gauge column flow
    
colnames(lp)[2]<-"flow"

lp$flow<- lp$flow*(1/3600) #convert from DHSVM units (m^3/hr) to m^3/s
    
lp[,datatime:=as.POSIXct(DATE, format= "%m.%d.%Y-%H:%M:%S", tz = "UTC")]

lp[,month:=month(datatime)]
    
lp[,year:=year(datatime)]

lp[,wateryear:= ifelse(month>=10, year+1, year)] #adds column for water year
    
#filter by date and create new objects for normals 1 and 2 defined above
lp[datatime >= startnormal1 & datatime <= endnormal1, normal :=normal1name]
lp[datatime >= startnormal2 & datatime <= endnormal2, normal :=normal2name]
lp[datatime >= startnormal3 & datatime <= endnormal3, normal :=normal3name]
    
normals<-c(normal1name, normal2name, normal3name)
    
lp<-lp %>%
  filter(normal %in% normals)

#build rolling avg flow in a new column called roll_avg_flow, arranged by model, normal, and date and grouped by model and normal
lproll<-lp %>%
  arrange(model, normal, datatime) %>%
  group_by(model, normal) %>%
  mutate(roll_avg_flow = roll_sum(flow, d, align = c("right"), fill = NA, partial = FALSE)/d) #by aligning right, the avg flow listed corresponds to the prior period
    
#select the annual flow maxima for each model/normal/wateryear, arrange by flow (max to min)
Ann_max<-lproll %>%
  group_by(model, normal, wateryear) %>% top_n(1, roll_avg_flow) %>%
  arrange(model, normal, -roll_avg_flow)
    
#add rank number to each row, from highest to lowest flows, they get 1:n
Ann_max<-Ann_max %>%
  group_by(model, normal) %>%
  dplyr::mutate(rank_num = 1:n())

#select columns of interest and reorder them as described above
AMPF<- Ann_max %>%
  select(model, roll_avg_flow, normal, rank_num, datatime)

#output a .csv with columns for: global climate model forcing, AMF, normal, rank, date, time
write.csv(AMPF, file = paste0(g,"_", d,"_hr_flows_AMF.csv"), row.names = FALSE)

#output AMF as an RDS file 
saveRDS(AMPF, file =paste0(g,"_", d,"_hr_flows_AMF"))

  }
}
