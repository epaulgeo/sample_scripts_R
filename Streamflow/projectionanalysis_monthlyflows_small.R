# This script reads in streamflow.only files and creates plots of mean monthly discharge by GCM and across a model ensemble.
# The script's DHSVM savepoints (streamflow.only columns), normal periods, aggregating periods (months/days/etc.), and plotting format can be modified significantly.
# Created by Evan Paul at WWU

library(zoo)
library(lubridate)
library(xts)
library(chron)
library(hydroTSM)
library(hydroGOF)
library(plyr)
library(dplyr)
library(dygraphs)
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)

rm(list=ls())

wd <- "D:\\projected_sims\\MF"
setwd(wd)

#name of plot title
ptitle <- 'Middle Fork Gauge'

gauge<- 'MFUSGS'

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
#may want to omit historical wrf data "pnnl"

#pull hourly timeseries for the above gauge from all projections and append a column with the model name
# access1.0<-as.data.table(read.table("access1.0.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# access1.0[,model:="access1.0"]
# access1.3<-as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# access1.3[,model:="access1.3"]
# bcc_csm1.1<-as.data.table(read.table("bcc-csm1.1.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# bcc_csm1.1[,model:="bcc_csm1.1"]
# canesm2<-as.data.table(read.table("canesm2.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# canesm2[,model:="canesm2"]
# ccsm4<-as.data.table(read.table("ccsm4.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# ccsm4[,model:="ccsm4"]
# csiro_mk3.6.0<-as.data.table(read.table("csiro-mk3.6.0.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# csiro_mk3.6.0[,model:="csiro_mk3.6.0"]
# fgoals_g2<-as.data.table(read.table("fgoals-g2.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# fgoals_g2[,model:="fgoals_g2"]
# gfdl_cm3<-as.data.table(read.table("gfdl-cm3.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# gfdl_cm3[,model:="gfdl_cm3"]
# giss_e2_h<-as.data.table(read.table("giss-e2-h.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# giss_e2_h[,model:="giss_e2_h"]
# miroc5<-as.data.table(read.table("miroc5.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# miroc5[,model:="miroc5"]
# mri_cgcm3<-as.data.table(read.table("mri-cgcm3.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# mri_cgcm3[,model:="mri-cgcm3"]
# noresm1<-as.data.table(read.table("noresm1-m.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
# noresm1[,model:="noresm1"]

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

lp[,datatime:=as.POSIXct(DATE, format= "%m.%d.%Y-%H:%M:%S")]

#add columns for month and day
lp[,year:=year(datatime)]
  
lp[,month:=month(datatime)]

lp[,day:=day(datatime)]

lp[,hour:=hour(datatime)]

lp[,wateryear:= ifelse(month>=10, year+1, year)] #adds column for water year

#filter by date and create new objects for normals 1 and 2 defined above
lp[datatime >= startnormal1 & datatime <= endnormal1, normal :=normal1name]
lp[datatime >= startnormal2 & datatime <= endnormal2, normal :=normal2name]
lp[datatime >= startnormal3 & datatime <= endnormal3, normal :=normal3name]

normals<-c(normal1name, normal2name, normal3name)

lp<-lp %>%
  filter(normal %in% normals)

#averages flow across models, months, normals
lpnday<- lp %>%
  group_by(model, normal, month) %>%
  dplyr::summarise(flow = mean(flow))
setDT(lpnday)

lpnday$normal[lpnday$normal == "1990s"]<-"1990s GCM" #rename normals for plotting purposes
lpnday$normal[lpnday$normal == "2050s"]<-"2050s GCM"
lpnday$normal[lpnday$normal == "2080s"]<-"2080s GCM"

#create object to avg across models 
lpndayavg<-lpnday %>%
  group_by(normal, month) %>%
  dplyr::summarise(flow = mean(flow)) 
setDT(lpndayavg)
lpndayavg[,model:="Avg"]

lpndayavg$normal[lpndayavg$normal == "1990s GCM"]<-"1990s Avg" #rename normals for plotting purposes
lpndayavg$normal[lpndayavg$normal == "2050s GCM"]<-"2050s Avg"
lpndayavg$normal[lpndayavg$normal == "2080s GCM"]<-"2080s Avg"

#calculate percent change
lpnavgwide <- lpndayavg %>%
  pivot_wider(names_from = normal, values_from = flow)
setDT(lpnavgwide)

lpnavgwide[,change2050 := round((`2050s Avg` - `1990s Avg`)/`1990s Avg` * 100, 0)]
lpnavgwide[,change2080 := round((`2080s Avg` - `1990s Avg`)/`1990s Avg` * 100, 0)]

# save percent changes as excel file
xcel_file <- paste0(gauge, '_flowchange.csv')
write.csv(lpnavgwide, xcel_file)

lpn<-rbind(lpnday,lpndayavg) #bind individual models with average of models

#set arbitrary year for plotting purposes (make oct nov dec one year earlier than Jan - Sept data to plot by water year)
lpn[month>=10 & month<=12,year:='2019']
lpn[month>=0 & month<=9,year:='2020']
lpn<-lpn %>%
  mutate(date = make_date(year, month, 1))
setDT(lpn)

#####################plot setup

xlabel <- "\nMonth"
ylabel <- bquote('Discharge ' ~(m^3~s^-1))
tlabel <- ""
#manual legend labels
lname <- ""
#llabels <- c("1990s", "2050s", "2080s")

#set data and variables for 1990s vs 2050s
ylimits<-c(0, max(lpn$flow))
pdata <- lpn[normal!="2080s GCM" & normal != "2080s Avg" & date != "2020-02-29",] #Filter: NOT normal to remove
px <- lpn[normal!="2080s GCM" & normal != "2080s Avg" & date != "2020-02-29", date]
py <- lpn[normal!="2080s GCM" & normal != "2080s Avg" & date != "2020-02-29", flow]
pgroup <- lpn[normal!="2080s GCM" & normal != "2080s Avg" & date != "2020-02-29", normal]
pgroup2<- lpn[normal!="2080s GCM" & normal != "2080s Avg" & date != "2020-02-29", model]

pgroup <- factor(pdata$normal, levels = c("1990s GCM", "2050s GCM", "1990s Avg", "2050s Avg")) #order normals included

plot <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=interaction(pgroup2,pgroup), colour=factor(pgroup), alpha = factor(pgroup))) +
  scale_colour_manual(values=c("gray", "lightblue", "black", "blue"), name=lname, guide=guide_legend(order=1)) + #controls colors
  scale_alpha_manual(values=c(0.8, 0.8, 1, 1), name = lname, guide = guide_legend(order=1))+#controls alpha
  guides(colour = guide_legend(override.aes = list(shape = NA)), alpha = guide_legend()) + #controls legend
  ylab(ylabel) + ggtitle(ptitle) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + coord_cartesian(ylim = ylimits) +
  theme(legend.position = c(0.97,0.95), axis.title.y = element_text(margin = margin(t = 10)), plot.margin = margin (1,10,10,10),
        axis.text.x = element_text(colour = "black", size=8), axis.text.y = element_text(colour = "black", size = 8), 
        axis.title = element_text(size=10), panel.background = element_blank(), panel.border = element_rect(fill = NA),
        legend.key = element_blank(), legend.text = element_text(size=8), legend.margin = margin(-20), plot.title = element_text(hjust = 0.5),
        legend.justification = c("right", "top"), panel.grid.major.y = element_line(color = "black", size = 0.2, linetype = 2),
        axis.ticks = element_line(), strip.text = element_text(size=8), axis.title.x = element_blank(),
        strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.major.x = element_blank()) #controls plot background/tics/etc.

plot
plotname <- paste0('MonthlyAvgFlows_2050s_', gauge)

ggsave(filename = paste0(plotname, ".png"), plot = plot, units = c("in"), width = 6.5, height = 4, dpi = 300)

#set data and variables for 1990s vs 2050s
pdata <- lpn[normal!="2050s GCM" & normal != "2050s Avg" & date != "2020-02-29",] #Filter: NOT normal to remove
px <- lpn[normal!="2050s GCM" & normal != "2050s Avg" & date != "2020-02-29", date]
py <- lpn[normal!="2050s GCM" & normal != "2050s Avg" & date != "2020-02-29", flow]
pgroup <- lpn[normal!="2050s GCM" & normal != "2050s Avg" & date != "2020-02-29", normal]
pgroup2<- lpn[normal!="2050s GCM" & normal != "2050s Avg" & date != "2020-02-29", model]

pgroup <- factor(pdata$normal, levels = c("1990s GCM", "2080s GCM", "1990s Avg", "2080s Avg")) #order normals included

plot <- ggplot() +
geom_line(data=pdata, aes(x=px, y=py, group=interaction(pgroup2,pgroup), colour=factor(pgroup), alpha = factor(pgroup))) +
  scale_colour_manual(values=c("gray", "pink", "black", "red"), name=lname, guide=guide_legend(order=1)) + #controls colors
  scale_alpha_manual(values=c(0.8, 0.8, 1, 1), name = lname, guide = guide_legend(order=1))+#controls alpha
  guides(colour = guide_legend(override.aes = list(shape = NA)), alpha = guide_legend()) + #controls legend
  xlab(xlabel) + ylab(ylabel) +
  ggtitle(tlabel) + scale_x_date(date_breaks = "1 month", date_labels = "%b") + coord_cartesian(ylim = ylimits) +
  theme(legend.position = c(0.97,0.95), axis.title.y = element_text(margin = margin(t = 10)), plot.margin = margin (1,10,10,10),
        axis.text.x = element_text(colour = "black", size=8), axis.text.y = element_text(colour = "black", size = 8), 
        axis.title = element_text(size=10), panel.background = element_blank(), panel.border = element_rect(fill = NA),
        legend.key = element_blank(), legend.text = element_text(size=8), legend.margin = margin(-20), 
        legend.justification = c("right", "top"), panel.grid.major.y = element_line(color = "black", size = 0.2, linetype = 2),
        axis.ticks = element_line(), strip.text = element_text(size=8), axis.title.x = element_blank(),
        strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white"),
        panel.grid.major.x = element_blank()) #controls plot background/tics/etc.

plot
plotname <- paste0('MonthlyAvgFlows_2080s_', gauge)

ggsave(filename = paste0(plotname, ".png"), plot = plot, units = c("in"), width = 6.5, height = 4, dpi = 300)
