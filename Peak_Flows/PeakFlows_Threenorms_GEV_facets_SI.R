# This script computes rolling average flow magnitudes from DHSVM timeseries (Streamflow.Only files) and uses them in 
# flood frequency analysis (FFA). The FFA consists of empirical generalized extreme value functions (GEV type 1 aka Gumbel). Users can define 
# normals (over which to build annual peak flow timseries), flow durations, return intervals of interest, and DHSVM output locations. 
# The script generates cumulative probability (x) vs flow (y) plots, return interval (x) vs flow (y) plots, and .csv/.RDS files for
# each output location and flow duration with columns for Flow, Return Period, Model, Normal, and Nearest Return Interval. This file can
# be used to compute % changes in peak flows within the same timeseires (GCM forcing set) across normals. Kolmogorov-Smirnov (KS) tests
# are also performed between normals for each flow distribution; these results can be used to differentiate if there is statistically significant
# differences in empirical GEV functions between normals; KS tables are output for each model/flow duration. There are two primary loops within the script,
# the inner loop runs through different flow durations, the outter loop runs through different gauging locations. There are also internal loops
# that derive and plot individual GEV functions for cumulative probability and return period. When initially running the script, remove loops for simplicty/troubleshooting.
# Written by Evan Paul at WWU, last revised 5-27-2022

#packages required to run script
library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(dygraphs)
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(RcppRoll)
library(zoo)
library(lmom)
library(DescTools)
library(scales)

#clear memory, set working directory
rm(list=ls())
cat("\014") #clears console

wd <- "D:\\projected_sims\\cedarville"
setwd(wd)

ptitle <- 'North Cedarville Gauge'

####################################Start of Gauging Location Loop#######################

Save_points<-c('CEDARVILLE')

for (g in unique(Save_points)) {

#select the DHSVM output location you're interested in (must match column name in DHSVM output file exactly):
gauge <- g

#fd<-3 #flow duration of interest (in hours), note that units will be in m^3 per flow duration, only use if not looping through multiple flow durations above

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

access1.0<-as.data.table(read.table("access1.0.only",sep = "", header=T, fill=T)[-1,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
access1.0[,model:="ACCESS1.0"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("access1.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  access1.0 <- access1.0[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge

  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("access1.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  access1.0 <- access1.0[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

access1.3<-as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
access1.3[,model:="ACCESS1.3"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  access1.3 <- access1.3[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("access1.3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  access1.3 <- access1.3[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

bcc_csm1.1<-as.data.table(read.table("bcc-csm1.1.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
bcc_csm1.1[,model:="BCC-CSM1.1"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("bcc-csm1.1.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  bcc_csm1.1 <- bcc_csm1.1[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("bcc-csm1.1.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  bcc_csm1.1 <- bcc_csm1.1[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

canesm2<-as.data.table(read.table("canesm2.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
canesm2[,model:="CanESM2"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("canesm2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  canesm2 <- canesm2[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("canesm2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  canesm2 <- canesm2[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

ccsm4<-as.data.table(read.table("ccsm4.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
ccsm4[,model:="CCSM4"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("ccsm4.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  ccsm4 <- ccsm4[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("ccsm4.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  ccsm4 <- ccsm4[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

csiro_mk3.6.0<-as.data.table(read.table("csiro-mk3.6.0.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
csiro_mk3.6.0[,model:="CSIRO-Mk3.6.0"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("csiro-mk3.6.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  csiro_mk3.6.0 <- csiro_mk3.6.0[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("csiro-mk3.6.0.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  csiro_mk3.6.0 <- csiro_mk3.6.0[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

fgoals_g2<-as.data.table(read.table("fgoals-g2.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
fgoals_g2[,model:="FGOALS-g2"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("fgoals-g2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  fgoals_g2 <- fgoals_g2[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("fgoals-g2.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  fgoals_g2 <- fgoals_g2[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

gfdl_cm3<-as.data.table(read.table("gfdl-cm3.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
gfdl_cm3[,model:="GFDL-CM3"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("gfdl-cm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  gfdl_cm3 <- gfdl_cm3[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("gfdl-cm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  gfdl_cm3 <- gfdl_cm3[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

giss_e2_h<-as.data.table(read.table("giss-e2-h.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
giss_e2_h[,model:="GISS-E2-H"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("giss-e2-h.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  giss_e2_h <- giss_e2_h[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("giss-e2-h.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  giss_e2_h <- giss_e2_h[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

miroc5<-as.data.table(read.table("miroc5.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
miroc5[,model:="MIROC5"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("miroc5.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  miroc5 <- miroc5[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("miroc5.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  miroc5 <- miroc5[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

mri_cgcm3<-as.data.table(read.table("mri-cgcm3.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
mri_cgcm3[,model:="MRI-CGCM3"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("mri-cgcm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  mri_cgcm3 <- mri_cgcm3[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("mri-cgcm3.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  mri_cgcm3 <- mri_cgcm3[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

noresm1<-as.data.table(read.table("noresm1-m.only",sep = "", header=T, fill=T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
noresm1[,model:="NorESM1-M"]
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


########################Start of flow duration loop################################
fd<-c(1, 3, 24, 72, 168) #set flow durations of interest (hours)

for (d in fd) { #loops through each position in the vector above 

lp<- rbindlist(l, use.names = TRUE, fill = TRUE)

#rename gauge column flow

colnames(lp)[2]<-"flow"

lp$flow<- lp$flow*(1/3600) #convert from DHSVM units (m^3/hr) to m^3/s

lp[,datatime:=as.POSIXct(DATE, format= "%m.%d.%Y-%H:%M:%S", tz = "UTC")]

lp[,year:=year(datatime)]

lp[,month:=month(datatime)]

lp[,wateryear:= ifelse(month>=10, year+1, year)] #adds column for water year

#filter by date and create new objects for normals 1 and 2 defined above
lp[datatime >= startnormal1 & datatime <= endnormal1, normal :=normal1name]
lp[datatime >= startnormal2 & datatime <= endnormal2, normal :=normal2name]
lp[datatime >= startnormal3 & datatime <= endnormal3, normal :=normal3name]

normals<-c(normal1name, normal2name, normal3name)

lp<-lp %>%
  filter(normal %in% normals)

#build rolling sums in a new column called roll_sum_mean, arranged by model, normal, and date and grouped by model and normal
lproll<-lp %>%
  arrange(model, normal, datatime) %>%
  group_by(model, normal) %>%
  mutate(roll_sum_flow = roll_sum(flow, d, align = c("left"), fill = NA, partial = FALSE)/d) #####fd[d]->d

#select the annual flow maxima for each model/normal, arrange by flow (max to min)
Ann_max<-lproll %>%
  group_by(model, normal, wateryear) %>% top_n(1, roll_sum_flow) %>%
  arrange(model, normal, -roll_sum_flow)

#add rank number to each row, from highest to lowest flows, they get 1:n
Ann_max<-Ann_max %>%
  group_by(model, normal) %>%
  dplyr::mutate(rank_num = 1:n())

#use lmom package to compute recurance intervals for Gumbel distribution (code based on: https://rpubs.com/cassiorampinelli/528388)
Ann_max <- Ann_max %>% 
  group_by(model, normal) %>%
  mutate(p = (c(1:length(roll_sum_flow)))/(length(roll_sum_flow)+1)) %>% #compute cumulative probabilities
  mutate(tr = 1/p) #compute recurrance interval

#Set constant y-axis for faceted Cumulative Probability plots
ylimits_CP<-c(0, (max(Ann_max$roll_sum_flow)*1.2))

#Set constant y-axis for faceted Return Period plots
ylimits<- c((min(Ann_max$roll_sum_flow)), max(Ann_max$roll_sum_flow)*1.2)

#set constant x-axis for recurrance intervals

xlimits<- c(0, max(c(Ann_max$tr, Ann_max$tr)))

####################################derive empircal GEV functions and plot Q vs cumulative probability for each model/normal####################################

allcp_ret = list() #make an empty list to store flow/cumulative probability lists for all models (for facet wrapping)
allcp_kstest = list()

for (i in unique(Ann_max$model)) {
  
  Ann_max_1 <- Ann_max %>% filter(model == i, normal == normal1name) #filter by model and normal 1
  Ann_max_2 <- Ann_max %>% filter(model == i, normal == normal2name) #filter by model and normal 2
  Ann_max_3 <- Ann_max %>% filter(model == i, normal == normal3name) #filter by model and normal 2
  
  fit_1<- samlmu(Ann_max_1$roll_sum_flow) #L moments for normal 1
  para_1<- pelgev(fit_1)  #Gumbel parameters for normal 1
  by_1<-((max(Ann_max_1$roll_sum_flow)*1.5)-(min(Ann_max_1$roll_sum_flow)))/100 #interval for generating empirical cdf in next row (generates 100 points between max and min)
  boundcp_1<-as.data.table(cbind(cdfgev(seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = -by_1), para_1), seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = -by_1))) #make bound object of probabilities and flows
  
  fit_2<- samlmu(Ann_max_2$roll_sum_flow) #same as above for normal 2
  para_2<- pelgev(fit_2) 
  by_2<-((max(Ann_max_2$roll_sum_flow)*1.5)-(min(Ann_max_2$roll_sum_flow)))/100
  boundcp_2<-as.data.table(cbind(cdfgev(seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = -by_2), para_2), seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = -by_2)))
  
  fit_3<- samlmu(Ann_max_3$roll_sum_flow) #same as above for normal 3
  para_3<- pelgev(fit_3) 
  by_3<-((max(Ann_max_3$roll_sum_flow)*1.5)-(min(Ann_max_3$roll_sum_flow)))/100
  boundcp_3<-as.data.table(cbind(cdfgev(seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = -by_3), para_3), seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = -by_3)))
  
  #build long table of cumulative probs/flows for each normal
  boundcp_1[,normal:= normal1name]
  boundcp_2[,normal:= normal2name]
  boundcp_3[,normal:= normal3name]
  cplist=list(boundcp_1, boundcp_2, boundcp_3)
  boundcp<-rbindlist(cplist, use.names = FALSE)
  setnames(boundcp, c("V1", "V2"), c("Cum_prob", "Flow_kcfs"))
  boundcp[,model := i]
  allcp_ret[[i]]<- boundcp
  
  #Run Kolmogorov-Smirnov Tests on 2050s vs 1990s and 2080s vs 1990s
  ks2_1<- ks.test(boundcp_2$V2, boundcp_1$V2) #compute KS statistics between normal 2 and normal 1
  ks3_1<- ks.test(boundcp_3$V2, boundcp_1$V2) #"" normal 3 and normal 1
  ks3_2<-ks.test(boundcp_3$V2, boundcp_2$V2) #"" normal 3 and normal 2
  ksbound<-as.data.table(rbind(ks2_1, ks3_1, ks3_2)) #bind ks objects
  ksbound[,model := i] #add column with model name
  allcp_kstest[[i]]<-ksbound #add data to list
  
  #name plot and save it in working directory
  plotname <- paste0('CP_', gauge, "_", d, "_hourflows_", i)
  
  #save .pdfs of plots
  #pdf(paste0(plotname, ".pdf"), width = 84/25.4, height = 100/25.4, paper = "special")
  
  #create plot with both normals annual series and their GEV Type 1 regressions
  plot(1-(Ann_max_1$p), Ann_max_1$roll_sum_flow, ylim = ylimits ,col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Cumulative Probability", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  points(1-(Ann_max_2$p), Ann_max_2$roll_sum_flow, col = "blue", cex =0.55, pch =15)
  points(1-(Ann_max_3$p), Ann_max_3$roll_sum_flow, col = "red", cex = 0.55, pch =17)
  lines(cdfgev(seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), para_1), seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), col = "black")
  lines(cdfgev(seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = (-1)), para_2), seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = (-1)), col = "blue")
  lines(cdfgev(seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = (-1)), para_3), seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = (-1)), col = "red")
  
  legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name, normal2name, normal3name), col = c("black", "blue", "red"), lty = 1, cex = 0.65)
 
  dev.off()
  
  #save .bmps of plots
  bmp(paste0(plotname, ".bmp"), width = 100, height = 100, units = "mm", res = 300)
  
  #create plot with both normals annual series and their GEV Type 1 regressions
  plot(1-(Ann_max_1$p), Ann_max_1$roll_sum_flow, ylim = ylimits ,col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Cumulative Probability", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  points(1-(Ann_max_2$p), Ann_max_2$roll_sum_flow, col = "blue", cex =0.55, pch =15)
  points(1-(Ann_max_3$p), Ann_max_3$roll_sum_flow, col = "red", cex = 0.55, pch =17)
  lines(cdfgev(seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), para_1), seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), col = "black")
  lines(cdfgev(seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = (-1)), para_2), seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = (-1)), col = "blue")
  lines(cdfgev(seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = (-1)), para_3), seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = (-1)), col = "red")
  
  legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name, normal2name, normal3name), col = c("black", "blue", "red"), lty = 1, cex = 0.65)
  
  dev.off()
  
}
#Building and saving KS statistics across all models
  ks_combined<- rbindlist(allcp_kstest) #bind KS statistics across models post-loop, still need to simplify cols, add D to statistic col
  setnames(ks_combined, c("statistic", "p.value", "data.name"), c("dstatistic", "pvalue", "dataxy")) #rename columns for readability
  ks_combined$dataxy[ks_combined$dataxy=="boundcp_2$V2 and boundcp_1$V2"]<-paste0(normal2name, "_",normal1name)#rename dataxy column contents for readability
  ks_combined$dataxy[ks_combined$dataxy=="boundcp_3$V2 and boundcp_1$V2"]<-paste0(normal3name, "_",normal1name)#rename dataxy column contents for readability
  ks_combined$dataxy[ks_combined$dataxy=="boundcp_3$V2 and boundcp_2$V2"]<-paste0(normal3name, "_",normal2name)#rename dataxy column contents for readability
  
  #write a .csv of the KS data 
  #fwrite(ks_combined, file = paste0("KS_", gauge,"_", d,"_hourflows","_",normal1name, "_",normal2name, "_",normal3name, ".csv"), row.names = FALSE)
  #write a RDS of the KS data
  saveRDS(ks_combined, file =paste0("KS_", gauge,"_", d,"_hr_flows","_",normal1name, "_",normal2name, "_",normal3name, ".RDS"))

#Facet plotting for all Cumulative Probability data
  agboundcps<-rbindlist(allcp_ret)#bind all flow return data across models for facet plotting
  setDT(agboundcps)
  #setup for facet plotting of data
  xlabel1 <- "\n Cumulative Probability"
  ylabel1 <- bquote('Discharge ' ~(m^3~s^-1))
  #manual legend labels
  lname1 <- ""
  llabels1 <- c("1990s", "2050s", "2080s")
  #set data and variables
  pdata1 <- agboundcps
  px1 <- agboundcps[, Cum_prob]
  py1 <- agboundcps[, Flow_kcfs]
  pgroup1 <- agboundcps[, normal]

  plot1 <- ggplot() +
    geom_line(data=pdata1, aes(x=px1, y=py1, group=factor(pgroup1), colour=factor(pgroup1), alpha = factor(pgroup1))) + #maps aesthetics for x and y data, data groups, and color/alpha
    scale_colour_manual(values=c("black", "blue","red"),name=lname1, labels=llabels1, guide=guide_legend(order=1)) + #controls colour
    scale_alpha_manual(values=c(1, 0.75, 0.75), name = lname1, guide = guide_legend(order=1))+ #controls alpha
    guides(colour = guide_legend(override.aes = list(shape = NA)), alpha = guide_legend()) + #controls legend
    scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) + 
    xlab(xlabel1) + ylab(ylabel1) + ylim(ylimits_CP)+ #xscale + #theme_economist_white(gray_bg=FALSE) + xscale #sets x and y labels and xscale
    theme(legend.position = "top", axis.title.y = element_text(margin = margin(t = 20)), plot.margin = margin (1,20,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
          panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 2,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 2, colour = "black"), axis.ticks = element_blank(),
          panel.spacing.x = unit(0.95, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) + #controls plot background/tics/etc.
    ggtitle(ptitle) + facet_wrap(.~model, nrow=4)
  
  plot1
  plot1name <- paste0('FW_CumProb_', gauge, '_', d, "_hourflows")
  
  ggsave(filename = paste0(plot1name , ".png"), plot = plot1, units = c("in"), width = 6.5, height = 8, dpi = 300)
  #ggsave(filename = paste0(plot1name, ".pdf"), plot = plot1, units = c("mm"), width = 174, height = 200)
  

#################################plot Q vs recurrence time for each model/normal############################
datalist1 = list() #make empty list to store all flow per return interval values in a table
datalist2 = list()
datalist3 = list()
allflow_ret = list() #make an empty list to store all flow/return intervals for all models for facet wrapping

for (i in unique(Ann_max$model)) {
  
  Ann_max_1 <- Ann_max %>% filter(model == i, normal == normal1name) #filter by model and normal 1
  Ann_max_2 <- Ann_max %>% filter(model == i, normal == normal2name) #filter by model and normal 2
  Ann_max_3 <- Ann_max %>% filter(model == i, normal == normal3name) #filter by model and normal 2
  
  ret_ints<-c(2, 5, 10, 20, 50, 100) #define your return intervals of interest

  fit_1<- samlmu(Ann_max_1$roll_sum_flow) #L moments for normal 1
  para_1<- pelgev(fit_1)  #Gumbel parameters for normal 1
  y_1<-seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1/10)) #set flows (Q) for cum prob function; from 1.3 times the max flow to the min flow
  gumbel.accum_1<-cdfgev(y_1,para_1) #find where flows would be on cumulative probability function
  fitted.tr_1<-1/(1-gumbel.accum_1) #recurrance interval for y_1 values specified above (inverse of cumulative probability)
  bound_flow_rec_1<-as.data.table(cbind(y_1, fitted.tr_1)) #bind flows and return intervals

  #loop to find the closest Q for the user-defined return intervals in ret_int_yrs
  ret_ints_1<-c() #make blank vector to fill with values, this is what you match to in the temp filter below
  for (num in seq_along(ret_ints)) {
    ret_ints_1[num]<-min(Closest(fitted.tr_1, ret_ints[num], which = FALSE))
  }
  
  #filter to build matrix of flows and return intervals of interest from the greater flow/return interval matrix
  temp1 <- bound_flow_rec_1 %>%
    filter(fitted.tr_1 %in% ret_ints_1) %>%
    mutate(y_1 = y_1, model = i, normal = normal1name)
  setnames(temp1, c("y_1", "fitted.tr_1"), c("Flow_m3ps", "Return_Period_yr"))
  datalist1[[i]]<- temp1
  
  #### same as above for normal 2
  
  fit_2<- samlmu(Ann_max_2$roll_sum_flow)
  para_2<- pelgev(fit_2) 
  y_2<-seq((max(Ann_max_2$roll_sum_flow)*1.5), min(Ann_max_2$roll_sum_flow), by = (-1/10))
  gumbel.accum_2<-cdfgev(y_2,para_2)
  fitted.tr_2<-1/(1-gumbel.accum_2)
  bound_flow_rec_2<- as.data.table(cbind(y_2, fitted.tr_2))
  
  #loop to find the closest Q for the user-defined return intervals in ret_int_yrs
  ret_ints_2<-c() #make blank vector to fill with values, this is what you matched in the temp filter below
  for (num in seq_along(ret_ints)) {
    ret_ints_2[num]<-min(Closest(fitted.tr_2, ret_ints[num], which = FALSE))
  }
  
  #filter to build matrix of flows and return intervals of interest from the greater flow/return interval matrix
  temp2 <- bound_flow_rec_2 %>%
    filter(fitted.tr_2 %in% ret_ints_2) %>%
    mutate(y_2 = y_2, model = i, normal = normal2name)
  setnames(temp2, c("y_2", "fitted.tr_2"), c("Flow_m3ps", "Return_Period_yr"))
  datalist2[[i]]<- temp2
  
  #### same as above for normal 3
  
  fit_3<- samlmu(Ann_max_3$roll_sum_flow)
  para_3<- pelgev(fit_3) 
  y_3<-seq((max(Ann_max_3$roll_sum_flow)*1.5), min(Ann_max_3$roll_sum_flow), by = (-1/10))
  gumbel.accum_3<-cdfgev(y_3,para_3)
  fitted.tr_3<-1/(1-gumbel.accum_3)
  bound_flow_rec_3<- as.data.table(cbind(y_3, fitted.tr_3))
  
  #loop to find the closest Q for the user-defined return intervals in ret_int_yrs
  ret_ints_3<-c() #make blank vector to fill with values, this is what you matched in the temp filter below
  for (num in seq_along(ret_ints)) {
    ret_ints_3[num]<-min(Closest(fitted.tr_3, ret_ints[num], which = FALSE))
  }
  
  #filter to build matrix of flows and return intervals of interest from the greater flow/return interval matrix
  temp3 <- bound_flow_rec_3 %>%
    filter(fitted.tr_3 %in% ret_ints_3) %>%
    mutate(y_3 = y_3, model = i, normal = normal3name)
  setnames(temp3, c("y_3", "fitted.tr_3"), c("Flow_m3ps", "Return_Period_yr"))
  datalist3[[i]]<- temp3
  
  #build long table of flows/return intervals for each normal
  bound_flow_rec_1[,normal:= normal1name]
  bound_flow_rec_2[,normal:= normal2name]
  bound_flow_rec_3[,normal:= normal3name]
  flowslist=list(bound_flow_rec_1, bound_flow_rec_2, bound_flow_rec_3)
  boundflows<-rbindlist(flowslist, use.names = FALSE)
  setnames(boundflows, c("y_1", "fitted.tr_1"), c("Flow_m3ps", "Return_Period_yr"))
  boundflows[,model := i]
  allflow_ret[[i]]<- boundflows
  
  #Create y-limits for return period plots
  RP_ylimits<- c(0, max(boundflows$Flow_m3ps))
  
  #name plot and save it in working directory
  plotname <- paste0('RP_', gauge, "_", d, "_hourflows_", i)
                     
  #save .pdfs of plots
  #pdf(paste0(plotname, ".pdf"), width = 84/25.4, height = 100/25.4, paper = "special")
  
  #create plot with both normals annual series and their Gumbel regressions
  plot(Ann_max_1$tr, Ann_max_1$roll_sum_flow, xlim = c(0,100), ylim = RP_ylimits, col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Return Period (yr)", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  points(Ann_max_2$tr, Ann_max_2$roll_sum_flow, col = "blue", cex =0.55, pch =15)
  points(Ann_max_3$tr, Ann_max_3$roll_sum_flow, col = "red", cex = 0.55, pch =17)
  lines(fitted.tr_1, y_1, col = "black") 
  lines(fitted.tr_2, y_2, col = "blue") 
  lines(fitted.tr_3, y_3, col = "red")
  legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name, normal2name, normal3name), col = c("black", "blue", "red"), lty = 1, cex = 0.65)
  
  dev.off()
  
  #save .bmps of plots
  bmp(paste0(plotname, ".bmp"), width = 100, height = 100, units = "mm", res = 300)
  
  #create plot with both normals annual series and their Gumbel regressions
  plot(Ann_max_1$tr, Ann_max_1$roll_sum_flow, xlim = c(0,100), ylim = RP_ylimits, col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Return Period (yr)", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  points(Ann_max_2$tr, Ann_max_2$roll_sum_flow, col = "blue", cex =0.55, pch =15)
  points(Ann_max_3$tr, Ann_max_3$roll_sum_flow, col = "red", cex = 0.55, pch =17)
  lines(fitted.tr_1, y_1, col = "black") 
  lines(fitted.tr_2, y_2, col = "blue") 
  lines(fitted.tr_3, y_3, col = "red")
  legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name, normal2name, normal3name), col = c("black", "blue", "red"), lty = 1, cex = 0.65)
  
  dev.off()
  
} 

ag_ret_int1<-rbindlist(datalist1) #aggregated data from all models for normal 1 (hindcast)
ag_ret_int2<-rbindlist(datalist2) #same as above for normal2
ag_ret_int3<-rbindlist(datalist3) #same as above for normal3
ag_ret_ints<-rbindlist(list(ag_ret_int1, ag_ret_int2, ag_ret_int3)) #bound object of both lists
ag_ret_ints$Return_Period_yr<-round(ag_ret_ints$Return_Period_yr, digits = 0) #rounding return intervals to nearest whole integer

#write a .csv of the RP data 
#fwrite(ag_ret_ints, file = paste0(gauge,"_", d,"_hourflows","_",normal1name, "_",normal2name, "_",normal3name, ".csv"), row.names = FALSE)
#write a RDS of the RP data
saveRDS(ag_ret_ints, file =paste0(gauge,"_", d,"_hr_flows","_",normal1name, "_",normal2name, "_",normal3name, ".RDS"))


#facet plot all recurrence interval plots
agboundflows<-rbindlist(allflow_ret)#bind all flow return data across models for facet plotting
setDT(agboundflows)
#setup for facet plotting of data
xlabel <- "\nReturn Period (yr)"
ylabel <- bquote('Discharge ' ~(m^3~s^-1))

FW_RP_ylimits<-c(0,max(agboundflows$Flow_m3ps)/1.25) #y-axis limits
#manual legend labels
lname <- ""
llabels <- c("1990s", "2050s", "2080s")
#set data and variables
pdata <- agboundflows
px <- agboundflows[, Return_Period_yr]
py <- agboundflows[, Flow_m3ps]
pgroup <- agboundflows[,normal]

plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup), alpha = factor(pgroup))) +#maps aesthetics for x and y data, data groups, and color/alpha
  scale_colour_manual(values=c("black", "blue","red"),name=lname, labels=llabels, guide=guide_legend(order=1)) +#controls colour
  scale_alpha_manual(values=c(1, 0.75, 0.75), name = lname, guide = guide_legend(order=1))+#controls alpha
  guides(colour = guide_legend(override.aes = list(shape = NA)), alpha = guide_legend()) + #controls legend
  xlab(xlabel) + ylab(ylabel) + scale_x_continuous(breaks = c(10,50,100), minor_breaks = c(2,20)) + coord_cartesian(xlim = c(0,100), ylim = FW_RP_ylimits)+ #xscale + #theme_economist_white(gray_bg=FALSE) + xscale #sets x and y labels and xscale
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin = margin(t = 20)), plot.margin = margin (1,20,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
        panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 2,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 2, colour = "black"), axis.ticks = element_blank(),
        panel.spacing.x = unit(0.95, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) + #controls plot background/tics/etc.
  ggtitle(ptitle) + facet_wrap(.~model, nrow=4)

plot2
plot2name <- paste0('FW_RetPer_', gauge, "_", d, "_hourflows")

ggsave(filename = paste0(plot2name , ".png"), plot = plot2, units = c("in"), width = 6.5, height = 8, dpi = 300)
#ggsave(filename = paste0(plot2name, ".pdf"), plot = plot2, units = c("mm"), width = 174, height = 200)

  }
}