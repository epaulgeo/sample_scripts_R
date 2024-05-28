# This script reads in Streamflow.Only files and finds rolling sum streamflow magnitude. It compares these magnitudes to user-defined
# flow thresholds like a 10-year return period to count number of exceedances per normal per GCM forcing set
# Written by Evan Paul, WWU, last modified 6-25-2022

library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(RcppRoll)
library(pals)

rm(list=ls())
wd <- "D:\\projected_sims\\cedarville"

setwd(wd)

fd<-3 #set flow durations of interest (hours)

#select the Streamflow.Only column location you're interested in (must match column name in DHSVM output file exactly):
gauge <- "CEDARVILLE"
tlabel <- 'North Cedarville Gauge'
ft<-1340 #PNNL_Obs threshold for 10-yr flow

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
    
    # PNNL_Obs<-as.data.table(read.table("pnnl_1980-2015.VALUES",sep = "", header=T, check.names = F, fill = T)[,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
    # PNNL_Obs[,model:="PNNL_Obs"]
    
    setwd(wd)
    
    l=list(access1.0, access1.3, bcc_csm1.1, canesm2, ccsm4, csiro_mk3.6.0, fgoals_g2, gfdl_cm3, giss_e2_h, miroc5, mri_cgcm3, noresm1)
  
#make bound object lp for longprojections
    
lp<- rbindlist(l, use.names = TRUE, fill = TRUE)
    
#rename re_name column flow
    
colnames(lp)[2]<-"flow"

lp$flow<- lp$flow*(1/3600) #convert from DHSVM units (m^3/hr) to m^3/s
    
lp[,datatime:=as.POSIXct(DATE, format= "%m.%d.%Y-%H:%M:%S", tz = "")] #RESUME HERE <- code stops working 

lp[,month:=month(datatime)]
    
lp[,year:=year(datatime)]

lp[,wateryear:= ifelse(month>=10, year+1, year)] #adds column for water year
    
#filter by date and create new objects for normals 1 and 2 defined above
lp[datatime >= startnormal1 & datatime <= endnormal1, normal :=normal1name]
lp[datatime >= startnormal2 & datatime <= endnormal2, normal :=normal2name]
lp[datatime >= startnormal3 & datatime <= endnormal3, normal :=normal3name]
    
normals<-c(normal1name, normal2name, normal3name)
    
lp<-lp %>%
  filter(normal %in% normals) #<-resume here

#build rolling sum P in a new column called roll_avg_P, arranged by model, normal, and date and grouped by model and normal
lproll<-lp %>%
  arrange(model, normal, datatime) %>%
  group_by(model, normal) %>%
  mutate(roll_sum_flow = roll_sum(flow, fd, align = c("right"), fill = NA, partial = FALSE)/fd) #by aligning right, the flow listed corresponds to the prior period
    
#count number of exceedances above user defined threshold
lpt<- lproll %>% drop_na() %>% 
  group_by(model, normal, .drop = F) %>%
  mutate(above=ifelse(roll_sum_flow<ft, NA, (rleid(roll_sum_flow>=ft)))) %>% #statement adds run-length id to flows above user thresholds, each exceeding P increases by 2 in column, NAs to everything else
  na.omit() %>% #%>% #removes NA rows
  mutate(above=rleid(above))# indexes numbers in new "above" column to make sense -> before they are indexed in multiples of 2, now they are by 1

#count number of exceedances per model/normal
lpt_tot_count <- lpt %>%
  group_by(model, normal) %>%
  summarize(count = max(above)) # counts total number of exceedances

#create empty data table with all model/normal combinations
lpt_empty<- lproll %>%
  group_by(model, normal) %>%
  summarize(aboveNA=0) 

setDT(lpt_tot_count)
setDT(lpt_empty)

#merge emtpy data table with "filled" data table counting exceedances
lpt_tot_count_all <- merge(lpt_tot_count, lpt_empty, by = c("model", "normal"), all=TRUE)

#add aboveNA column and count columns to create new count_tot col, select only that new column
lpt_tot_count_all <- lpt_tot_count_all %>%
  mutate(count_tot = count+aboveNA) %>%
  select(model, normal, count_tot)

#replace NA rows with 0s
lpt_tot_count_all[is.na(lpt_tot_count_all)] <- 0

#write a .csv of the events exceeding P thresholds, above column is index of number of P threshold exceedances
fwrite(lpt_tot_count_all, file = paste0(gauge,"_", fd,"_hourflows","_10yr_PNNL_exceedances", ".csv"), row.names = FALSE)
#write a RDS of the events exceeding P thresholds, above column is index of number of P threshold exceedances
saveRDS(lpt_tot_count_all, file =paste0(gauge,"_", fd,"_hourflows","_10yr_PNNL_exceedances", ".RDS"))

######################################################### by month #################

#count number of exceedances above user defined threshold
lptmo<- lproll %>% drop_na() %>% 
  group_by(model, normal, month, .drop = F) %>%
  mutate(above=ifelse(roll_sum_flow<ft, NA, (rleid(roll_sum_flow>=ft)))) %>% #statement adds run-length id to flows above user thresholds, each exceeding P increases by 2 in column, NAs to everything else
  na.omit() %>% #%>% #removes NA rows
  mutate(above=rleid(above))# indexes numbers in new "above" column to make sense -> before they are indexed in multiples of 2, now they are by 1

#count number of exceedances per model/normal/month
lpt_tot_count_mo <- lptmo %>%
  group_by(model, normal, month) %>%
  summarize(count = max(above)) #counts total number of exceedances by month in each model/normal

#create empty data table with all model/normal combinations
lpt_tot_count_mo_empty<- lproll %>%
  group_by(model, normal, month) %>%
  summarize(aboveNA=0)

#merge emtpy data table with "filled" data table counting exceedances
lpt_tot_count_mo_all <- merge(lpt_tot_count_mo, lpt_tot_count_mo_empty, by = c("model", "normal", "month"), all=TRUE)

#add aboveNA column and count columns to create new count_tot col, select only that new column
lpt_tot_count_mo_all <- lpt_tot_count_mo_all %>%
  mutate(count_tot = count+aboveNA) %>%
  select(model, normal, month, count_tot)

#replace NA rows with 0s
lpt_tot_count_mo_all[is.na(lpt_tot_count_mo_all)] <- 0

#write a .csv of the events exceeding flow thresholds, above column is index of number of flow threshold exceedances
fwrite(lpt_tot_count_mo_all, file = paste0(gauge,"_", fd,"_hourflows","_10yr_PNNL_exceedances_bymo", ".csv"), row.names = FALSE)
#write a RDS of the events exceeding flow thresholds, above column is index of number of flow threshold exceedances
saveRDS(lpt_tot_count_mo_all, file =paste0(gauge,"_", fd,"_hourflows","_10yr_PNNL_exceedances_bymo", ".RDS"))

setDT(lpt_tot_count_all)
setDT(lpt_tot_count_mo_all)
#############################Plot Setup##################################

#setup for facet plotting of data
xlabel <- "\n Climate Normal"
ylabel <- "Frequency \n"


#manual legend labels

#set data and variables
pdata <- lpt_tot_count_all
px <- lpt_tot_count_all[, normal]
py <- lpt_tot_count_all[, count_tot]
pgroup <- lpt_tot_count_all[,model]


#################line plot#######################

# plot2 <- ggplot() +
#   geom_line(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup))) +#maps aesthetics for x and y data, data groups, and color/alpha
#   scale_colour_manual(values = as.vector(cols25(13)), guide = guide_legend(order = 1)) + #selects pallette from the pals package, makes vector of first n number of colors within specific pallette
#   guides(colour = guide_legend(override.aes = list(shape = NA))) + #controls legend
#   
#   xlab(xlabel) + ylab(ylabel) + guides(color=guide_legend(title="GCM")) + scale_x_discrete(expand =c(0,0)) +#xscale + #theme_economist_white(gray_bg=FALSE) + xscale #sets x and y labels and xscale
#   theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 20)), plot.margin = margin (1,20,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
#         panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 2,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 2, colour = "black"), axis.ticks = element_blank(),
#         panel.spacing.x = unit(0.95, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) + #controls plot background/tics/etc.
#   ggtitle(tlabel) #+ facet_wrap(.~model, nrow=4)
# 
# plot2
# plot2name <- paste0('10_yr_Exceedances_line_', re_name, "_", fd, "_hourflows")
# 
# ggsave(filename = paste0(plot2name , ".bmp"), plot = plot2, units = c("mm"), width = 174, height = 100, dpi = 300)
# ggsave(filename = paste0(plot2name, ".pdf"), plot = plot2, units = c("mm"), width = 174, height = 100)

################boxplot#######################

plot3 <- ggplot(data=pdata, aes(x=px, y=py)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = "jitter", aes(x = px, y = py, color = pgroup)) +
  scale_colour_manual(values = as.vector(cols25(13)), guide = guide_legend(order = 1)) +#selects pallette from the pals package, makes vector of first n number of colors within specific pallette
  guides(colour = guide_legend(override.aes = list(shape = NA))) + #controls legend
  ggtitle(tlabel) +
  xlab(xlabel) + ylab(ylabel) + guides(color=guide_legend(title="GCM")) +
  theme_bw()# theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 10)), axis.title.x = element_text(margin = margin(t = 0, l = 0, b=0, r = 0)), plot.margin = margin (10,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
  #       panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 1, colour = "black"), axis.ticks = element_blank(),
  #       panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) #controls plot background/tics/etc.

plot3
plot3name <- paste0('10_yr_Exceedances_box_', gauge, "_", fd, "_hourflows")

ggsave(filename = paste0(plot3name , ".png"), plot = plot3, units = c("in"), width = 6.5, height = 4, dpi = 300)
# ggsave(filename = paste0(plot3name , ".bmp"), plot = plot3, units = c("mm"), width = 120, height = 100, dpi = 300)
# ggsave(filename = paste0(plot3name, ".pdf"), plot = plot3, units = c("mm"), width = 120, height = 100)

#############################by month Plot Setup##################################

#setup for facet plotting of data
xlabel_mo <- "\n Month"
ylabel_mo <- "Frequency \n"

#manual legend labels

#set data and variables
pdata_mo <- lpt_tot_count_mo_all
px_mo <- factor(lpt_tot_count_mo_all$month, levels = c(10,11,12,1,2,3,4,5,6,7,8,9))
py_mo <- lpt_tot_count_mo_all[, count_tot]

pcolor_mo <- lpt_tot_count_mo_all[,model]

plot4 <- ggplot(data=pdata_mo, aes(x=px_mo, y=py_mo, group = px_mo)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = "jitter", aes(x = px_mo, y = py_mo, color = pcolor_mo)) +
  scale_colour_manual(values = as.vector(cols25(13)), guide = guide_legend(order = 1)) +#selects pallette from the pals package, makes vector of first n number of colors within specific pallette
  guides(colour = guide_legend(override.aes = list(shape = NA))) + #controls legend
  ggtitle(tlabel) +
  xlab(xlabel_mo) + ylab(ylabel_mo) + guides(color=guide_legend(title="GCM")) +
  theme_bw() +# theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 10)), axis.title.x = element_text(margin = margin(t = 0, l = 0, b=0, r = 0)), plot.margin = margin (10,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
  #       panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 1, colour = "black"), axis.ticks = element_blank(),
  #       panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) + #controls plot background/tics/etc.
  facet_wrap(.~normal, nrow = 3)
plot4
plot4name <- paste0('10_yr_Exceedances_box_mo_', gauge, "_", fd, "_hourflows")

ggsave(filename = paste0(plot4name , ".png"), plot = plot4, units = c("in"), width = 6.5, height = 6, dpi = 300)
# ggsave(filename = paste0(plot4name , ".bmp"), plot = plot4, units = c("mm"), width = 170, height = 150, dpi = 300)
# ggsave(filename = paste0(plot4name, ".pdf"), plot = plot4, units = c("mm"), width = 170, height = 150)
