# This script computes rolling average flow magnitudes from DHSVM timeseries (Streamflow.Only files) and uses them in 
# flood frequency analysis (FFA). FFA is based on L-moments and generalized extreme value Gumbel distributions (or GEV type 1). Users can define 
# normals (over which to build annual peak flow timseries), flow durations, return intervals of interest, and DHSVM output locations. 
# The script generates cumulative probability (x) vs flow (y) plots, return interval (x) vs flow (y) plots, and .csv/.RDS files for
# each output location and flow duration with columns for Flow, Return Period, Model, Normal, and Nearest Return Interval. This file can
# be used to compute % changes in peak flows within the same timeseires (GCM forcing set) across normals. Kolmogorov-Smirnov (KS) tests
# are also performed between normals for each flow distribution; these results can be used to differentiate if there is substantial
# differences in flow distributions between normals; KS tables are output for each model/flow duration.
# Written by Evan Paul at WWU, last revised 4-20-2022


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

rm(list=ls())
wd <- "D:\\projected_sims\\cedarville"

setwd(wd)

####################################Start of Gauging Location Loop#######################

# #Save_points<-c("NFDarrington", "NFSquireCreek", "NFOso", "NFDeerCreek", "NFBoulderCreek", "NFArlington", "Stanwood", "Pilchuck626", "Silvana", "PortageCreek", "SFJimCreek", "SFGraniteFalls", "SFCanyonCreek", "SFJordanRd", "SFVerlot")
# Save_points<-c("Stanwood")
# 
# for (g in unique(Save_points)) {

#select the DHSVM output location you're interested in (must match column name in DHSVM output file exactly):
gauge <- "CEDARVILLE"
g<-"CEDARVILLE"
#start date and end date of normals

startnormal1<-'1985-10-01 00:00'
endnormal1<-'2015-09-30 23:00'
normal1name<- "1990s"

PNNL<-as.data.table(read.table("nf_pnnlobs.only",sep = "", header=T, fill = T)[-1 ,c("DATE",gauge)]) #pull time series index and data columns of DHSVM output
PNNL[,model:="PNNL"]
if (gauge == 'CEDARVILLE'){
  setwd("D:\\projected_sims\\MF")
  MF_flow <- as.data.table(read.table("mf_pnnlobs.Only", sep = "", header=T, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
  PNNL <- PNNL[, CEDARVILLE := CEDARVILLE + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
  
  setwd("D:\\projected_sims\\SF")
  SF_flow <- as.data.table(read.table("sf_pnnlobs.Only", sep = "", header=T, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
  PNNL <- PNNL[, CEDARVILLE := CEDARVILLE + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
}

setwd(wd)

# l=list(ACCESS1.0, ACCESS1.3, BCC_CSM1.1, CanESM2, CCSM4, CSIRO_Mk3.6.0, FGOALS_g2, GFDL_CM3, GISS_E2_H, MIROC5, MRI_CGCM3, NorESM1_M)
l=list(PNNL)
#make bound object lp for longprojections

########################Start of flow duration loop################################
fd<-c(3) #set flow durations of interest (hours)

# for (d in fd) { #loops through each position in the vector above 

d<-fd

lp<- rbindlist(l, use.names = TRUE, fill = TRUE)

#rename gauge column flow

colnames(lp)[2]<-"flow"

lp$flow<- lp$flow*(1/3600) #convert from DHSVM units (m^3/hr) to m^3/s

lp[,datatime:=as.POSIXct(DATE, format= "%m.%d.%Y-%H:%M:%S", tz = "")]

lp[,year:=year(datatime)]

lp[,month:=month(datatime)]

lp[,wateryear:= ifelse(month>=10, year+1, year)] #adds column for water year

#filter by date and create new objects for normals 1 and 2 defined above
lp[datatime >= startnormal1 & datatime <= endnormal1, normal :=normal1name]
# lp[datatime >= startnormal2 & datatime <= endnormal2, normal :=normal2name]
# lp[datatime >= startnormal3 & datatime <= endnormal3, normal :=normal3name]

normals<-c(normal1name)

lp<-lp %>%
  filter(normal %in% normals)

#build rolling sums in a new column called roll_sum_mean, arranged by model, normal, and date and grouped by model and normal
lproll<-lp %>%
  arrange(model, normal, datatime) %>%
  group_by(model, normal) %>%
  mutate(roll_sum_flow = roll_sum(flow, d, align = c("right"), fill = NA, partial = FALSE)/d) #####fd[d]->d

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
  mutate(p = (c(1:length(roll_sum_flow)))/(length(roll_sum_flow)+1)) %>% #compute empirical probabilities
  mutate(tr = 1/p) #compute recurrance interval

#Set constant y-axis for faceted Cumulative Probability plots
ylimits_CP<-c(0, (max(Ann_max$roll_sum_flow)*1.2))

#Set constant y-axis for faceted Return Period plots
ylimits<- c((min(Ann_max$roll_sum_flow)), max(Ann_max$roll_sum_flow)*1.2)

#set constant x-axis for recurrance intervals

xlimits<- c(0, max(c(Ann_max$tr, Ann_max$tr)))

####################################plot Q vs cumulative probability for each model/normal####################################

allcp_ret = list() #make an empty list to store flow/cumulative probability lists for all models (for facet wrapping)

for (i in unique(Ann_max$model)) {
  
  Ann_max_1 <- Ann_max %>% filter(model == i, normal == normal1name) #filter by model and normal 1
  
  fit_1<- samlmu(Ann_max_1$roll_sum_flow) #L moments for normal 1
  para_1<- pelgev(fit_1)  #Gumbel parameters for normal 1
  by_1<-((max(Ann_max_1$roll_sum_flow)*1.5)-(min(Ann_max_1$roll_sum_flow)))/100 #interval for generating empirical cdf in next row (generates 100 points between max and min)
  boundcp_1<-as.data.table(cbind(cdfgev(seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = -by_1), para_1), seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = -by_1))) #make bound object of probabilities and flows
  
  #build long table of cumulative probs/flows for each normal
  boundcp_1[,normal:= normal1name]
  cplist=list(boundcp_1)
  boundcp<-rbindlist(cplist, use.names = FALSE)
  setnames(boundcp, c("V1", "V2"), c("Cum_prob", "Flow_kcfs"))
  boundcp[,model := i]
  allcp_ret[[i]]<- boundcp
  
  #name plot and save it in working directory
  plotname <- paste0('PNNL_CP_', gauge, "_", d, "_hourflows_", i)
  
  #save .pdfs of plots
  # pdf(paste0(plotname, ".pdf"), width = 84/25.4, height = 100/25.4, paper = "special")
  # 
  # #create plot with both normals annual series and their GEV Type 1 regressions
  # plot(1-(Ann_max_1$p), Ann_max_1$roll_sum_flow, ylim = ylimits ,col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Cumulative Probability", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  # lines(cdfgum(seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), para_1), seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), col = "black")
  # legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name), col = c("black"), lty = 1, cex = 0.65)
  # 
  # dev.off()
  # 
  # #save .bmps of plots
  # bmp(paste0(plotname, ".bmp"), width = 84, height = 100, units = "mm", res = 300)
  # 
  # #create plot with both normals annual series and their GEV Type 1 regressions
  # plot(1-(Ann_max_1$p), Ann_max_1$roll_sum_flow, ylim = ylimits ,col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Cumulative Probability", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  # lines(cdfgum(seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), para_1), seq((max(Ann_max_1$roll_sum_flow)*1.5), min(Ann_max_1$roll_sum_flow), by = (-1)), col = "black")
  # legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name, normal2name, normal3name), col = c("black", "blue", "red"), lty = 1, cex = 0.65)
  # 
  # dev.off()
  
}

#Facet plotting for all Cumulative Probability data
  agboundcps<-rbindlist(allcp_ret)#bind all flow return data across models for facet plotting
  setDT(agboundcps)
  #setup for facet plotting of data
  xlabel1 <- "\n Cumulative Probability"
  ylabel1 <- bquote(atop('Discharge ' (~m^3 ~s^-1)),
                   "")
  tlabel1 <- gauge
  #manual legend labels
  lname1 <- ""
  llabels1 <- c("1990s")
  #set data and variables
  pdata1 <- agboundcps
  px1 <- agboundcps[, Cum_prob]
  py1 <- agboundcps[, Flow_kcfs]
  pgroup1 <- agboundcps[, normal]

  plot1 <- ggplot() +
    geom_line(data=pdata1, aes(x=px1, y=py1, group=factor(pgroup1), colour=factor(pgroup1), alpha = factor(pgroup1))) + #maps aesthetics for x and y data, data groups, and color/alpha
    scale_colour_manual(values=c("black"),name=lname1, labels=llabels1, guide=guide_legend(order=1)) + #controls colour
    scale_alpha_manual(values=c(1, 0.75, 0.75), name = lname1, guide = guide_legend(order=1))+ #controls alpha
    guides(colour = guide_legend(override.aes = list(shape = NA)), alpha = guide_legend()) + #controls legend
    scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1)) + 
    
    xlab(xlabel1) + ylab(ylabel1) + ylim(ylimits_CP)+ #xscale + #theme_economist_white(gray_bg=FALSE) + xscale #sets x and y labels and xscale
    theme(legend.position = "top", axis.title.y = element_text(margin = margin(t = 20)), plot.margin = margin (1,20,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
          panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 2,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 2, colour = "black"), axis.ticks = element_blank(),
          panel.spacing.x = unit(0.95, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) + #controls plot background/tics/etc.
    ggtitle(tlabel1) + facet_wrap(.~model, nrow=4)
  
  plot1
  plot1name <- paste0('PNNL_FW_CumProb_', gauge, d, "_hourflows")
  
  # ggsave(filename = paste0(plot1name , ".bmp"), plot = plot1, units = c("mm"), width = 174, height = 200, dpi = 300)
  # ggsave(filename = paste0(plot1name, ".pdf"), plot = plot1, units = c("mm"), width = 174, height = 200)
  # 

#################################plot Q vs recurrence time for each model/normal############################
datalist1 = list() #make empty list to store all flow per return interval values in a table
datalist2 = list()
datalist3 = list()
allflow_ret = list() #make an empty list to store all flow/return intervals for all models for facet wrapping

for (i in unique(Ann_max$model)) {
  
  Ann_max_1 <- Ann_max %>% filter(model == i, normal == normal1name) #filter by model and normal 1
  
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
  
  #build long table of flows/return intervals for each normal
  bound_flow_rec_1[,normal:= normal1name]
  flowslist=list(bound_flow_rec_1)
  boundflows<-rbindlist(flowslist, use.names = FALSE)
  setnames(boundflows, c("y_1", "fitted.tr_1"), c("Flow_m3ps", "Return_Period_yr"))
  boundflows[,model := i]
  allflow_ret[[i]]<- boundflows
  
  #Create y-limits for return period plots
  RP_ylimits<- c(0, max(boundflows$Flow_m3ps))
  
  #name plot and save it in working directory
  plotname <- paste0('RP_', gauge, "_", d, "_hourflows_", i)
                     
  #save .pdfs of plots
  # pdf(paste0(plotname, ".pdf"), width = 84/25.4, height = 100/25.4, paper = "special")
  # 
  # #create plot with both normals annual series and their Gumbel regressions
  # plot(Ann_max_1$tr, Ann_max_1$roll_sum_flow, xlim = c(0,100), ylim = RP_ylimits, col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Return Period (yr)", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  # lines(fitted.tr_1, y_1, col = "black") 
  # legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name), col = c("black"), lty = 1, cex = 0.65)
  # 
  # dev.off()
  
  #save .bmps of plots
  # bmp(paste0(plotname, ".bmp"), width = 84, height = 100, units = "mm", res = 300)
  # 
  # #create plot with both normals annual series and their Gumbel regressions
  # plot(Ann_max_1$tr, Ann_max_1$roll_sum_flow, xlim = c(0,100), ylim = RP_ylimits, col = "black", pch = 16, cex = 0.55, ylab = expression(Discharge ~(~m^3/~s^-1)), xlab = "Return Period (yr)", main = paste0(i), cex.lab = 0.75, cex.axis = 0.75)
  # lines(fitted.tr_1, y_1, col = "black")
  # legend("bottomright", inset = c(0, 1), xpd = TRUE, horiz = TRUE, bty = "n", legend = c(normal1name), col = c("black"), lty = 1, cex = 0.65)
  # 
  # dev.off()
  
} 

ag_ret_int1<-rbindlist(datalist1) #aggregated data from all models for normal 1 (hindcast)
ag_ret_ints<-rbindlist(list(ag_ret_int1)) #bound object of both lists
ag_ret_ints$Return_Period_yr<-round(ag_ret_ints$Return_Period_yr, digits = 0) #rounding return intervals to nearest whole integer

#write a .csv of the RP data 
fwrite(ag_ret_ints, file = paste0(gauge,"_", d,"_hourflows","_",normal1name, ".csv"), row.names = FALSE)
#write a RDS of the RP data
saveRDS(ag_ret_ints, file =paste0(gauge,"_", d,"_hr_flows","_",normal1name, ".RDS"))


#facet plot all recurrence interval plots
agboundflows<-rbindlist(allflow_ret)#bind all flow return data across models for facet plotting
setDT(agboundflows)
#setup for facet plotting of data
xlabel <- "\nReturn Period (yr)"
ylabel <- bquote(atop('Discharge ' (~m^3 ~s^-1)),
                  "")
FW_RP_ylimits<-c(0,max(agboundflows$Flow_m3ps)/1.25) #y-axis limits
tlabel <- gauge
#manual legend labels
lname <- ""
llabels <- c("1990s")
#set data and variables
pdata <- agboundflows
px <- agboundflows[, Return_Period_yr]
py <- agboundflows[, Flow_m3ps]
pgroup <- agboundflows[,normal]

plot2 <- ggplot() +
  geom_line(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup), alpha = factor(pgroup))) +#maps aesthetics for x and y data, data groups, and color/alpha
  scale_colour_manual(values=c("black"),name=lname, labels=llabels, guide=guide_legend(order=1)) +#controls colour
  scale_alpha_manual(values=c(1, 0.75, 0.75), name = lname, guide = guide_legend(order=1))+#controls alpha
  guides(colour = guide_legend(override.aes = list(shape = NA)), alpha = guide_legend()) + #controls legend

  xlab(xlabel) + ylab(ylabel) + scale_x_continuous(breaks = c(10,50,100), minor_breaks = c(2,20)) + coord_cartesian(xlim = c(0,100), ylim = FW_RP_ylimits)+ #xscale + #theme_economist_white(gray_bg=FALSE) + xscale #sets x and y labels and xscale
  theme(legend.position = "top", axis.title.y = element_text(margin = margin(t = 20)), plot.margin = margin (1,20,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
        panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 2,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 2, colour = "black"), axis.ticks = element_blank(),
        panel.spacing.x = unit(0.95, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) + #controls plot background/tics/etc.
  ggtitle(tlabel1) + facet_wrap(.~model, nrow=4)

plot2
plot2name <- paste0('PNNL_FW_RetPer_', gauge, d, "_hourflows")

# ggsave(filename = paste0(plot2name , ".bmp"), plot = plot2, units = c("mm"), width = 174, height = 200, dpi = 300)
# ggsave(filename = paste0(plot2name, ".pdf"), plot = plot2, units = c("mm"), width = 174, height = 200)


####################### lines for counting number of rows per model/normal ################
# test1<- Ann_max %>%
#   filter(model== "access1.0" & normal== "1990s") 
# 
# test2<- Ann_max %>%
#   filter(model == "access1.0" & normal== "2050s") #%>%
#   #nrow()
# 
# test3<- Ann_max %>%
#   filter(model == "access1.0" & normal== "2080s")
