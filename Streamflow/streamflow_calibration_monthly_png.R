# Compares monthly historical streamflow to DHSVM streamflow based on a file indexing system.
# Outputs a png file containing a figure of streamflow comparison at each gauge.
# Created by Evan Paul at WWU


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
library(hydroGOF)
library(grid)
library(xlsx)
library(reader)
library(scales)

rm(list=ls())
cat("\014") #clears console

####################

# First year of complete data for USGS stream gauges:
#   Cedarville = 2005 (WY 2006-present) 
#   NF USGS = 1987 (WY 1988-present) 
#   MF USGS = 1995 (WY 1996-present) 
#   SF USGS = 2008 (WY 2009-present) 
#   SF WICKERSHAM = 1999 (WY 2000-2008)

file_index <- 67
sim_name <- "WY2006-2015" #used in naming of the jpeg file; name of tabs in excel file
startyear <- "2005" #start year of simulation
endyear <- "2015" #end year of simulation
basin <- "NF" #uppernook, NF, MF, SF

#only use if basin == NF; simulations that get added to the NF basin simulation at CEDARVILLE
MF_sim <- 37
SF_sim <- 53

months <- 1:12 #range of months for analysis (ex. 6:9 is JJAS)
ts <- 1 #timestep in hours

ylabel <- bquote('Discharge ' ~(m^3~s^-1)) #y-axis label

####################

starttime <- paste0(startyear, '-10-01 00:00')
endtime <- paste0(endyear, '-09-30 00:00')

if (basin == "uppernook") {
  gauges <- list("CEDARVILLE", "NFUSGS", "MFUSGS", "SFUSGS", "WICKERSHAM")
} else if (basin == "MFSF") {
  gauges <- list("MFUSGS", "SFUSGS", "WICKERSHAM")
} else if (basin == "NF") {
  gauges <- list("CEDARVILLE", "NFUSGS")
} else if (basin == "MF") {
  gauges <- list("MFUSGS")
} else if (basin == "SF") {
  gauges <- list("SFUSGS")#, "WICKERSHAM")
}

#sets the x-label breaks of your plot
if (nchar(sim_name) == 6) {
  xscale <- scale_x_date(name = NULL, date_breaks = "1 month", date_labels = "%b") #breaks every 1 month (good for short calibrations)
} else {
  xscale <- scale_x_date(name = NULL, date_breaks = "1 year", date_labels = "%Y") #breaks every 1 year (good for longer calibrations)
}

####################

for (sim_number in file_index) { #loops through each file index; allows stats and figures for multiple sims to be created
  
  #set the location of your streamflow.only file directory
  sim_folder <- paste0(basin, "_2006-2015_", sim_number) #DHSVM output directory w/ indexing value; same name as the configuration file
  wd <- paste0("C:/Users/Evan/Documents/WWU/Research/DHSVM/Outputs/", basin, "/", sim_folder)
  setwd(wd)
  
  ####################
  
  for (gauge in gauges) {
    
    #name of jpeg file
    plot_name <- paste0("Streamflow ",  gauge, " ", sim_name, "_", sim_number, ".png") #used in the naming of the hist_flow file
    
    #extract flow timeseries for the above gauge and append a column with the trial number
    sim_flow <- as.data.table(read.table("Streamflow.Only", sep = "", header=T, as.is=1, row.names=NULL, fill=T)[-1, c("DATE", gauge)]) #pull time series index and data columns of DHSVM output
    sim_flow[, model := "Simulated"] #creates model column and appends simulated to every row
    colnames(sim_flow)[2] <- "flow" #rename gauge column flow
    
    #adds DHSVM MF and SF streamflow output from MF and SF basin simulation to Cedarville flow output from NF basin simulation
    if (basin == "NF") {
      if (gauge == "CEDARVILLE") {
        MF_wd <- paste0("C:/Users/Evan/Documents/WWU/Research/DHSVM/Outputs/MF/MF_2006-2015_", MF_sim)
        setwd(MF_wd)
        MF_flow <- as.data.table(read.table("Streamflow.Only", sep = "", header=T, as.is=1, row.names=NULL, fill=T)[-1, c("DATE", "MF")]) #pull time series index and data columns of DHSVM output
        sim_flow <- sim_flow[, flow := flow + MF_flow$MF] #adds the sum of MF column to flow column of cedarville gauge
        
        SF_wd <- paste0("C:/Users/Evan/Documents/WWU/Research/DHSVM/Outputs/SF/SF_2006-2015_", SF_sim)
        setwd(SF_wd)
        SF_flow <- as.data.table(read.table("Streamflow.Only", sep = "", header=T, as.is=1, row.names=NULL, fill=T)[-1, c("DATE", "SF")]) #pull time series index and data columns of DHSVM output
        sim_flow <- sim_flow[, flow := flow + SF_flow$SF] #adds the sum of SF column to flow column of cedarville gauge
      }
    }
    
    #convert from m^3 per timestep (as ts which is set above) to cfs
    sim_flow[, flow := flow*(1/3600)]
    sim_flow[, datatime := as.POSIXct(DATE, format = "%m.%d.%Y-%H:%M:%S", tz = "UTC")] #convert date column to R-recognized date-time format
    
    #set histwd to the directory where your historical time series are saved
    histwd <- "C:\\Users\\Evan\\Documents\\WWU\\Research\\historical_data\\USGS_streamflow"
    setwd(histwd) 
    
    #these if/else statements will select the corresponding historical time series matching "gauge" above
    if (gauge == "CEDARVILLE") {
      hist_flow <- as.data.table(read.table("cedarville_WY2006-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 340), breaks = seq(0, 320, 80)) #sets a constant y-axis limit for a consistent visual comparison
      gauge <- 'North Cedarville'
    } else if (gauge == "NFUSGS") {
      hist_flow <- as.data.table(read.table("NFUSGS_WY1988-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 70), breaks = seq(0, 60, 15))
      gauge <- 'North Fork'
    } else if (gauge == "MFUSGS") {
      hist_flow <- as.data.table(read.table("MFUSGS_WY1996-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 50), breaks = seq(0, 40, 10))
      gauge <- 'Middle Fork'
    } else if (gauge == "SFUSGS") {
      hist_flow <- as.data.table(read.table("SFUSGS_WY2009-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 70), breaks = seq(0, 60, 15))
      gauge <- 'South Fork'
    } else if (gauge == "WICKERSHAM") {
      hist_flow <- as.data.table(read.table("wickersham_WY2000-2008.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 70), breaks = seq(0, 60, 15))
    }
    
    #add column to indicate what the data are (historical)
    hist_flow[, model := "Observed"]
    setnames(hist_flow, c("V1", "V2"), c("DATE", "flow")) #set column names
    
    #removes any duplicate rows
    hist_flow <- hist_flow[!duplicated(hist_flow), ]
    
    #make date/time recognizable
    hist_flow[, flow := flow*0.028316847]
    hist_flow[, datatime := as.POSIXct(DATE, format = "%Y-%m-%d %H:%M", tz = "UTC")]
    
    #reset working directory
    setwd(wd)
    
    #make bound object flow_df in long format
    flow_df <- rbind(sim_flow, hist_flow, use.names = TRUE, fill = TRUE)
    
    #add columns and extract year month and day
    flow_df[, year := year(datatime)]
    
    flow_df[, month := month(datatime)]
    
    flow_df[, day := day(datatime)]
    
    #filter by date for the calibration period defined above
    flow_df <-subset(flow_df, datatime >= starttime & datatime <= endtime)
    
    #averages daily flows across the calibration period for the simulation and the historical dataset
    flow_daily <- flow_df %>%
      group_by(model, year, month) %>%
      dplyr::summarise(flow = mean(flow))
    setDT(flow_daily)
    
    #add date column into flow_daily (necessary for building zoo and eliminating simulated data for which there are no obs)
    flow_daily$date <- as.Date(with(flow_daily, paste(year, month, 1, sep = "-"), "%Y-%m-%d"))
    
    #build date vector of observed flows
    vdate <- flow_daily[model == "Observed", date]
    
    #filter to remove dhsvm flows where no obs flows are available
    flow_dailyclean <- flow_daily %>% 
      group_by(model) %>%
      filter(date %in% vdate)
    setDT(flow_dailyclean)
    
    #################### Build individual zoo objects for each model to compute statistics
    
    daily_hist <- flow_dailyclean[model == "Observed"]
    daily_hist[, c(1:3,5)] <- NULL #keeps flow column only
    daily_hist <- as.zoo(daily_hist)
    
    daily_dhsvm <- flow_dailyclean[model == "Simulated"]
    daily_dhsvm[, c(1:3,5)] <- NULL
    daily_dhsvm <- as.zoo(daily_dhsvm)
    
    
    #################### Compute calibration statistics
    
    # Means
    meanname1 <- mean(daily_dhsvm)
    mean_hist <- mean(daily_hist)
    
    # Percent Bias (PBIAS)
    PBIAS <- pbias(daily_dhsvm, daily_hist)
    
    # R squared
    x <- sum((daily_dhsvm-meanname1)*(daily_hist-mean_hist))
    y <- sqrt(sum((daily_dhsvm-meanname1)^2))
    z <- sqrt(sum((daily_hist-mean_hist)^2))
    
    R2 <- (x/(y*z))^2
    
    # R
    R <- (x/(y*z))
    
    # RSR
    SD_hist <- (sd(daily_hist))
    RMSE <- rmse(daily_dhsvm, daily_hist)
    RSR <- RMSE/SD_hist
    
    # NSE
    nse <- NSE(daily_dhsvm, daily_hist)
    
    # KGE
    kge <- KGE(daily_dhsvm, daily_hist)
    
    #################### Build table for statistics
    
    #paste combined stats for each sim dataset
    stattypes <- c("%BIAS", "RSR", "R", "R^2", "NSE", "KGE")
    combstats <- c(round(PBIAS, 1), round(RSR, 2), round(R, 2), round(R2, 2), round(nse, 2), round(kge, 2))
    colnames <- c("Sim", "PNNL")
    statstable <- cbind.data.frame(stattypes, combstats)
    setnames(statstable, colnames)
    
    #transpose table
    statstable <- t(statstable)
    
    #################### Build plot and export plot + calibration summary table
    
    #title based on gauge location
    tlabel <- gauge
    
    #manual legend labels
    llabels <- unique(flow_dailyclean[, model])
    
    nse_label <- paste0('NSE = ', round(nse, 2), ' | KGE = ', round(kge, 2))
    
    #set data and variables
    pdata <- flow_dailyclean
    px <- flow_dailyclean[, date]
    py <- flow_dailyclean[, flow]
    pgroup <- flow_dailyclean[, model]
    
    if (length(months) == 12) {
      plot <- ggplot() +
        geom_line(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup)), size = 0.4, alpha = 0.8) +
        scale_colour_manual(values=c("black", "red"), name=NULL, labels=llabels) +
        theme_bw() + 
        xscale + yscale + ggtitle(tlabel) + coord_cartesian(clip = 'off') +
        theme(text = element_text(size = 8), legend.position = c(0.99, 0.99), legend.text = element_text(size = 7), legend.key.size = unit(3, 'mm'), legend.margin = margin(0,0,0,0),
              panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), legend.justification = c("right", "top"),
              panel.grid.major.y = element_line(color = "gray", size = 0.2, linetype = 2), panel.border = element_rect(size = 0.25))
              #plot.margin = margin(5.5,6.5,5.5,5.5))
    } else {
      plot <- ggplot() +
        geom_point(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup)), size = 0.75) +
        scale_colour_manual(values=c("black", "red"), name=lname, labels=llabels, guide=guide_legend(order=1)) +
        guides(colour = guide_legend(override.aes = list())) +
        xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg=FALSE) + xscale +
        ggtitle(tlabel)
    }
    
    grob <- grobTree(textGrob(nse_label, x=0.9,  y=1.05, g = gpar(fontsize = 7)))
                              
    plotgrob <- plot + annotation_custom(grob)
    
    ggsave(filename = plot_name, plot = plotgrob, units = c("in"), width = 6.5, height = 2, dpi = 300)
  }
}


