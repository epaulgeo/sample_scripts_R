# Compares daily historical streamflow to DHSVM streamflow based on a file indexing system.
# Outputs a .pdf file containing figure of streamflow comparison at each gauge.
# Adds stats to an excel file with conditional formatting.
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
library(gridExtra)
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

file_index <- 70
sim_name <- "WY2006-2015" #used in naming of the pdf file; name of tabs in excel file
startyear <- "2005" #start year of simulation
endyear <- "2015" #end year of simulation
basin <- "NF" #uppernook, NF, MF, SF

#only use if basin == NF; simulations that get added to the NF basin simulation at CEDARVILLE
MF_sim <- 45
SF_sim <- 55

months <- 1:12 #range of months for analysis (ex. 6:9 is JJAS)
ts <- 1 #timestep in hours

ylabel <- "Discharge (cfs)\n" #y-axis label

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
  gauges <- list("SFUSGS", "WICKERSHAM")
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
  
  #name of pdf file
  pdf_name <- paste0("Streamflow ", sim_name, "_", sim_number, ".pdf") #used in the naming of the hist_flow file
  pdf(pdf_name, width = 10, height = 6)
  
  #################### Adds adjusted parameters from top of configuration file to excel file
  
  #set location of configuration files
  config_dir <- "C:/Users/Evan/Documents/WWU/Research/DHSVM/config_files"
  setwd(config_dir)
  
  description = c()
  if (basin == "NF") { #adds MF and SF sim to description for NF excel column
    description <- c(paste0("MF sim", MF_sim, " and SF sim", SF_sim))
  }
  
  config_file <- paste0(sim_folder, ".txt") #name of config file
  con <- file(config_file, open = "r") #open configuration file in "read" mode
  lines <- readLines(con) #read in configuration file
  for (i in 24:length(lines)) { #start at line 24 (beginning of adjusted parameters description)
    if (lines[i] == "") { #loop stops if a blank line is added beneath the description 
      break
    }
    description[i] <- lines[i] #append lines to description object
  }
  close(con)
  
  description <- description[!is.na(description)] #removes NA from description
  description <- gsub('[#]', '', description) # removes # from description
  description <- paste0(description, collapse = "; ")
  
  #creates initial stats dataframe for excel
  date <- format(Sys.Date(), "%m/%d/%Y") #current time
  stats_df <- data.frame(date, sim_folder, sim_name, sim_number, description)
  stats_list <- list(stats_df)
  
  #reset to directory with streamflow.only file
  setwd(wd)
  
  ####################
  
  for (gauge in gauges) {
    
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
    sim_flow[, flow := flow*0.00980963/ts]
    sim_flow[, datatime := as.POSIXct(DATE, format = "%m.%d.%Y-%H:%M:%S", tz = "UTC")] #convert date column to R-recognized date-time format
    
    #set histwd to the directory where your historical time series are saved
    histwd <- "C:\\Users\\Evan\\Documents\\WWU\\Research\\historical_data\\USGS_streamflow"
    setwd(histwd) 
    
    #these if/else statements will select the corresponding historical time series matching "gauge" above
    if (gauge == "CEDARVILLE") {
      hist_flow <- as.data.table(read.table("cedarville_WY2006-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 45000), breaks = seq(0, 40000, 10000)) #sets a constant y-axis limit for a consistant visual comparison
    } else if (gauge == "NFUSGS") {
      hist_flow <- as.data.table(read.table("NFUSGS_WY1988-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 12000), breaks = seq(0, 10000, 2500))
    } else if (gauge == "MFUSGS") {
      hist_flow <- as.data.table(read.table("MFUSGS_WY1996-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 8500), breaks = seq(0, 8000, 2000))
    } else if (gauge == "SFUSGS") {
      hist_flow <- as.data.table(read.table("SFUSGS_WY2009-2020.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 14000), breaks = seq(0, 12500, 2500))
    } else if (gauge == "WICKERSHAM") {
      hist_flow <- as.data.table(read.table("wickersham_WY2000-2008.csv", sep=",", header=F))
      yscale <- scale_y_continuous(name = ylabel, limits = c(0, 8000), breaks = seq(0, 8000, 2000))
    }
    
    #add column to indicate what the data are (historical)
    hist_flow[, model := "Historical"]
    setnames(hist_flow, c("V1", "V2"), c("DATE", "flow")) #set column names
    
    #removes any duplicate rows
    hist_flow <- hist_flow[!duplicated(hist_flow), ]
    
    #make date/time recognizable
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
      group_by(model, year, month, day) %>%
      dplyr::summarise(flow = mean(flow))
    setDT(flow_daily)
    
    #add date column into flow_daily (necessary for building zoo and eliminating simulated data for which there are no obs)
    flow_daily$date <- as.Date(with(flow_daily, paste(year, month, day, sep = "-"), "%Y-%m-%d"))
    
    #build date vector of observed flows
    vdate <- flow_daily[model == "Historical", date]
    
    #filter to remove dhsvm flows where no obs flows are available
    flow_dailyclean <- flow_daily %>% 
      group_by(model) %>%
      filter(date %in% vdate)
    setDT(flow_dailyclean)
    
    #################### Build individual zoo objects for each model to compute statistics
    
    daily_hist <- flow_dailyclean[model == "Historical"]
    daily_hist[, c(1:4,6)] <- NULL #keeps flow column only
    daily_hist <- as.zoo(daily_hist)
    
    daily_dhsvm <- flow_dailyclean[model == "Simulated"]
    daily_dhsvm[, c(1:4,6)] <- NULL
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
    
    #create stats dataframe
    stats <- data.frame(round(PBIAS, 1), round(RSR, 2), round(R2, 2), round(nse, 2), round(kge, 2))
    
    #appends stats dataframe to list containing all stats for each gauge in the gauge for loop
    stats_list[[gauge]] <- stats
    
    #################### Build plot and export plot + calibration summary table
    
    #title based on gauge location
    tlabel <- gauge
    
    #manual legend labels
    llabels <- unique(flow_dailyclean[, model])
    
    #set data and variables
    pdata <- flow_dailyclean
    px <- flow_dailyclean[, date]
    py <- flow_dailyclean[, flow]
    pgroup <- flow_dailyclean[, model]
    
    if (length(months) == 12) {
      plot <- ggplot() +
        geom_line(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup)), size = 0.5, alpha = 0.8) +
        scale_colour_manual(values=c("black", "red"), name=NULL, labels=llabels) +
        theme_economist_white(gray_bg=FALSE) + 
        xscale + yscale + ggtitle(tlabel)
    } else {
      plot <- ggplot() +
        geom_point(data=pdata, aes(x=px, y=py, group=factor(pgroup), colour=factor(pgroup)), size = 0.75) +
        scale_colour_manual(values=c("black", "red"), name=lname, labels=llabels, guide=guide_legend(order=1)) +
        guides(colour = guide_legend(override.aes = list())) +
        xlab(xlabel) + ylab(ylabel) + theme_economist_white(gray_bg=FALSE) + xscale +
        ggtitle(tlabel)
    }
    
    grobtable <- tableGrob(statstable, theme = ttheme_minimal())
    grid.arrange(plot, grobtable, heights = c(5,1.5), nrow=2)
  }
  
  dev.off()
  
  #################### Excel file
  
  excel_df <- dplyr::bind_cols(stats_list)
  
  #excel file directory path
  wd <- "C:/Users/Evan/Documents/WWU/Research/DHSVM/"
  setwd(wd)
  
  #load in excel file
  wb <- loadWorkbook(file = "DHSVM_simulation_log.xlsx")
  
  #adds border in excel file
  cs <- CellStyle(wb) +
    Border(color = "black",
           position = "LEFT",
           pen = "BORDER_THIN")
  
  #list of columns to apply the border
  csBorder <- list(`6` = cs, `11` = cs, `16` = cs, `21` = cs, `26` = cs)
  
  if (basin == "uppernook") {
    if (sim_name == "WY2006-2015") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"WY2006-2015",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    } else if (sim_name == "WY2012") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"WY2012",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header; started w/ sim 27
    }
  } else if (basin == "MFSF") {
    if (sim_name == "WY2006-2015") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"MFSF WY2006-2015",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    } else if (sim_name == "WY2012") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"MFSF WY2012",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    }
  } else if (basin == "NF") {
    if (sim_name == "WY2006-2015") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"NF WY2006-2015",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    } else if (sim_name == "WY2012") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"NF WY2012",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    }
  } else if (basin == "MF") {
    if (sim_name == "WY2006-2015") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"MF WY2006-2015",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    } else if (sim_name == "WY2012") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"MF WY2012",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    }
  } else if (basin == "SF") {
    if (sim_name == "WY2006-2015") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"SF WY2006-2015",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    } else if (sim_name == "WY2012") {
      addDataFrame(excel_df, sheet = getSheets(wb)$"SF WY2012",
                   colStyle = csBorder,
                   row.names = F, col.names = F, startRow = sim_number + 4) #append after 3-line header
    }
  }
  
  saveWorkbook(wb, "DHSVM_simulation_log.xlsx")
}

graphics.off()

