# This script amalgamates .csv or .RDS files created by the script "PeakFlows_Threenorms_GEV_facets_SI.R"
# to compute percent changes in streamfow magnitude between normals. It will load in all .RDS files from a specified directory, bind them, 
# and create identifying location and flow duration columns based on file name. It replaces the return interval column with 
# rounded return intervals in case of data consistency issues (e.g., a flow magnitude for a return interval of 101 rather than 100 is returned by
# the parent script). Lastly, it reorients the data for easy computation of percent changes between normals given matching GCM, 
# flow duration, and return interval. The output .csv file is ready for boxplotting or pivot table analysis.
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
library(stringr)

rm(list=ls())
cat("\014") #clears console

filename <- "cedarville_m3s_changes.RDS"

wd <- "D:\\projected_sims\\cedarville"
setwd(wd)

#combine all .RDS fils from working directory; add filename column for parsing later on 
#caution - this will amalgamate ALL .csvs in wd
read_plus <- function(flnm) {
  readRDS(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.RDS", 
             full.names = T) %>% 
  map_df(~read_plus(.))

#separate filename column into location and duration columns
as.data.table(tbl_with_sources)
setDT(tbl_with_sources)

#duplicate filename column
tbl_with_sources$duration<-tbl_with_sources$filename

#change filename column to durations and trucate filename to only include DHSVM output location
setnames(tbl_with_sources, "filename", "location")

tbl_with_sources$location<-str_extract(tbl_with_sources$location, "(?<=/).+(?=(_))") #gets rid of file name pieces from right by _
tbl_with_sources$location<-str_extract(tbl_with_sources$location, "^.*(?=(_))")
tbl_with_sources$location<-str_extract(tbl_with_sources$location, "^.*(?=(_))")
tbl_with_sources$location<-str_extract(tbl_with_sources$location, "^.*(?=(_))")
tbl_with_sources$location<-str_extract(tbl_with_sources$location, "^.*(?=(_))")
tbl_with_sources$location<-str_extract(tbl_with_sources$location, "^.*(?=(_))")

tbl_with_sources$duration<-str_extract(tbl_with_sources$duration, "(?<=_).+(?=_hr_flows)") #gets rid of file name pieces 

#delete rows with return interval of 1 <- these rows cause errors when replacing the return interval column below
tbl<-tbl_with_sources[tbl_with_sources$Return_Period != 1]

#make new column of return intervals (this standardizes return intervals that are slightly off from what was intended [i.e. when Return_intervals have values of 101 vs 100])
Return_Interval2 <- c(100,50,20,10,5,2)
tbl[,Return_Interval2 := rep.int(Return_Interval2,length(unique(tbl[,model]))*length(unique(tbl[,normal]))*length(unique(tbl[,location]))*length(unique(tbl[,duration])))]

#pivot by reshaping
tblwide<-dcast(tbl, model + Return_Interval2 + location + duration ~ normal, value.var = "Flow_m3ps")

#calculate percent changes
tblwide[,change2050 := round((`2050s` - `1990s`)/`1990s` * 100, 2)]
tblwide[,change2080 := round((`2080s` - `1990s`)/`1990s` * 100, 2)]

#write a .RDS of the data 
saveRDS(tblwide, file = filename)