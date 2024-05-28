# This script creates boxplots of percent changes in streamflow mag from the output created by the script "Peakflow_Changs_rds.R"
# The script is hard-coded to select streamflow.only columns of interest, flow durations of interest, facet plotting order, and boxplot order.
# Written by Evan Paul at WWU, last revised 4-20-2022

library(ggplot2)
library(data.table)
library(ggthemes)
library(tidyr)
library(pals)
library(dplyr)

rm(list=ls())
cat("\014") #clears console

basin <- "CEDARVILLE"
tlabel<- "North Cedarville Gauge"

wd <- "D:\\projected_sims\\cedarvile"
setwd(wd)

#y-axis limits for plot; determine from flowsofint
ymin <- -30
ymax <- 150

#return_interval <- 5
flows<-readRDS(paste0("cedarville_m3s_changes.RDS")) #reads in RDS file with % changes

#filter for return interval of interest and location(s) of interest
flowsofint<- flows %>%
  #filter(Return_Interval2 == return_interval & duration != 1 & (location == basin))
  filter(duration != 1 & (location == basin))

#creates average of percent change by return period
avg2050 <- flowsofint %>%
  group_by(Return_Interval2) %>%
  summarise(avg2050 = mean(change2050))

avg2080 <- flowsofint %>%
  group_by(Return_Interval2) %>%
  summarise(avg2080 = mean(change2080))

pdata <- flowsofint

#Manually ordering facets for plots and flow durations within each facet
pdata$duration <- factor(pdata$duration, levels = c(3, 24, 72, 168))

#2050s
tlabel2<- "1990s to 2050s" #modify based on normal comparison
xlabel<- "Peak Flow Duration (hr)"
ylabel<- "Streamflow Magnitude Change (%)"

plot1<- ggplot(pdata, aes(x = duration, y = change2050)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar') + #this line adds the error lines at the top/bottom of the whiskers
  geom_point(aes(color = model)) +
  ggtitle(tlabel) +
  labs(subtitle = tlabel2) +
  scale_colour_manual(values = as.vector(cols25(13))) + #selects pallette from the pals package, makes vector of first n number of colors within specific pallette
  facet_wrap(.~Return_Interval2) +
  geom_hline(data = avg2050, aes(yintercept = avg2050), color = "red", alpha = 0.6) +
  xlab(xlabel) + ylab(ylabel) + guides(color=guide_legend(title="GCM")) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  theme_bw()#theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 10)), axis.title.x = element_text(margin = margin(t = 10, l = 0, b=0, r = 0)), plot.margin = margin (1,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
        #panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 1, colour = "black"), axis.ticks = element_blank(),
        #panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) #controls plot background/tics/etc.

plot1

plotname <- paste0("PercentChangePeakFlows_", basin, "_2050s")

ggsave(filename = paste0(plotname , ".png"), plot = plot1, units = c("in"), width = 6.5, height = 4, dpi = 300)
#ggsave(filename = paste0(plotname, ".pdf"), plot = plot1, units = c("in"), width = 6.5, height = 8)

#2080s
tlabel2<- "1990s to 2080s" #modify based on normal comparison
xlabel<- "Peak Flow Duration (hr)"
ylabel<- "Streamflow Magnitude Change (%)"

plot1<- ggplot(pdata, aes(x = duration, y = change2080)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar') + #this line adds the error lines at the top/bottom of the whiskers
  geom_point(aes(color = model)) +
  labs(subtitle = tlabel2) +
  scale_colour_manual(values = as.vector(cols25(13))) + #selects pallette from the pals package, makes vector of first n number of colors within specific pallette
  xlab(xlabel) + ylab(ylabel) + guides(color=guide_legend(title="GCM")) +
  facet_wrap(.~Return_Interval2) +
  geom_hline(data = avg2080, aes(yintercept = avg2080), color = "red", alpha = 0.6) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  theme_bw()#theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 10)), axis.title.x = element_text(margin = margin(t = 10, l = 0, b=0, r = 0)), plot.margin = margin (1,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.text = element_text(size=10),
        #panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 1, colour = "black"), axis.ticks = element_blank(),
        #panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) #controls plot background/tics/etc.

plot1

plotname <- paste0("PercentChangePeakFlows_", basin, "_2080s")

ggsave(filename = paste0(plotname , ".png"), plot = plot1, units = c("in"), width = 6.5, height = 4, dpi = 300)
#ggsave(filename = paste0(plotname, ".pdf"), plot = plot1, units = c("mm"), width = 174, height = 200)
