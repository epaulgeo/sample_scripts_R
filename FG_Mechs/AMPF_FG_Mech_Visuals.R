# This script is for visualizing annual maximum peak flow (AMPF) flow generating (FG) mechanisms. It reads in the output file from "Bind_Basin_FG_Mech_to_Flows.R" and simplifies the columns
# to assign a single FG mech to each AMPF. If multiple decision tree triggers are met (i.e., the event qualifies as extreme precip and rain on snow), the least common
# FG mechanism is assigned for that given event (not including other). The order of assignment for the Nooksack watershed is: 1) snowmelt, 2) rain on snow 3) extreme precip, 4) other.
# Created by Evan Paul at WWU, last modified May 2022

library(lubridate)
library(chron)
library(plyr)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(pals)
library(gridExtra)

rm(list=ls())

basin <- "SF"
ptitle <- "South Fork"

wd <- paste0("D:\\projected_sims\\", basin)
setwd(wd)

AMPF_FG_Mechs<- readRDS(paste0(basin, "_AMF_FG_Mechs_1990s_2050s_2080s"), refhook = NULL)

#simplify data

AMPF_FG_Mechs_simple <- AMPF_FG_Mechs %>%
  mutate(FG_mech = as.factor(case_when( # add FG_mech qualifier column
    EP_event >= 1 & ROS_event < 1 ~ "Ext Pcp", # row gets 1 if it was an EP event and not a ROS event
    ROS_event >= 1 ~ "ROS", # row gets a 2 if it was a ROS event
    Snwmlt_event >= 1 & EP_event < 1 & ROS_event < 1 ~ "Snw Mlt", # row gets a 3 if it was a snowmelt event and not a EP or ROS event
    EP_event <1 & ROS_event < 1 & Snwmlt_event < 1 ~ "Other"))) # row gets a 4 (other) if it doesn't qualify as EP, ROS, or snowmelt -> most likely long duration precip/soil saturation event
    

setDT(AMPF_FG_Mechs_simple)
###################### Boxplot

xlabel <- "Normal"
ylabel <- bquote(atop('Discharge ' (~m^3 ~s^-1)))
#manual legend labels
lname <- ""
llabels <- c("1990s", "2050s", "2080s")

 #set data and variables - Group by Normal and color by FG Mech
 pdata <- AMPF_FG_Mechs_simple
 # px <- lp_count_l[, date]
 py <- AMPF_FG_Mechs_simple[, roll_avg_flow]
 pgroup <- AMPF_FG_Mechs_simple[,normal]
 pcolor<-AMPF_FG_Mechs_simple[,FG_mech]
 
 #set FG Mech order for plotting
 pcolor <- factor(pcolor, levels = c("Ext Pcp", "ROS", "Snw Mlt", "Other"))
 
 plot1<- ggplot(pdata, aes(pgroup, py, group=pgroup)) +
   geom_boxplot(outlier.shape = NA) +
   stat_boxplot(geom ='errorbar') + #this line adds the error lines at the top/bottom of the whiskers
   geom_point(position="jitter", size = 0.25, aes(color = pcolor)) +
   scale_colour_manual(values = as.vector(glasbey(4))) + #selects pallette from the pals package, makes vector of first n number of colors within specific pallette
   
   xlab(xlabel) + ylab(ylabel) + guides(color=guide_legend(title="FG Mech")) +
   theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 0)), axis.title.x = element_text(margin = margin(t = 0, l = 0, b=0, r = 0)), plot.margin = margin (10,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.title = element_text(size =10),legend.text = element_text(size=10),
         panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 1, colour = "black"), axis.ticks = element_blank(),
         panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) #controls plot background/tics/etc.
 plot1
 
 plotname <- paste0("FG_mechs_boxplot_", basin, "_24hr")
 
 #ggsave(filename = paste0(plotname , ".bmp"), plot = plot1, units = c("mm"), width = 120, height = 80, dpi = 300)
 #ggsave(filename = paste0(plotname, ".pdf"), plot = plot1, units = c("mm"), width = 120, height = 80)
 
 ###################### Stacked Bar Chart
 
 ylabel2<- "Proportion of AMPF \n"
 plot2<- ggplot(pdata, aes(x=pgroup, fill = pcolor)) +
   geom_bar(position = "fill", stat = "count") +
   scale_fill_manual(values = as.vector(glasbey(4))) + 
   xlab(xlabel) +ylab(ylabel2) + guides(fill=guide_legend(title="FG Mech")) + ggtitle(ptitle) +
   theme(plot.title = element_text(hjust = 0.5), legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 0)), axis.title.x = element_text(margin = margin(t = 10, l = 0, b=0, r = 0)), plot.margin = margin (10,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.title = element_text(size =10),legend.text = element_text(size=10),
         panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(size = 0.1, linetype = 1, colour = "black"), axis.ticks = element_blank(),
         panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white")) #controls plot background/tics/etc.
 plot2
 
 plotname2 <- paste0("FG_mechs_stackbar_", basin, "_24hr")
 
 ggsave(filename = paste0(plotname2 , ".png"), plot = plot2, units = c("in"), width = 6.5, height = 3.75, dpi = 300)
 #ggsave(filename = paste0(plotname2, ".pdf"), plot = plot2, units = c("mm"), width = 120, height = 80)
 
 ########################## geomdensity
xlabel3<- "Normalized Kernel Density"
 plot3<-ggplot(pdata, aes(y=py, after_stat(count), color = pcolor, fill = pcolor)) +
   geom_density(alpha = 0.1) +
   scale_fill_manual(values = as.vector(glasbey(4))) +
   scale_color_manual(values = as.vector(glasbey(4))) + #selects pallette from the pals package, makes vector of first n number of colors within specific pallette
   xlab(xlabel3) + ylab(ylabel) + guides(color=guide_legend(title="FG Mech"), fill=guide_legend(title="FG Mech")) +
   theme(legend.position = "right", axis.title.y = element_text(margin = margin(t = 0, l = 0, b=0, r = 0)), axis.title.x = element_text(margin = margin(t = 10, l = 0, b=0, r = 0)), plot.margin = margin (10,10,10,10), axis.text.x = element_text(colour = "black", size=10), axis.text.y = element_text(colour = "black", size = 10), axis.title = element_text(size=12), legend.title = element_text(size =10), legend.text = element_text(size=10),
         panel.background = element_blank(), legend.key = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.grid.major = element_line(size = 0.1, linetype = 1,  colour = "black"), panel.grid.minor = element_line(), axis.ticks = element_blank(),
         panel.spacing.x = unit(0.75, "lines"), strip.text = element_text(size=10), strip.text.x=element_text(margin = margin(b=1)), strip.background = element_rect(color = "white", fill = "white"))+ #controls plot background/tics/etc.
   facet_wrap(~normal)
 plot3
 
 plotname3 <- paste0("FG_mechs_density_", basin,"_24hr")
 
 ggsave(filename = paste0(plotname3 , ".png"), plot = plot3, units = c("in"), width = 6.5, height = 4, dpi = 300)
 #ggsave(filename = paste0(plotname3, ".pdf"), plot = plot3, units = c("mm"), width = 160, height = 80)

 
 #stats for written sections
 percent_precip_1990s <- AMPF_FG_Mechs_simple %>%
  filter(normal == "1990s" & FG_mech == "Ext Pcp")
 
 percent_ROS_1990s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "1990s" & FG_mech == "ROS")
 
 percent_precip_2050s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2050s" & FG_mech == "Ext Pcp")
 
 percent_ROS_2050s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2050s" & FG_mech == "ROS")
 
 percent_precip_2080s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2080s" & FG_mech == "Ext Pcp")
 
 percent_ROS_2080s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2080s" & FG_mech == "ROS")
 
 avgmag_precip_1990s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "1990s" & FG_mech == "Ext Pcp") %>%
   summarise(mean(roll_avg_flow))
 
 avgmag_ROS_1990s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "1990s" & FG_mech == "ROS") %>%
   summarise(mean(roll_avg_flow))
 
 avgmag_precip_2050s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2050s" & FG_mech == "Ext Pcp") %>%
   summarise(mean(roll_avg_flow))
 
 avgmag_ROS_2050s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2050s" & FG_mech == "ROS") %>%
   summarise(mean(roll_avg_flow))
 
 avgmag_precip_2080s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2080s" & FG_mech == "Ext Pcp") %>%
   summarise(mean(roll_avg_flow))
 
 avgmag_ROS_2080s <- AMPF_FG_Mechs_simple %>%
   filter(normal == "2080s" & FG_mech == "ROS") %>%
   summarise(mean(roll_avg_flow))
 
 
 
 