# Redbreast project 2022 
# Date: 02/09/2022
# Author(s): Dr. Callie Crawford
# Goals: Assess magnification error of videos and create new dataset for
  # magnification correction.

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Load in libraries
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(rstatix)
library(car)
library(plyr)
library(reshape2)
library(lmer4)
library(RColorBrewer)
library(lmerTest)
library(reshape2)
library(rptR)
library(ggridges)

### Info about variables----
# PG: 95% of peak gape when mouth is open (height) in cm 
# TTO: Timing of mouth opening in relation to PG in ms 
# TTC: Timing of mouth closing in relation to PG in ms 
# PPROT: Peak protrusion (eye to anterior tip of upper jaw) in cm 
# PPROTVEL: Speed of protrusion in cm/s
# tPPROT: Timing of peak protusion in relation to peak gape in cm 
# VELPG: velocity of the body at the time of peak gape in cm/s 
# maxVEL: maximum velocity of the body through the digitized frames in cm/s
# tmaxVEL: timing of maximum velocity in relation to peak gape in ms 
# ACCPG: acceleration at the time of peak gape in cm/s^2 
# H_L_ratio: height to length ratio of the ingested volume 
# AI: accuracy index 
# ingested_volume: volume of ingested water during suction feeding (cm^3) 
# PPDiopen: predator prey distance at mouth opening in cm 
# timeatcapture: timing of prey capture relative to peak gape in ms 
# VELpreycapture: velocity of the body at the time of prey capture in cm/s

# Read in data ----
data <- read.csv("Redbreast2022_FINAL.csv")
measures <- read.csv("FINAL/MorphologyMeasurements_Redbreast_2022_averaged.csv")

# Reformat data ----
data_merged <- merge(data, measures, by= "Individual")
data_merged$Gape_prop <- data_merged$PG/data_merged$Gape_height

data_merged$SL_prop <- data_merged$SL.x/data_merged$SL.y

## Make the cm corrections to all relevant data ----

Redbreast_data_mag <- data.frame(matrix(, nrow=100, ncol=0))

Redbreast_data_mag$Population <- data_merged$Population
Redbreast_data_mag$Individual <- data_merged$Individual
Redbreast_data_mag$Trialname <- data_merged$Trialname
Redbreast_data_mag$cm_mag <- data_merged$cm_mag
Redbreast_data_mag$SL <- data_merged$SL
Redbreast_data_mag$PG <- data_merged$PG
Redbreast_data_mag$SL_mag <- data_merged$SL/data_merged$cm_mag
Redbreast_data_mag$PG_mag <- data_merged$PG/data_merged$cm_mag
Redbreast_data_mag$PGVEL_mag <- data_merged$PGVEL
Redbreast_data_mag$tPGVEL <- data_merged$tPGVEL
Redbreast_data_mag$TTO <- data_merged$TTO
Redbreast_data_mag$TTC <- data_merged$TTC
Redbreast_data_mag$DUR_open <- data_merged$DUR_open
Redbreast_data_mag$PPROT_mag <- data_merged$PPROT/data_merged$cm_mag
Redbreast_data_mag$tPPROT <- data_merged$tPPROT
Redbreast_data_mag$PPROTVEL_mag <- data_merged$PPROTVEL/data_merged$cm_mag
Redbreast_data_mag$VELPG_mag <- data_merged$VELPG/data_merged$cm_mag
Redbreast_data_mag$maxVEL_mag <- data_merged$maxVEL/data_merged$cm_mag
Redbreast_data_mag$tmaxVEL <- data_merged$tmaxVEL
Redbreast_data_mag$minVEL_mag <- data_merged$minVEL/data_merged$cm_mag
Redbreast_data_mag$tminVEL <- data_merged$tminVEL
Redbreast_data_mag$ACCPG_mag <- data_merged$ACCPG/data_merged$cm_mag
Redbreast_data_mag$maxACC_mag <- data_merged$maxACC/data_merged$cm_mag
Redbreast_data_mag$tmaxACC <- data_merged$tmaxACC
Redbreast_data_mag$minACC_mag <- data_merged$minACC/data_merged$cm_mag
Redbreast_data_mag$tminACC <- data_merged$tminACC
Redbreast_data_mag$predicted_L <- data_merged$predicted_L
Redbreast_data_mag$predicted_H <- data_merged$predicted_H
Redbreast_data_mag$H_L_ratio <- data_merged$H_L_ratio
Redbreast_data_mag$dp <- data_merged$dp
Redbreast_data_mag$db <- data_merged$db
Redbreast_data_mag$AI <- data_merged$AI
Redbreast_data_mag$AIx <- data_merged$AIx
Redbreast_data_mag$AIy <- data_merged$AIy
Redbreast_data_mag$AIz <- data_merged$AIz
Redbreast_data_mag$ingested_volume_mag <- data_merged$ingested_volume/data_merged$cm_mag^3
Redbreast_data_mag$ipreycapture <- data_merged$ipreycapture
Redbreast_data_mag$iPG <- data_merged$iPG
Redbreast_data_mag$PPDiopen_mag <- data_merged$PPDiopen/data_merged$cm_mag
Redbreast_data_mag$PPDiPG_mag <- data_merged$PPDiPG/data_merged$cm_mag
Redbreast_data_mag$PPDipreycapture_mag <- data_merged$PPDipreycapture/data_merged$cm_mag
Redbreast_data_mag$timeatcapture <- data_merged$timeatcapture
Redbreast_data_mag$VELpreycapture_mag <- data_merged$VELpreycapture/data_merged$cm_mag
Redbreast_data_mag$SL_measured <- data_merged$SL_measured
Redbreast_data_mag$Gape_height <- data_merged$Gape_height
Redbreast_data_mag$Gape_prop <- Redbreast_data_mag$PG_mag/data_merged$Gape_height

# Write as csv 
write.csv(Redbreast_data_mag, "Redbreast2022_MAG.csv")

  
# Plot ----
ggplot(data=data_merged, aes(x=PG, y=Gape_prop ,colour=Individual, fill=Individual)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Peak Gape")+
  ylab("Proportion of Max Gape")


ggplot(data=data_merged, aes(x=SL.y, y=Gape_height ,colour=Individual, fill=Individual)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Standard Length")+
  ylab("Max Anatomical Gape")


ggplot(data=data_merged, aes(x=SL.y, y=SL_prop ,colour=Individual, fill=Individual)) +
  geom_point() +
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  xlab("Standard Length")+
  ylab("Video Standard Length as Proportion of Measured Standard Length")

# Read in data 
data_merged <- read.csv("FINAL/MagnificationErrorCorrectedData/Redbreast2023_MAG.csv")

SL_comparisons <- ggplot(data=data_merged, aes(x=SL.y, y=SL.x ,colour=Individual, fill=Individual)) +
  geom_point(size= 2) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_manual(values=park_palettes$Everglades)+ 
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Video Standard Length (cm)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=12),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

VELPG_comparison <- ggplot(data=data_merged, aes(x=SL.y, y=VELPG_mag ,colour=Individual,  fill=Individual)) +
  geom_point(size= 2) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_manual(values=park_palettes$Everglades)+ 
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Velocity at Peak Gape (cm/s)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=12),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face="bold"),
    legend.text = element_text(size=12))

PG_comparison <- ggplot(data=data_merged, aes(x=SL.y, y=PG, colour=Individual, fill=Individual)) +
  geom_point(size= 2) +
  geom_smooth(method=lm , color="black", size = 1.5, fill="#949494", se=TRUE)+
  geom_abline(intercept=0, slope=1, color= "black", linetype = "dashed", size= 1.5)+
  scale_color_manual(values=park_palettes$Everglades)+ 
  theme_classic()+
  xlab("Measured Standard Length (cm)")+
  ylab("Video Peak Gape (cm)")+
  theme_cowplot()+
  theme(
    axis.title = element_text( color="black", size=12),
    axis.line = element_line(size = 1, colour = "black"),
    axis.text = element_text( color="black", size=10))

plot_grid(SL_comparisons, VELPG_comparison, PG_comparison, align= "v",
          labels = "AUTO", ncol = 1,rel_heights = c(1,1,1))

## Convert the rest of the variables to make a new data set to use for analysis that cancels out the magnification error 
