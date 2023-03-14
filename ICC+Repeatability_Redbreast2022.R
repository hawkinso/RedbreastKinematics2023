# Redbreast project 2022 
# Date: 01/26/2022 
# Author(s): Olivia H Hawkins, Dr. Callie Crawford 
# Goals: Intraclass correlation (ICC) calculations to understand across individual variation 
  # in the context of existing within individual variation.

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Load in libraries ----
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
library(ICC)
library(cowplot)

# Load in data 
data <- read.csv("FINAL/MagnificationErrorCorrectedData/Redbreast2022_MAG.csv")

# Subset data that will be used in analysis ----
# Individual
all.data <- data %>%
  dplyr::select(Individual,SL_mag,PG_mag,TTO,TTC,PPROT_mag,PPROTVEL_mag,tPPROT,VELPG_mag,maxVEL_mag,tmaxVEL,ACCPG_mag,H_L_ratio,AI,ingested_volume_mag,PPDiopen_mag,timeatcapture,VELpreycapture_mag)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual)


# intraclass correlation ----
# In this case, we will use the ICC to investigate the variability of kinematics within an individual in the context of variation across all individuals 
# high ICC (close to 1) = high repeatability/similarity of observations 
# low ICC (close to 0) = observations are not similar 
# below 0.5: poor, 0.5-0.75: moderate, 0.75-0.90: good, above 0.9: excellent 

#### Making dataframes and calculating ICC for each variable 

# STEPS: 
    # ONE: Subset the data to where it only has the kinematic variable and Individuals

pg<- data %>% select(Individual,PG_mag)
gprop <- data %>% select(Individual,Gape_prop)
tto<- data %>% select(Individual,TTO)
ttc<- data %>% select(Individual,TTC)
pprot<- data %>% select(Individual,PPROT_mag)
pprotvel<- data %>% select(Individual,PPROTVEL_mag)
tpprot <- data %>% select(Individual,tPPROT)
velpg<- data %>% select(Individual,VELPG_mag)
maxvel<- data %>% select(Individual,maxVEL_mag)
tmaxvel<- data %>% select(Individual,tmaxVEL)
accpg<- data %>% select(Individual,ACCPG_mag)
ratio<- data %>% select(Individual,H_L_ratio)
ai<- data %>% select(Individual,AI)
vol<- data %>% select(Individual,ingested_volume_mag)
ppd<- data %>% select(Individual,PPDiopen_mag)
time<- data %>% select(Individual,timeatcapture)
velpc<- data %>% select(Individual,VELpreycapture_mag)


# TWO: Run the ICC model 
# x : grouping variable 
# y : measurement 
# data : subset of original split dataset 
# alpha : significance cut off 
# CI.type : THD for unbalanced design

pg.icc <- ICCest(Individual, PG_mag, data = pg, alpha = 0.05, CI.type = c("Smith"))
gprop.icc <- ICCest(Individual, Gape_prop, data = gprop, alpha = 0.05, CI.type = c("Smith"))
tto.icc <- ICCest(Individual, TTO, data = tto, alpha = 0.05, CI.type = c("Smith"))
ttc.icc <- ICCest(Individual, TTC, data = ttc, alpha = 0.05, CI.type = c("Smith"))
pprot.icc <- ICCest(Individual, PPROT_mag, data = pprot, alpha = 0.05, CI.type = c("Smith"))
pprotvel.icc <- ICCest(Individual, PPROTVEL_mag, data = pprotvel, alpha = 0.05, CI.type = c("Smith"))
tpprot.icc <- ICCest(Individual, tPPROT, data = tpprot, alpha = 0.05, CI.type = c("Smith"))
velpg.icc <- ICCest(Individual, VELPG_mag, data = velpg, alpha = 0.05, CI.type = c("Smith"))
maxvel.icc <- ICCest(Individual, maxVEL_mag, data = maxvel, alpha = 0.05, CI.type = c("Smith"))
tmaxvel.icc <- ICCest(Individual, tmaxVEL, data = tmaxvel, alpha = 0.05, CI.type = c("Smith"))
accpg.icc <- ICCest(Individual, ACCPG_mag, data = accpg, alpha = 0.05, CI.type = c("Smith"))
ratio.icc <- ICCest(Individual, H_L_ratio, data = ratio, alpha = 0.05, CI.type = c("Smith"))
ai.icc <- ICCest(Individual, AI, data = ai, alpha = 0.05, CI.type = c("Smith"))
vol.icc <- ICCest(Individual, ingested_volume_mag, data = vol, alpha = 0.05, CI.type = c("Smith"))
ppd.icc <- ICCest(Individual, PPDiopen_mag, data = ppd, alpha = 0.05, CI.type = c("Smith"))
time.icc <- ICCest(Individual, timeatcapture, data = time, alpha = 0.05, CI.type = c("Smith"))
velpc.icc <- ICCest(Individual, VELpreycapture_mag, data = velpc, alpha = 0.05, CI.type = c("Smith"))



# THREE: Export ICC output to DataFrame

ICCResults <- function(Grouping, var){
  mod <- ICCest(Grouping, var, alpha = 0.05, CI.type = c("Smith"))
  data.frame(mod$ICC, mod$LowerCI, mod$UpperCI, mod$N, mod$k, mod$varw, mod$vara)
}

pg.icc <- ICCResults(pg$Individual, pg$PG_mag)
prop.icc <- ICCResults(gprop$Individual, gprop$Gape_prop)
tto.icc <- ICCResults(tto$Individual, tto$TTO)
ttc.icc <- ICCResults(ttc$Individual, ttc$TTC)
pprot.icc <- ICCResults(pprot$Individual, pprot$PPROT_mag)
pprotvel.icc <- ICCResults(pprotvel$Individual, pprotvel$PPROTVEL_mag)
tpprot.icc <- ICCResults(tpprot$Individual, tpprot$tPPROT)
velpg.icc <- ICCResults(velpg$Individual, velpg$VELPG_mag)
maxvel.icc <- ICCResults(maxvel$Individual, maxvel$maxVEL_mag)
tmaxvel.icc <- ICCResults(tmaxvel$Individual, tmaxvel$tmaxVEL)
accpg.icc <- ICCResults(accpg$Individual, accpg$ACCPG_mag)
ratio.icc <- ICCResults(ratio$Individual, ratio$H_L_ratio)
ai.icc <- ICCResults(ai$Individual, ai$AI)
vol.icc <- ICCResults(vol$Individual, vol$ingested_volume_mag)
ppd.icc <- ICCResults(ppd$Individual, ppd$PPDiopen_mag)
time.icc <- ICCResults(time$Individual, time$timeatcapture)
velpc.icc <- ICCResults(velpc$Individual, velpc$VELpreycapture_mag)


#Creating DataFrames 

ICCResults_DF_all <- data.frame(rbind(pg.icc,prop.icc, tto.icc, ttc.icc, pprot.icc, 
                                      pprotvel.icc, tpprot.icc, velpg.icc, 
                                      maxvel.icc, tmaxvel.icc, accpg.icc, 
                                      ratio.icc, ai.icc, vol.icc, ppd.icc, 
                                      time.icc, velpc.icc))

ICCResults_DF_all$variable <- c("PG","Gape_prop","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG",
                                "maxVEL","tmaxVEL","ACCPG","H:L","AI", "ingested_volume", "PPDiopen",
                                "timeatcapture", "VelPreyCapture")

# Write to a csv file 
#NOTE: Rows are doubled because of CI calculations. Variables will also be doubled. 
# Each variable technically gets two rows 
write.csv(ICCResults_DF_all,"ICCResults_all_redbreast_2022.csv")

# remove the variables that have weird variance structures then write updated csv
ICC_all <- ICC_all %>% 
  filter(!row_number() %in% c(7,11,13))

write.csv(ICC_all,"ICCResults_fig_redbreast_2022.csv")

# Read in new data to make figure 
ICC_all <- read.csv("ICCResults_fig_redbreast_2022.csv")

# We manually added grouping factors for each type of variable 
# Feeding, Locomotion, Accuracy 

ICC_all$Category <- factor(ICC_all$Category,levels =c("Feeding","Locomotion","Accuracy"))

#Plotting ICC + Upper/Lower CI

var_break <- c("PG", "Gape_prop","TTO", "TTC", "PPROT", "PPROTVEL", "VELPG", 
               "maxVEL", "tmaxVEL", "timeatcapture", "H:L", "ingested_volume", "PPDiopen", "VelPreyCapture")

ICC_all <- ICC_all[1:14,]


ggplot(ICC_all, aes(x=variable, y=mod.ICC, color=Category))+
  geom_point(aes(size=2))+
  scale_color_manual(values=park_palettes$Everglades)+
  geom_errorbar(aes(ymin=mod.LowerCI, ymax=mod.UpperCI), width=0.5, size=1)+
  geom_hline(yintercept = 0.75, linetype=2)+
  geom_hline(yintercept = 0.50, linetype=2)+
  facet_grid(~Category, scales="free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1), 
        legend.position= "None")+
  xlab("Trait")+
  ylab("Mean ICC estimate")+
  theme(axis.title.x=element_text(face="bold"),
        axis.title.y = element_text(face="bold"))




