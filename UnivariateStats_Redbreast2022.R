# Redbreast project 2022 
# Date: 01/24/2022
# Author(s): Olivia H Hawkins 
# Goals: Univariate stats of prey capture kinematics. ANOVA's are used for each trait with 
  # individual as group. The purpose of this analysis is to determine the repeatability of traits 
  # acros individuals. 

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Load in data 
all.data <- read.csv("FINAL/MagnificationErrorCorrectedData/Redbreast2022_MAG.csv")

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
library(devtools)
devtools::install_github("katiejolly/nationalparkcolors")
library(nationalparkcolors)
library(paletteer)


## One way ANOVA for each kinematic variable. Individuals will be treated as groups ----
# In output, Individuals are first, then Residuals 
# Function to run the anova and extract the results in a data frame 
ResANOVA <- function(Y,Grouping){
  mod <- aov(Y~Grouping)
  mod.sum <- anova(mod)
  data.frame(mod.sum$Df,mod.sum$'Sum Sq',mod.sum$'Mean Sq', mod.sum$'F value',mod.sum$'Pr(>F)')
  
} 


# ANOVA across individuals 
pg.aov <- ResANOVA(all.data$PG_mag,all.data$Individual)
gprop.aov <- ResANOVA(all.data$Gape_prop,all.data$Individual)
tto.aov <- ResANOVA(all.data$TTO,all.data$Individual)
ttc.aov <- ResANOVA(all.data$TTC,all.data$Individual)
pprot.aov <- ResANOVA(all.data$PPROT_mag,all.data$Individual)
pprotvel.aov <- ResANOVA(all.data$PPROTVEL_mag,all.data$Individual)
tpprot.aov <- ResANOVA(all.data$tPPROT,all.data$Individual)
velpg.aov <- ResANOVA(all.data$VELPG_mag,all.data$Individual)
maxvel.aov <- ResANOVA(all.data$maxVEL_mag,all.data$Individual)
tmaxvel.aov <- ResANOVA(all.data$tmaxVEL,all.data$Individual)
accpg.aov <- ResANOVA(all.data$ACCPG_mag,all.data$Individual)
ratio.aov <- ResANOVA(all.data$H_L_ratio,all.data$Individual)
vol.aov <- ResANOVA(all.data$ingested_volume_mag,all.data$Individual)
ppd.aov <- ResANOVA(all.data$PPDiopen_mag,all.data$Individual)
time.aov <- ResANOVA(all.data$timeatcapture,all.data$Individual)
velcapture.aov <- ResANOVA(all.data$VELpreycapture_mag,all.data$Individual)
ai.aov <- ResANOVA(all.data$AI,all.data$Individual)


# Write CSV files with results 
aov <- data.frame(rbind(pg.aov,gprop.aov,tto.aov,ttc.aov,pprot.aov,pprotvel.aov,tpprot.aov,velpg.aov,maxvel.aov,tmaxvel.aov,accpg.aov,ratio.aov,vol.aov,ppd.aov,
                       time.aov,velcapture.aov,ai.aov))
aov$Stat.Designation <- rep(c("Individuals","Residuals"),length.out=34)
colnames(aov) <- c("Df","SumSquares","MeanSumSquares","F.stat","p.value","Stat.Designation")

aov <- aov %>%
  select(Stat.Designation,everything())

write.csv(aov,"ANOVAResults_Redbreast.csv")

# Adjust p values for False Discovery Rate (FDR). This will tell us our proportion of false discoveries ----
  # given the total amount of possible false and true discoveries.

# Get the p values from the dataframe that has empty cells in between
pvals <- aov %>%
  select(p.value)%>%
  slice(seq(1, n(), by =2))

# unlist p values 
pvals <- as.numeric(unlist(pvals)) 

# Get FDR 
fdr.anova <- p.adjust(pvals, method = "fdr", n = length(pvals))
round(fdr.anova,5)


# Make boxplots where dots are included, and color coded by individual ----

# Proportion of peak gape
gape.prop <- ggplot(data, aes(x=Individual, y=Gape_prop,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
theme_classic()+ 
  scale_fill_manual(values=park_palettes$Everglades)+
  ylim(0,1)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        legend.position = "none")+
  ylab("Proportion of anatomical gape at peak gape")
  #stat_compare_means(method = "anova",label.y=1,label.x=4.5)

# Time to mouth opening 
TTO <- ggplot(all.data, aes(x=Individual, y=TTO,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,70)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        legend.position = "bottom")+
  ylab("Time to mouth opening (ms)")
 # stat_compare_means(method = "anova",label.y=70,label.x=4.5)

# Time to mouth closing
TTC <- ggplot(all.data, aes(x=Individual, y=TTC,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,165)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "bottom")+
  ylab("Time to mouth closing (ms)")
 # stat_compare_means(method = "anova",label.y=165,label.x=4.5)

# Peak protrusion
PPROT <- ggplot(all.data, aes(x=Individual, y=PPROT_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,1)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Peak protrusion (cm)")
 # stat_compare_means(method = "anova",label.y=1,label.x=4.5)

# Protrusion velocity
protvel <- ggplot(all.data, aes(x=Individual, y=PPROTVEL_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,70)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Peak protrusion velocity (cm/s)")
  #stat_compare_means(method = "anova",label.y=70,label.x=4.5)

# Time to peak protrusion
tpprot <- ggplot(all.data, aes(x=Individual, y=tPPROT,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(-50,50)+
  geom_hline(yintercept=0,linetype=2)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Timing of peak protrusion (ms)")
  #stat_compare_means(method = "anova",label.y=50,label.x=4.5)

# Velocity at peak gape
velpg <- ggplot(all.data, aes(x=Individual, y=VELPG_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,80)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Velocity at peak gape (cm/s)")
  #stat_compare_means(method = "anova",label.y=80,label.x=4.5)

# Maximum velocity
maxvel <- ggplot(all.data, aes(x=Individual, y=maxVEL_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,120)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Maximum velocity (cm/s)")
 # stat_compare_means(method = "anova",label.y=120,label.x=4.5)

# Time to maximum velocity
tmaxvel <- ggplot(all.data, aes(x=Individual, y=tmaxVEL,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(-60,60)+
  geom_hline(yintercept=0,linetype=2)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Timing of maximum velocity (ms)")
 # stat_compare_means(method = "anova",label.y=60,label.x=4.5)

# Acceleration at peak gape
accpg <- ggplot(all.data, aes(x=Individual, y=ACCPG_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+
  ylim(-5000,5000)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        legend.position = "none")+
  ylab(bquote(bold('Acceleration at peak gape'~(cm/s^2))))
  #stat_compare_means(method = "anova",label.y=5000,label.x=4.5)

# Height to length ratio of ellipsoid volume
hl <- ggplot(all.data, aes(x=Individual, y=H_L_ratio,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  geom_hline(yintercept=1,linetype=2)+
  theme_classic()+ 
  ylim(0,1.5)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Height to length ratio of volume")
 # stat_compare_means(method = "anova",label.y=1.5,label.x=4.5)

# Accuracy Index (AI)
ai <- ggplot(all.data, aes(x=Individual, y=AI,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,1)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Accuracy Index")
 # stat_compare_means(method = "anova",label.y=1,label.x=4.5)

# Ingested volume 
ig <- ggplot(all.data, aes(x=Individual, y=ingested_volume_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,50)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab(bquote(bold("Ingested volume"~(cm^3))))
  #stat_compare_means(method = "anova",label.y=50,label.x=4.5)  

# Predator-Prey distance 
ppd <- ggplot(all.data, aes(x=Individual, y=PPDiopen_mag,fill=Individual)) + # maybe scale to mouth diameter
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,3)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Predator-prey distance at mouth opening (cm)")
  #stat_compare_means(method = "anova",label.y=3,label.x=4.5) 

# Velocity at prey capture
velcap <- ggplot(all.data, aes(x=Individual, y=VELpreycapture_mag,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(0,90)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Velocity at prey capture (cm/s)")
 # stat_compare_means(method = "anova",label.y=90,label.x=4.5)

# Time at prey capture
timap <- ggplot(all.data, aes(x=Individual, y=timeatcapture,fill=Individual)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.8, alpha=0.5)+
  theme_classic()+ 
  ylim(-90,90)+
  geom_hline(yintercept=0,linetype=2)+
  scale_fill_manual(values=park_palettes$Everglades)+
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        legend.position = "none")+
  ylab("Timing of prey capture (ms)")
  #stat_compare_means(method = "anova",label.y=90,label.x=4.5)

# Graph together 
## Feeding 
ggarrange(gape.prop, PPROT, protvel,tpprot,TTO,TTC, 
          labels = c("A", "B", "C", "D", "E","F"),
          ncol = 2, nrow = 3,common.legend = TRUE)

## Locomotion 
ggarrange(accpg, maxvel, tmaxvel,velpg, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,common.legend = TRUE)

## Accuracy 
ggarrange(hl, ai, ig, ppd, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,common.legend = TRUE)


# Density plots for each trait color coded by individual ----

#FEEDING 
ggplot(data=all.data.ind, aes(x=PG_scale_ind,group=Individual, fill=Individual))  +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  xlab("Scaled Peak gape")+
  xlim(-3,3)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14)) # use

ggplot(data=all.data, aes(x=TTO ,group=Individual, fill=Individual))+
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth opening (ms)")

ggplot(data=all.data, aes(x=TTC ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7) +
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth closing (ms)")+
  xlim(10,150)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        legend.position="none")


ggplot(data=all.data, aes(x=PPROT ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7) +
  theme_classic()+
  ylab("Density")+
  xlab("Peak protrusion (cm)")


ggplot(data=all.data, aes(x=PPROTVEL ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density") +
  xlab("Protrusion velocity (cm/s)")


ggplot(data=all.data, aes(x=tPPROT ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  xlab("Timing of peak protrusion (ms)")


# SWIMMING 
ggplot(data=all.data.ind, aes(x=VELPG_scale_ind ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density") +
  xlab("Scaled velocity at peak gape")+
  xlim(-3,3)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14))# use


ggplot(data=all.data, aes(x=maxVEL_mag ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density") +
  xlab("Maximum velocity (cm/s)")+
  xlim(0,125)


ggplot(data=all.data, aes(x=tmaxVEL ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  xlab("Timing of maximum velocity (ms)")+
  xlim(-100,60)


ggplot(data=all.data, aes(x=ACCPG_mag ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  labs(x=bquote('Acceleration at peak gape'~(cm/s^2)))


# ACCURACY 
ggplot(data=all.data, aes(x=H_L_ratio ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  xlab("Height:Length of volume")


ggplot(data=all.data, aes(x=AI ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.7)+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")+
  xlim(0,1)+
  theme(axis.title.x = element_text(face="bold",size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.position = "none")


ggplot(data=all.data, aes(x=ingested_volume_mag ,group=Individual, fill=Individual)) +
  scale_fill_manual(values=park_palettes$Everglades)+
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  labs(x=bquote('Ingested volume'~(cm^3)))


ggplot(data=all.data, aes(x=PPDiopen_mag ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Predator-prey distance at mouth opening (cm)")


ggplot(data=all.data, aes(x=timeatcapture ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Time at capture relative to peak gape (ms)")


ggplot(data=all.data, aes(x=VELpreycapture ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Velocity at prey capture (cm/s)")