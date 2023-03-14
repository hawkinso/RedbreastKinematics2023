# Redbreast project 2022 
# Date: 11/23/2021, updated 01/12/2022
# Author(s): Olivia H Hawkins 
# Goals: Clean and summarize data, check statistical assumptions, make any transformations
  # if needed.

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Info about data: 
# Sex: LAUR01 and LAUR04 are male, LAUR02, LAUR03, and LAUR05 are female 

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

# Read in data 
# This data sheet is available at: browse.URL("")
data <- read.csv("Redbreast2022_MAG.csv")

# Subset data that will be used in analysis ----
# Individual
all.data <- data %>%
  dplyr::select(Individual,SL_mag,PG_mag,Gape_prop,TTO,TTC,PPROT_mag,PPROTVEL_mag,tPPROT,VELPG_mag,maxVEL_mag,tmaxVEL,ACCPG_mag,H_L_ratio,AI,ingested_volume_mag,PPDiopen_mag,timeatcapture,VELpreycapture_mag)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual) 

all.data.shap <-  data %>%
  dplyr::select(Individual,PG_mag,TTO,TTC,PPROT_mag,PPROTVEL_mag,tPPROT,VELPG_mag,maxVEL_mag,tmaxVEL,ACCPG_mag,H_L_ratio,AI,ingested_volume_mag,PPDiopen_mag,timeatcapture,VELpreycapture_mag)%>%
  group_by(Individual)%>%
  convert_as_factor(Individual) 

all.data.stat <- data %>%
  dplyr::select(SL_mag,PG_mag,Gape_prop,TTO,TTC,PPROT_mag,PPROTVEL_mag,tPPROT,VELPG_mag,maxVEL_mag,tmaxVEL,ACCPG_mag,H_L_ratio,AI,ingested_volume_mag,PPDiopen_mag,timeatcapture,VELpreycapture_mag) %>% 
  get_summary_stats()

# Make sure data is stored as data frame 
as.data.frame(all.data)

# Check out data structure 
str(all.data)

# Get group means and sd for each individual ----
means <- all.data %>%
  get_summary_stats()

# Get standard length (mean +/- SD): 9.25 +/- 0.97 cm 
mean(all.data$SL_mag) #9.25
sd(all.data$SL_mag) # 0.97

# Write to .csv 
write_csv(means,file = "Redbreast_summarystatXindividual_2021.csv",append = FALSE)
write_csv(all.data.stat, file="Redbreast_summarystatsALL_2022.csv",append=FALSE)

# Check assumptions ----
# Normality, homogeneity of variance,independence of observations
# Independence of observations is not met as each individual is sampled 20 times 


# Check normality with Shapiro-Wilk test 
## By individuals 
sw <- ddply(.data=all.data.shap, .variables=c("Individual"),numcolwise(shapiro.test))
sw <- sw[-c(3:4,7:8,11:12,15:16,19:20),]

sw.results <- gather(data = sw,key = Variable, value=Results,2:17)

# Add column to show what the value is (W statistic and p value)
sw.results$Value <- rep(c("W statistic","p value"))

# Find the individual x variable combination that violates normality 
sw.results2 <- sw.results %>%
   group_by(Individual)%>%
    filter(Value=="p value")%>% 
  filter(Results<0.05)

# Export data 
write.csv(sw.results,"Shapiro.wilk_Redbreast2022.csv")

# Visualize normality by individual
ggqqplot(data=all.data, x = ("PG_mag"),
         color = "Individual",facet.by="Individual") # pretty good
ggqqplot(all.data, x = "TTO",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "TTC",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROT_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "PPROTVEL_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tPPROT",
         color = "Individual",facet.by = "Individual") # ok
ggqqplot(all.data, x = "VELPG_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "maxVEL_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "tmaxVEL",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ACCPG_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "H_L_ratio",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "AI",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "ingested_volume_mag",
         color = "Individual",facet.by = "Individual") # ok 
ggqqplot(all.data, x = "PPDiopen_mag",
         color = "Individual",facet.by = "Individual") # pretty good
ggqqplot(all.data, x = "timeatcapture",
         color = "Individual",facet.by = "Individual") # pretty good 
ggqqplot(all.data, x = "VELpreycapture_mag",
         color = "Individual",facet.by = "Individual") # pretty good

# Identify outliers 

# Individual 1 
Fish.1 <- all.data %>%
  filter(Individual=="LAUR01")

Fish.1 %>% select(PG_mag) %>% identify_outliers() 
Fish.1 %>% select(TTO) %>% identify_outliers() 
Fish.1 %>% select(TTC) %>% identify_outliers()
Fish.1 %>% select(PPROT_mag) %>% identify_outliers()
Fish.1 %>% select(PPROTVEL_mag) %>% identify_outliers()  
Fish.1 %>% select(tPPROT) %>% identify_outliers() # one extreme 
Fish.1 %>% select(VELPG_mag) %>% identify_outliers()
Fish.1 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.1 %>% select(tmaxVEL) %>% identify_outliers() 
Fish.1 %>% select(ACCPG_mag) %>% identify_outliers() # one extreme 
Fish.1 %>% select(H_L_ratio) %>% identify_outliers()
Fish.1 %>% select(AI) %>% identify_outliers()
Fish.1 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.1 %>% select(PPDiopen_mag) %>% identify_outliers()
Fish.1 %>% select(timeatcapture) %>% identify_outliers()
Fish.1 %>% select(VELpreycapture_mag) %>% identify_outliers()


# Individual 2 
Fish.2 <- all.data %>%
  filter(Individual=="LAUR02")

Fish.2 %>% select(PG_mag) %>% identify_outliers() 
Fish.2 %>% select(TTO) %>% identify_outliers() 
Fish.2 %>% select(TTC) %>% identify_outliers()
Fish.2 %>% select(PPROT_mag) %>% identify_outliers() # one extreme
Fish.2 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.2 %>% select(tPPROT) %>% identify_outliers() 
Fish.2 %>% select(VELPG_mag) %>% identify_outliers()
Fish.2 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.2 %>% select(tmaxVEL) %>% identify_outliers() 
Fish.2 %>% select(ACCPG_mag) %>% identify_outliers() # one extreme 
Fish.2 %>% select(H_L_ratio) %>% identify_outliers()
Fish.2 %>% select(AI) %>% identify_outliers()
Fish.2 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.2 %>% select(PPDiopen_mag) %>% identify_outliers() 
Fish.2 %>% select(timeatcapture) %>% identify_outliers()
Fish.2 %>% select(VELpreycapture_mag) %>% identify_outliers()


# Individual 3
Fish.3 <- all.data %>%
  filter(Individual=="LAUR03")

Fish.3 %>% select(PG_mag) %>% identify_outliers()  
Fish.3 %>% select(TTO) %>% identify_outliers() 
Fish.3 %>% select(TTC) %>% identify_outliers()
Fish.3 %>% select(PPROT_mag) %>% identify_outliers() 
Fish.3 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.3 %>% select(tPPROT) %>% identify_outliers() 
Fish.3 %>% select(VELPG_mag) %>% identify_outliers()
Fish.3 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.3 %>% select(tmaxVEL) %>% identify_outliers() 
Fish.3 %>% select(ACCPG_mag) %>% identify_outliers() 
Fish.3 %>% select(H_L_ratio) %>% identify_outliers()
Fish.3 %>% select(AI) %>% identify_outliers()
Fish.3 %>% select(ingested_volume_mag) %>% identify_outliers() 
Fish.3 %>% select(PPDiopen_mag) %>% identify_outliers() 
Fish.3 %>% select(timeatcapture) %>% identify_outliers()
Fish.3 %>% select(VELpreycapture_mag) %>% identify_outliers()

# Individual 4 
Fish.4 <- all.data %>%
  filter(Individual=="LAUR04")

Fish.4 %>% select(PG_mag) %>% identify_outliers()  
Fish.4 %>% select(TTO) %>% identify_outliers() 
Fish.4 %>% select(TTC) %>% identify_outliers()
Fish.4 %>% select(PPROT_mag) %>% identify_outliers() 
Fish.4 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.4 %>% select(tPPROT) %>% identify_outliers()  
Fish.4 %>% select(VELPG_mag) %>% identify_outliers()
Fish.4 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.4 %>% select(tmaxVEL) %>% identify_outliers()  
Fish.4 %>% select(ACCPG_mag) %>% identify_outliers() # one extreme
Fish.4 %>% select(H_L_ratio) %>% identify_outliers()
Fish.4 %>% select(AI) %>% identify_outliers()
Fish.4 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.4 %>% select(PPDiopen_mag) %>% identify_outliers()  
Fish.4 %>% select(timeatcapture) %>% identify_outliers()
Fish.4 %>% select(VELpreycapture_mag) %>% identify_outliers()

# Individual 5 
Fish.5 <- all.data %>%
  filter(Individual=="LAUR05")

Fish.5 %>% select(PG_mag) %>% identify_outliers()  
Fish.5 %>% select(TTO) %>% identify_outliers() 
Fish.5 %>% select(TTC) %>% identify_outliers()
Fish.5 %>% select(PPROT_mag) %>% identify_outliers() 
Fish.5 %>% select(PPROTVEL_mag) %>% identify_outliers() 
Fish.5 %>% select(tPPROT) %>% identify_outliers() 
Fish.5 %>% select(VELPG_mag) %>% identify_outliers()
Fish.5 %>% select(maxVEL_mag) %>% identify_outliers()
Fish.5 %>% select(tmaxVEL) %>% identify_outliers()  
Fish.5 %>% select(ACCPG_mag) %>% identify_outliers() 
Fish.5 %>% select(H_L_ratio) %>% identify_outliers()
Fish.5 %>% select(AI) %>% identify_outliers()
Fish.5 %>% select(ingested_volume_mag) %>% identify_outliers()
Fish.5 %>% select(PPDiopen_mag) %>% identify_outliers() 
Fish.5 %>% select(timeatcapture) %>% identify_outliers()
Fish.5 %>% select(VELpreycapture_mag) %>% identify_outliers()

# Check homogeneity of variance with Levene's test across all individuals 
leveneTest(all.data$PG_mag~all.data$Individual) # 0.15
leveneTest(all.data$TTO~all.data$Individual) # 0.26
leveneTest(all.data$TTC~all.data$Individual) # 0.2
leveneTest(all.data$PPROT_mag~all.data$Individual) # 0.13
leveneTest(all.data$PPROTVEL_mag~all.data$Individual) # p < 0.0001
leveneTest(all.data$tPPROT~all.data$Individual) # p = 0.25
leveneTest(all.data$VELPG_mag~all.data$Individual) # p < 0.0001
leveneTest(all.data$maxVEL_mag~all.data$Individual) # p = 0.0001
leveneTest(all.data$tmaxVEL~all.data$Individual) # 0.4 
leveneTest(all.data$ACCPG_mag~all.data$Individual) # p = 0.0002
leveneTest(all.data$H_L_ratio~all.data$Individual)#  p =0.23
leveneTest(all.data$AI~all.data$Individual) #p = 0.64
leveneTest(all.data$ingested_volume_mag~all.data$Individual) # P < 0.0001
leveneTest(all.data$PPDiopen_mag~all.data$Individual) # p = 0.04
leveneTest(all.data$timeatcapture~all.data$Individual) # p = 0.54
leveneTest(all.data$VELpreycapture_mag~all.data$Individual) # p = 0.0001


# Check that size is similar among individuals ----
# We want to be sure that size does not influence any variables 
ggboxplot(all.data, x = "Individual", y = "SL", add = "point")

# Check assumptions
# Outliers
SL <- all.data %>%  # no extreme outliers 
  group_by(Individual) %>%
  select(SL) %>%
  identify_outliers()

# Normality 
shapiro.test(all.data$SL) # p = 0.01 
ggqqplot(all.data$SL) # looks ok 

# General linear mixed model 
# Some variables are influenced by size... we will need to scale variables by standard length
PGmod.SL <- lmer(PG_mag~SL_mag+(1|Individual),data=all.data)
summary(PGmod.SL) 

GProp.SL <- lmer(Gape_prop~SL_mag+(1|Individual),data=all.data)
summary(GProp.SL) 

TTOmod.SL <- lmer(TTO~SL_mag+(1|Individual),data=all.data)
summary(TTOmod.SL) 

TTCmod.SL <- lmer(TTC~SL_mag+(1|Individual),data=all.data)
summary(TTCmod.SL) 

PPROTmod.SL <- lmer(PPROT_mag~SL_mag+(1|Individual),data=all.data)
summary(PPROTmod.SL)  # p = 0.04

PPROTVELmod.SL <- lmer(PPROTVEL_mag~SL_mag+(1|Individual),data=all.data)
summary(PPROTVELmod.SL) 

tPPROTmod.SL <- lmer(tPPROT~SL_mag+(1|Individual),data=all.data)
summary(tPPROTmod.SL)

VELPGmod.SL <- lmer(VELPG_mag~SL_mag+(1|Individual),data=all.data)
summary(VELPGmod.SL) 

maxVELmod.SL <- lmer(maxVEL_mag~SL_mag+(1|Individual),data=all.data)
summary(maxVELmod.SL) 

tmaxVELmod.SL <- lmer(tmaxVEL~SL_mag+(1|Individual),data=all.data)
summary(tmaxVELmod.SL) 

ACCPGmod.SL <- lmer(ACCPG_mag~SL_mag+(1|Individual),data=all.data)
summary(ACCPGmod.SL) 

HLmod.SL <- lmer(H_L_ratio~SL_mag+(1|Individual),data=all.data)
summary(HLmod.SL) 

AImod.SL <- lmer(AI~SL_mag+(1|Individual),data=all.data)
summary(AImod.SL) 

ingestedmod.SL <- lmer(ingested_volume_mag~SL_mag+(1|Individual),data=all.data)
summary(ingestedmod.SL) 

PPDiopenmod.SL <- lmer(PPDiopen_mag~SL_mag+(1|Individual),data=all.data)
summary(PPDiopenmod.SL) 

timeatcapturemod.SL <- lmer(timeatcapture~SL_mag+(1|Individual),data=all.data)
summary(timeatcapturemod.SL)

VELpreycapturemod.SL <- lmer(VELpreycapture_mag~SL_mag+(1|Individual),data=all.data)
summary(VELpreycapturemod.SL)


# Diagnostic plots ---- 

# Use histogram overlaps 
ggplot(data=all.data.ind, aes(x=PG_scale_ind,group=Individual, fill=Individual))  +
  scale_fill_brewer(palette="Dark2")+
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Scaled Peak gape")+
  xlim(-3,3)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14)) # use

ggplot(data=all.data, aes(x=TTO ,group=Individual, fill=Individual))
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth opening (ms)")

ggplot(data=all.data, aes(x=TTC ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()+
  ylab("Density")+
  xlab("Duration of mouth closing (ms)")+
  xlim(10,150)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14))


ggplot(data=all.data, aes(x=PPROT ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_classic()+
  ylab("Density")+
  xlab("Peak protrusion (cm)")


ggplot(data=all.data, aes(x=PPROTVEL ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Protrusion velocity (cm/s)")


ggplot(data=all.data, aes(x=tPPROT ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Timing of peak protrusion (ms)")

ggplot(data=all.data.ind, aes(x=VELPG_scale_ind ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Scaled velocity at peak gape")+
  xlim(-3,3)+
  theme(legend.position = "none",
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14))# use


ggplot(data=all.data, aes(x=maxVEL ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  xlab("Maximum velocity (cm/s)")


ggplot(data=all.data, aes(x=tmaxVEL ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Timing of maximum velocity (ms)")


ggplot(data=all.data, aes(x=ACCPG ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  labs(x=bquote('Acceleration at peak gape'~(cm/s^2)))

ggplot(data=all.data, aes(x=H_L_ratio ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Height:Length of volume")


ggplot(data=all.data, aes(x=AI ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density")+
  xlab("Accuracy Index")+
  xlim(0,1)# use 


ggplot(data=all.data, aes(x=ingested_volume ,group=Individual, fill=Individual)) +
  geom_density(adjust=1.5, alpha=.4)+
  theme_classic()+
  ylab("Density") +
  labs(x=bquote('Ingested volume'~(cm^3)))


ggplot(data=all.data, aes(x=PPDiopen ,group=Individual, fill=Individual)) +
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

# Morphology data ---- 
measures <- read.csv("MorphologyMeasurements_Redbreast_2022_averaged.csv")

data_merged <- merge(all.data, measures, by= "Individual")
data_merged$Gape_prop <- data_merged$PG/data_merged$Gape_height

ggplot(data=data, aes(x=PG_mag, y=Gape_prop ,colour=Individual, fill=Individual)) +
  geom_point() +
  theme_classic()+
  xlab("Peak Gape")+
  ylab("Proportion of Max Gape")+
  theme(axis.title.x = element_text(face="bold",size=12),
        axis.title.y = element_text(face="bold",size=12),
        legend.position = "top",
        legend.title = element_text(face="bold",size=12),
        legend.box.background = element_rect(color = "black"),
        legend.box.margin = margin(t = 1, l = 1))

ggplot(data=data, aes(x=SL_measured, y=Gape_height ,colour=Individual, fill=Individual)) +
  geom_point() +
  theme_classic()+
  xlab("Standard Length")+
  ylab("Max Anatomical Gape")


