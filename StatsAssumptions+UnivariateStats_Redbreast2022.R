# Redbreast project 2022 
# Date: 01/24/2022
# Author(s): Olivia H Hawkins 
# Goals: Checking assumptions of each dataset (small mouth vs large mouth) and running basic univariate statistics 

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

# Load in data sets --- 
small <- read.csv("SmallMouth_Redbreast2022.csv")
large <- read.csv("LargeMouth_Redbreast2022.csv")

# Make sure data is grouped by individual 
smsmall.data <- small %>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

large.data <- large %>%
  group_by(Individual)%>%
  convert_as_factor(Individual)

#Get group means and sd for each individual ----
means.small <- smsmall.data %>%
  get_summary_stats()

means.large <- large.data %>%
  get_summary_stats()

# Write csv files 
write.csv(means.small,file = "Means_SmallMouth_Redbreast2022.csv")
write.csv(means.large,file = "Means_LargeMouth_Redbreast2022.csv")


# Statistical assumptions: Small mouth ---- 
# Normality:: By individual 
ggqqplot(data=small.data, x = ("PG"),
         color = "Individual",facet.by="Individual") 
ggqqplot(small.data, x = "TTO",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "TTC",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "PPROT",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "PPROTVEL",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "tPPROT",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "VELPG",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "maxVEL",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "tmaxVEL",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "ACCPG",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "H_L_ratio",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "AI",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "ingested_volume",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "PPDiopen",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "timeatcapture",
         color = "Individual",facet.by = "Individual") 
ggqqplot(small.data, x = "VELpreycapture",
         color = "Individual",facet.by = "Individual") 

# Normality:: Whole data set visualization 
ggqqplot(small.data, x = "PG")
ggqqplot(small.data, x = "TTO") 
ggqqplot(small.data, x = "TTC") 
ggqqplot(small.data, x = "PPROT") 
ggqqplot(small.data, x = "PPROTVEL") 
ggqqplot(small.data, x = "tPPROT") 
ggqqplot(small.data, x = "VELPG") 
ggqqplot(small.data, x = "maxVEL") 
ggqqplot(small.data, x = "tmaxVEL") 
ggqqplot(small.data, x = "ACCPG") 
ggqqplot(small.data, x = "H_L_ratio") 
ggqqplot(small.data, x = "AI") 
ggqqplot(small.data, x = "ingested_volume") 
ggqqplot(small.data, x = "PPDiopen") 
ggqqplot(small.data, x = "timeatcapture") 
ggqqplot(small.data, x = "VELpreycapture") 

# Extreme outlier identification by individual
# Individual 1 
Fish.1 <- small.data %>%
  filter(Individual=="LAUR01")

Fish.1 %>% select(PG) %>% identify_outliers()  
Fish.1 %>% select(TTO) %>% identify_outliers() 
Fish.1 %>% select(TTC) %>% identify_outliers(TTC)
Fish.1 %>% select(PPROT) %>% identify_outliers(PPROT)
Fish.1 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.1 %>% select(tPPROT) %>% identify_outliers(tPPROT)  
Fish.1 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.1 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.1 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.1 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.1 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.1 %>% select(AI) %>% identify_outliers(AI)
Fish.1 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.1 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)
Fish.1 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.1 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)


# Individual 2 
Fish.2 <- small.data %>%
  filter(Individual=="LAUR02")

Fish.2 %>% select(PG) %>% identify_outliers() 
Fish.2 %>% select(TTO) %>% identify_outliers() 
Fish.2 %>% select(TTC) %>% identify_outliers(TTC)
Fish.2 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.2 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.2 %>% select(tPPROT) %>% identify_outliers(tPPROT)  
Fish.2 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.2 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.2 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.2 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.2 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.2 %>% select(AI) %>% identify_outliers(AI)
Fish.2 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.2 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
Fish.2 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.2 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)


# Individual 3
Fish.3 <- small.data %>%
  filter(Individual=="LAUR03")

Fish.3 %>% select(PG) %>% identify_outliers()  
Fish.3 %>% select(TTO) %>% identify_outliers() 
Fish.3 %>% select(TTC) %>% identify_outliers(TTC)
Fish.3 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.3 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.3 %>% select(tPPROT) %>% identify_outliers(tPPROT) 
Fish.3 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.3 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.3 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL) 
Fish.3 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.3 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.3 %>% select(AI) %>% identify_outliers(AI)
Fish.3 %>% select(ingested_volume) %>% identify_outliers(ingested_volume) 
Fish.3 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)  
Fish.3 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.3 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Individual 4 
Fish.4 <- small.data %>%
  filter(Individual=="LAUR04")

Fish.4 %>% select(PG) %>% identify_outliers()  
Fish.4 %>% select(TTO) %>% identify_outliers() 
Fish.4 %>% select(TTC) %>% identify_outliers(TTC)
Fish.4 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.4 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.4 %>% select(tPPROT) %>% identify_outliers(tPPROT)  
Fish.4 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.4 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.4 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.4 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.4 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.4 %>% select(AI) %>% identify_outliers(AI)
Fish.4 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.4 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)  
Fish.4 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.4 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Indivdual 5 
Fish.5 <- small.data %>%
  filter(Individual=="LAUR05")

Fish.5 %>% select(PG) %>% identify_outliers()  
Fish.5 %>% select(TTO) %>% identify_outliers() 
Fish.5 %>% select(TTC) %>% identify_outliers(TTC) # one extreme 
Fish.5 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.5 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.5 %>% select(tPPROT) %>% identify_outliers(tPPROT) 
Fish.5 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.5 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.5 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  # one extreme
Fish.5 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.5 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.5 %>% select(AI) %>% identify_outliers(AI)
Fish.5 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.5 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
Fish.5 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.5 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Extreme outlier identification for all observations 
small.data %>% select(PG) %>% identify_outliers()  # one extreme
small.data %>% select(TTO) %>% identify_outliers() # one extreme
small.data %>% select(TTC) %>% identify_outliers(TTC) 
small.data %>% select(PPROT) %>% identify_outliers(PPROT) 
small.data %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
small.data %>% select(tPPROT) %>% identify_outliers(tPPROT) 
small.data %>% select(VELPG) %>% identify_outliers(VELPG)
small.data %>% select(maxVEL) %>% identify_outliers(maxVEL)
small.data %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  # one extreme
small.data %>% select(ACCPG) %>% identify_outliers(ACCPG) 
small.data %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
small.data %>% select(AI) %>% identify_outliers(AI)
small.data %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
small.data %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
small.data %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
small.data %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Homogeneity:: Levene's test for individuals 
leveneTest(small.data$PG~small.data$Individual)
leveneTest(small.data$TTO~small.data$Individual)
leveneTest(small.data$TTC~small.data$Individual)
leveneTest(small.data$PPROT~small.data$Individual)
leveneTest(small.data$PPROTVEL~small.data$Individual) # 0.001
leveneTest(small.data$tPPROT~small.data$Individual) 
leveneTest(small.data$VELPG~small.data$Individual) 
leveneTest(small.data$maxVEL~small.data$Individual) # 0.02
leveneTest(small.data$tmaxVEL~small.data$Individual)
leveneTest(small.data$ACCPG~small.data$Individual)
leveneTest(small.data$H_L_ratio~small.data$Individual)
leveneTest(small.data$AI~small.data$Individual)
leveneTest(small.data$ingested_volume~small.data$Individual) # 0.008
leveneTest(small.data$PPDiopen~small.data$Individual) 
leveneTest(small.data$timeatcapture~small.data$Individual) 
leveneTest(small.data$VELpreycapture~small.data$Individual) 

# Statistical assumptions: Large mouth ---- 
# Normality:: By individual 
ggqqplot(data=large.data, x = ("PG"),
         color = "Individual",facet.by="Individual") 
ggqqplot(large.data, x = "TTO",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "TTC",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "PPROT",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "PPROTVEL",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "tPPROT",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "VELPG",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "maxVEL",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "tmaxVEL",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "ACCPG",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "H_L_ratio",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "AI",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "ingested_volume",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "PPDiopen",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "timeatcapture",
         color = "Individual",facet.by = "Individual") 
ggqqplot(large.data, x = "VELpreycapture",
         color = "Individual",facet.by = "Individual") 

# Normality:: Whole data set visualization 
ggqqplot(large.data, x = "PG")
ggqqplot(large.data, x = "TTO") 
ggqqplot(large.data, x = "TTC") 
ggqqplot(large.data, x = "PPROT") 
ggqqplot(large.data, x = "PPROTVEL") 
ggqqplot(large.data, x = "tPPROT") 
ggqqplot(large.data, x = "VELPG") 
ggqqplot(large.data, x = "maxVEL") 
ggqqplot(large.data, x = "tmaxVEL") 
ggqqplot(large.data, x = "ACCPG") 
ggqqplot(large.data, x = "H_L_ratio") 
ggqqplot(large.data, x = "AI") 
ggqqplot(large.data, x = "ingested_volume") 
ggqqplot(large.data, x = "PPDiopen") 
ggqqplot(large.data, x = "timeatcapture") 
ggqqplot(large.data, x = "VELpreycapture")

# Extreme outlier identification by individual
# Individual 1 
Fish.1 <- large.data %>%
  filter(Individual=="LAUR01")

Fish.1 %>% select(PG) %>% identify_outliers()  
Fish.1 %>% select(TTO) %>% identify_outliers() 
Fish.1 %>% select(TTC) %>% identify_outliers(TTC)
Fish.1 %>% select(PPROT) %>% identify_outliers(PPROT)
Fish.1 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.1 %>% select(tPPROT) %>% identify_outliers(tPPROT)  # one extreme
Fish.1 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.1 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.1 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.1 %>% select(ACCPG) %>% identify_outliers(ACCPG) # one extreme
Fish.1 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.1 %>% select(AI) %>% identify_outliers(AI)
Fish.1 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.1 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)
Fish.1 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.1 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)


# Individual 2 
Fish.2 <- large.data %>%
  filter(Individual=="LAUR02")

Fish.2 %>% select(PG) %>% identify_outliers() 
Fish.2 %>% select(TTO) %>% identify_outliers() 
Fish.2 %>% select(TTC) %>% identify_outliers(TTC)
Fish.2 %>% select(PPROT) %>% identify_outliers(PPROT) # two extreme 
Fish.2 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.2 %>% select(tPPROT) %>% identify_outliers(tPPROT) # one extreme  
Fish.2 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.2 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.2 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.2 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.2 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.2 %>% select(AI) %>% identify_outliers(AI)
Fish.2 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.2 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
Fish.2 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.2 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)


# Individual 3
Fish.3 <- large.data %>%
  filter(Individual=="LAUR03")

Fish.3 %>% select(PG) %>% identify_outliers()  
Fish.3 %>% select(TTO) %>% identify_outliers() 
Fish.3 %>% select(TTC) %>% identify_outliers(TTC)
Fish.3 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.3 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.3 %>% select(tPPROT) %>% identify_outliers(tPPROT) 
Fish.3 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.3 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.3 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL) 
Fish.3 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.3 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.3 %>% select(AI) %>% identify_outliers(AI)
Fish.3 %>% select(ingested_volume) %>% identify_outliers(ingested_volume) # one extreme 
Fish.3 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)  
Fish.3 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.3 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Individual 4 
Fish.4 <- large.data %>%
  filter(Individual=="LAUR04")

Fish.4 %>% select(PG) %>% identify_outliers()  
Fish.4 %>% select(TTO) %>% identify_outliers() 
Fish.4 %>% select(TTC) %>% identify_outliers(TTC)
Fish.4 %>% select(PPROT) %>% identify_outliers(PPROT) 
Fish.4 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.4 %>% select(tPPROT) %>% identify_outliers(tPPROT)  
Fish.4 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.4 %>% select(maxVEL) %>% identify_outliers(maxVEL) #one extreme 
Fish.4 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.4 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.4 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.4 %>% select(AI) %>% identify_outliers(AI)
Fish.4 %>% select(ingested_volume) %>% identify_outliers(ingested_volume)
Fish.4 %>% select(PPDiopen) %>% identify_outliers(PPDiopen)  
Fish.4 %>% select(timeatcapture) %>% identify_outliers(timeatcapture)
Fish.4 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Indivdual 5 
Fish.5 <- large.data %>%
  filter(Individual=="LAUR05")

Fish.5 %>% select(PG) %>% identify_outliers()  
Fish.5 %>% select(TTO) %>% identify_outliers() 
Fish.5 %>% select(TTC) %>% identify_outliers(TTC) 
Fish.5 %>% select(PPROT) %>% identify_outliers(PPROT) # one extreme 
Fish.5 %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
Fish.5 %>% select(tPPROT) %>% identify_outliers(tPPROT) 
Fish.5 %>% select(VELPG) %>% identify_outliers(VELPG)
Fish.5 %>% select(maxVEL) %>% identify_outliers(maxVEL)
Fish.5 %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
Fish.5 %>% select(ACCPG) %>% identify_outliers(ACCPG) 
Fish.5 %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
Fish.5 %>% select(AI) %>% identify_outliers(AI)
Fish.5 %>% select(ingested_volume) %>% identify_outliers(ingested_volume) # one extreme
Fish.5 %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
Fish.5 %>% select(timeatcapture) %>% identify_outliers(timeatcapture) # two extreme
Fish.5 %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)

# Extreme outlier identification for all observations 
large.data %>% select(PG) %>% identify_outliers()  
large.data %>% select(TTO) %>% identify_outliers() 
large.data %>% select(TTC) %>% identify_outliers(TTC) 
large.data %>% select(PPROT) %>% identify_outliers(PPROT) # three extreme
large.data %>% select(PPROTVEL) %>% identify_outliers(PPROTVEL) 
large.data %>% select(tPPROT) %>% identify_outliers(tPPROT) # two extreme
large.data %>% select(VELPG) %>% identify_outliers(VELPG)
large.data %>% select(maxVEL) %>% identify_outliers(maxVEL) # one extreme
large.data %>% select(tmaxVEL) %>% identify_outliers(tmaxVEL)  
large.data %>% select(ACCPG) %>% identify_outliers(ACCPG) # one extreme
large.data %>% select(H_L_ratio) %>% identify_outliers(H_L_ratio)
large.data %>% select(AI) %>% identify_outliers(AI) # one extreme
large.data %>% select(ingested_volume) %>% identify_outliers(ingested_volume) # one extreme
large.data %>% select(PPDiopen) %>% identify_outliers(PPDiopen) 
large.data %>% select(timeatcapture) %>% identify_outliers(timeatcapture) # two extreme
large.data %>% select(VELpreycapture) %>% identify_outliers(VELpreycapture)
 
# Homogeneity:: Levene's test for individuals 
leveneTest(large.data$PG~large.data$Individual) # 0.04
leveneTest(large.data$TTO~large.data$Individual)
leveneTest(large.data$TTC~large.data$Individual)
leveneTest(large.data$PPROT~large.data$Individual)
leveneTest(large.data$PPROTVEL~large.data$Individual) # 0.006
leveneTest(large.data$tPPROT~large.data$Individual) 
leveneTest(large.data$VELPG~large.data$Individual) # 0.04
leveneTest(large.data$maxVEL~large.data$Individual) 
leveneTest(large.data$tmaxVEL~large.data$Individual)
leveneTest(large.data$ACCPG~large.data$Individual)
leveneTest(large.data$H_L_ratio~large.data$Individual)
leveneTest(large.data$AI~large.data$Individual)
leveneTest(large.data$ingested_volume~large.data$Individual) # p < 0.0001
leveneTest(large.data$PPDiopen~large.data$Individual) 
leveneTest(large.data$timeatcapture~large.data$Individual) 
leveneTest(large.data$VELpreycapture~large.data$Individual) # 0.009



