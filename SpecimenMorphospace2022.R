# Redbreast project 2022 
# Date: 05/17/2022 
# Author(s): Olivia H Hawkins
# Goals: Make morphospace based off Collar and Wainwright (2009) using collection
  # specimens from Louisiana State University Natural History Museum (LSUMZ).

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
library(reshape2)
library(lmer4)
library(RColorBrewer)
library(lmerTest)
library(reshape2)
library(rptR)
library(ggridges)
library(ICC)
library(cowplot)
library(plotrix)
library(nationalparkcolors)

# Load in data 
data <- read.csv("SpecimenMeans_2022.csv")

# Summarize the data for each species irrespective of collection ID and state----

species.sum <- data %>% 
  select(Sci.name,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  convert_as_factor(Sci.name) %>%
  group_by(Sci.name) %>%
  get_summary_stats()

species.sum.fig <- species.sum %>%
  select(Sci.name,variable,mean,sd)%>%
  convert_as_factor(Sci.name)

means <- data %>%
  filter(Collection=="LSU")%>%
  group_by(Sci.name) %>%
  summarise(Mean.D.L=mean(Mean.D.L.ratio),
            sd.DL = sd(Mean.D.L.ratio),
            Mean.pos = mean(Mean.dorsal.fin.position),
            sd.pos = sd(Mean.dorsal.fin.position),
            se.DL = std.error(Mean.D.L.ratio),
            se.pos = std.error(Mean.dorsal.fin.position))
  
means$Genus <- c("Ambloplites","Ambloplites","Centrarchus","Lepomis","Lepomis","Lepomis",
                         "Lepomis","Lepomis","Lepomis","Lepomis","Lepomis","Lepomis","Lepomis",
                         "Lepomis","Micropterus","Micropterus","Pomoxis","Pomoxis")
# Summarize the data for each species with collection in mind and not accounting for state ----

# EAK Collection
species.sum.EAK <- data %>% 
  filter(Collection=="EAK") %>% 
  select(Sci.name,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  convert_as_factor(Sci.name) %>%
  group_by(Sci.name) %>%
  get_summary_stats()

means.EAK <- data %>%
  group_by(Sci.name) %>%
  filter(Collection=="EAK") %>% 
  summarise(Mean.D.L=mean(Mean.D.L.ratio),
            sd.DL = sd(Mean.D.L.ratio),
            Mean.pos = mean(Mean.dorsal.fin.position),
            sd.pos = sd(Mean.dorsal.fin.position))

# LSU Collection
species.sum.LSU <- data %>% 
  filter(Collection=="LSU") %>%
  select(Sci.name,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  convert_as_factor(Sci.name) %>%
  group_by(Sci.name)%>%
  get_summary_stats()

means.LSU <- data %>%
  group_by(Sci.name) %>%
  filter(Collection=="LSU") %>% 
  summarise(Mean.D.L=mean(Mean.D.L.ratio),
            sd.DL = sd(Mean.D.L.ratio),
            Mean.pos = mean(Mean.dorsal.fin.position),
            sd.pos = sd(Mean.dorsal.fin.position))

# Double check that the species we have from EAK (frozen) and LSU (ethanol) collections are comparable 
allmeans.EAK <- data %>%
  select(Sci.name,Collection, Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)%>%
  filter(Collection=="EAK") 

allmeans.LSU <- data %>%
  select(Sci.name, Collection,Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)%>%
  filter(Collection=="LSU") 

# Visual check 
species.sum.col <- data %>% 
  group_by(Sci.name,Collection)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)

ggplot(data = species.sum.col,aes(x=Mean.D.L.ratio,y=Mean.dorsal.fin.position,color=Sci.name,shape=Collection))+
  geom_point()+
  theme_classic()

# Summarize the data for each species with collection and state in mind ----
species.sum.EAK.state <- data %>% 
  filter(Collection=="EAK") %>%
  group_by(Species,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  get_summary_stats()

species.sum.LSU.state <- data %>% 
  filter(Collection=="LSU") %>%
  group_by(Species,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height) %>%
  get_summary_stats()

# Double check that the species we have from Georgia and Louisiana collections are comparable 
species.sum.state <- data %>% 
  group_by(Sci.name,Location)%>%
  select(Mean.D.L.ratio,Mean.dorsal.fin.position,Mean.gape.width,Mean.gape.height)

ggplot(data = species.sum.state,aes(x=Mean.D.L.ratio,y=Mean.dorsal.fin.position,color=Sci.name,shape=Location))+
  geom_point()+
  theme_classic()

# Stats check ----
# Frozen vs ethanol, LSU vs EAK, Georgia vs Louisiana basically the same grouping
# use t test to figure out if the speciemens from EAK can be included with the LSU measures 
t.test(allmeans.EAK$Mean.D.L.ratio[allmeans.EAK$Sci.name=="Lepomis auritus"],allmeans.LSU$Mean.D.L.ratio[allmeans.LSU$Sci.name=="Lepomis auritus"])
t.test(allmeans.EAK$Mean.D.L.ratio[allmeans.EAK$Sci.name=="Lepomis punctatus"],allmeans.LSU$Mean.D.L.ratio[allmeans.LSU$Sci.name=="Lepomis punctatus"])
t.test(allmeans.EAK$Mean.D.L.ratio[allmeans.EAK$Sci.name=="Lepomis macrochirus"],allmeans.LSU$Mean.D.L.ratio[allmeans.LSU$Sci.name=="Lepomis macrochirus"])

t.test(allmeans.EAK$Mean.dorsal.fin.position[allmeans.EAK$Sci.name=="Lepomis auritus"],allmeans.LSU$Mean.dorsal.fin.position[allmeans.LSU$Sci.name=="Lepomis auritus"])
t.test(allmeans.EAK$Mean.dorsal.fin.position[allmeans.EAK$Sci.name=="Lepomis punctatus"],allmeans.LSU$Mean.dorsal.fin.position[allmeans.LSU$Sci.name=="Lepomis punctatus"])
t.test(allmeans.EAK$Mean.dorsal.fin.position[allmeans.EAK$Sci.name=="Lepomis macrochirus"],allmeans.LSU$Mean.dorsal.fin.position[allmeans.LSU$Sci.name=="Lepomis macrochirus"])

# GRAPH 1: body depth/length ratio and position of dorsal fin 
# using pooled data for species (no constraint of collection,individual,location,year,preservation method)

ggplot(data = means,aes(x=Mean.D.L,y=Mean.pos,color=Sci.name,shape=Sci.name))+
  geom_point(size=3)+
  scale_shape_manual(values=c(1,16,13,0,2,3,4,5,6,7,8,9,10,12,15,18,17,13))+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  geom_errorbar(aes(ymin = Mean.pos-sd.pos,ymax = Mean.pos+sd.pos),) + 
  geom_errorbar(aes(xmin = Mean.D.L-sd.DL,xmax = Mean.D.L + sd.DL))+
  theme_classic()+
  xlab("Body depth/Body length")+
  ylab("Dorsal fin position")+
  xlim(0.19,0.42)+
  ylim(0.24,0.42)

# Using standard error 
ggplot(data = means,aes(x=Mean.D.L,y=Mean.pos,shape=Sci.name,color=Genus))+
  geom_point(size=2.7)+
  scale_shape_manual(values = c(0,1,4,2,5,6,7,8,10,12,13,15,16,17,23,22,24,16))+
  scale_color_manual(values = c("#91D5DE", "#2E8289", "#B4674E", "#EAAE37", "#565F41"))+
  geom_errorbar(aes(ymin = Mean.pos-se.pos,ymax = Mean.pos+se.pos),) + 
  geom_errorbar(aes(xmin = Mean.D.L-se.DL,xmax = Mean.D.L + se.DL))+
  theme_classic()+
  xlab("Body depth/Body length")+
  ylab("Dorsal fin position")+
  xlim(0.19,0.42)+
  ylim(0.24,0.42)+
  labs(shape = "Species")+
  theme(axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))
  
  

