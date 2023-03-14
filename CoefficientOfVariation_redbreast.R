# Redbreast project 2022 
# Date: 11/23/2021, updated 01/12/2022
# Author(s): Olivia H Hawkins 
# Goals: Calculate the Coefficient of Variation for all feeding, accuracy, and locmotion 
  # traits for each individual. 

# Git hub repository 
browseURL("https://github.com/hawkinso/Redbreast2022.git")

# Libraries 
library(dplyr)
library(reshape2)

# Coefficient of variation by individual ----

# Write custom function 
CoVar <- function(mean,sd){
  CV1 <- ((sd)/(mean))
  CV <- CV1 * 100
  return(abs(CV))
}

# use data frame 'means' to supply the mean and sd 
PG <- CoVar(mean = means$mean[means$variable=="PG_mag"],sd=means$sd[means$variable=="PG_mag"])
Gape_prop <- CoVar(mean = means$mean[means$variable=="Gape_prop"],sd=means$sd[means$variable=="Gape_prop"])
TTO <- CoVar(mean = means$mean[means$variable=="TTO"],sd=means$sd[means$variable=="TTO"])
TTC <- CoVar(mean = means$mean[means$variable=="TTC"],sd=means$sd[means$variable=="TTC"])
PPROT <- CoVar(mean = means$mean[means$variable=="PPROT_mag"],sd=means$sd[means$variable=="PPROT_mag"])
PPROTVEL <- CoVar(mean = means$mean[means$variable=="PPROTVEL_mag"],sd=means$sd[means$variable=="PPROTVEL_mag"])
tPPROT <- CoVar(mean = means$mean[means$variable=="tPPROT"],sd=means$sd[means$variable=="tPPROT"])
VELPG <- CoVar(mean = means$mean[means$variable=="VELPG_mag"],sd=means$sd[means$variable=="VELPG_mag"])
maxVEL <- CoVar(mean = means$mean[means$variable=="maxVEL_mag"],sd=means$sd[means$variable=="maxVEL_mag"])
tmaxVEL <- CoVar(mean = means$mean[means$variable=="tmaxVEL"],sd=means$sd[means$variable=="tmaxVEL"])
ACCPG <- CoVar(mean = means$mean[means$variable=="ACCPG_mag"],sd=means$sd[means$variable=="ACCPG_mag"])
H_L_ratio <- CoVar(mean = means$mean[means$variable=="H_L_ratio"],sd=means$sd[means$variable=="H_L_ratio"])
AI <- CoVar(mean = means$mean[means$variable=="AI"],sd=means$sd[means$variable=="AI"])
ingested_volume <- CoVar(mean = means$mean[means$variable=="ingested_volume_mag"],sd=means$sd[means$variable=="ingested_volume_mag"])
PPDiopen <- CoVar(mean = means$mean[means$variable=="PPDiopen_mag"],sd=means$sd[means$variable=="PPDiopen_mag"])
timeatcapture <- CoVar(mean = means$mean[means$variable=="timeatcapture"],sd=means$sd[means$variable=="timeatcapture"])
VELpreycapture <- CoVar(mean = means$mean[means$variable=="VELpreycapture_mag"],sd=means$sd[means$variable=="VELpreycapture_mag"])

# Merge into a dataframe
CV <- data.frame(rbind(PG,Gape_prop,TTO,TTC,PPROT,PPROTVEL,tPPROT,VELPG,maxVEL,tmaxVEL,ACCPG,H_L_ratio,AI,ingested_volume,PPDiopen,timeatcapture,VELpreycapture))

# rename the columns by individual 
names(CV)[1] <- "LAUR01"
names(CV)[2] <- "LAUR02"
names(CV)[3] <- "LAUR03"
names(CV)[4] <- "LAUR04"
names(CV)[5] <- "LAUR05"

CV$Variables <- c("PG","TTO","TTC","PPROT","PPROTVEL","tPPROT","VELPG","maxVEL","tmaxVEL","ACCPG","H_L_ratio","AI",
                  "ingested_volume","PPDiopen","timeatcapture","VELpreycapture")


# Melt data frame
CV_melt <- melt(CV,id.vars = "Variables", measure.vars = c("LAUR01","LAUR02","LAUR03","LAUR04","LAUR05"))
names(CV_melt)[2] <- "Individual"
names(CV_melt)[3] <- "CV"

# Export data 
write_csv(CV_melt,file = "Redbreast_CV_2022.csv",append = TRUE)


