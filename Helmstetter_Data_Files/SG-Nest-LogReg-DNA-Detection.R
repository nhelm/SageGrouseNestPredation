##Evaluating factors that influence detection of DNA from predated sage-grouse nests and Samples

library(readr)
library(tidyverse)  # data wrangling
library(broom)  # tidy regression output
library(mosaic)
library(MuMIn)

SG-Nest-Detection<-read.csv("PATH/SG-Nest-LogReg-DNA-Detection.csv")

SG-Nest-Detection<-SG-Nest-Detection[c("NestID","Detect","ElapsedTime",	
                                           "Precip",	"tmean",	"tmax",	"tmin",	"totalsamps")]

# Standardize variables
SG-Nest-Detection[3:8]<-as.data.frame(scale(SG-Nest-Detection[3:8]))

# Check correlation
Cor_Nest<-cor(SG-Nest-Detection[3:8],method="pearson")


# Select among correlated variables based on AICc

detect_min<-glm(formula = Detect ~tmin, family="binomial",data=SG-Nest-Detection)
detect_max<-glm(formula = Detect ~tmax, family="binomial",data=SG-Nest-Detection)
detect_mean<-glm(formula = Detect ~tmean, family="binomial",data=SG-Nest-Detection)

AICc(detect_min,detect_max,detect_mean)

detect_global<-glm(formula = Detect ~tmin+totalsamps+ElapsedTime+Precip+tmin*Precip, family="binomial",data=SG-Nest-Detection)
detect_intercept<-glm(formula = Detect ~1, family="binomial",data=SG-Nest-Detection)

# Run all subsets of global model

options(na.action = "na.fail")
AICc(detect_intercept)
All_Models_AIC_Detect<-dredge(detect_global, "sd",rank="AICc",evaluate=TRUE)

options(na.action = "na.omit") # set back to default


### By Sample

SG-Nest-Detection-Sample<-read.csv("PATH/SG-Nest-LogReg-DNA-Detection-Sample.csv")


##Select only variables of interest

SG-Nest-Detection-Sample<-SG-Nest-Detection-Sample[c("NestID","Detect","ElapsedTime",	
                                                  "Precip",	"tmean",	"tmax",	"tmin")]

## Standardized variables

SG-Nest-Detection-Sample[3:7]<-as.data.frame(scale(SG-Nest-Detection-Sample[3:7]))
View(SG-Nest-Detection-Sample)

#Check correlation
Cor_Samps<-cor(SG-Nest-Detection-Sample[3:7],method="pearson")

# Select among correlated variables based on AICc
detect_min_samps<-glm(formula = Detect ~tmin, family="binomial",data=SG-Nest-Detection-Sample)
detect_max_samps<-glm(formula = Detect ~tmax, family="binomial",data=SG-Nest-Detection-Sample)
detect_mean_samps<-glm(formula = Detect ~tmean, family="binomial",data=SG-Nest-Detection-Sample)

AICc(detect_min_samps,detect_max_samps,detect_mean_samps)

#Global Model
detect_global_samps<-glm(formula = Detect ~tmax+ElapsedTime+Precip+tmax*Precip, family="binomial",data=SG-Nest-Detection-Sample)

#Int. Only Model
detect_intercept_samps<-glm(formula = Detect ~1, family="binomial",data=SG-Nest-Detection-Sample)

# Run Subsets of global model
options(na.action = "na.fail")
All_Models_AIC_Detect<-dredge(detect_global_samps, "sd",rank="AICc",evaluate=TRUE)
options(na.action = "na.omit") # set back to default
