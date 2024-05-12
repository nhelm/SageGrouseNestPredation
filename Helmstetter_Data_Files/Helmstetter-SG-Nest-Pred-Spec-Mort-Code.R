library(readr)
library(tidyverse)
library(broom)
library(mosaic)
library(ggeffects)
library(nnet)

Multi_Nom_Variables <-read.csv("PATH/Helmstetter-SG-Nest-Pred-Spec-Mort-Code.csv")

# Standardize Variables
Multi_Nom_Variables[4:30]<-as.data.frame(scale(Multi_Nom_Variables[4:30]))


# Replace all NA's with 0's for columns 4:29 (scaled variables)

Multi_Nom_Variables[4:30][is.na(Multi_Nom_Variables[4:30])] <- 0

# Check for Correlation
Multi_Pearson<-cor(Multi_Nom_Variables[4:30],method="pearson")

# Univariate Models to compare scales
shrub8km<-multinom(formula = Multi_Class ~shrub8km,data=Multi_Nom_Variables)
shrub22km<-multinom(formula = Multi_Class ~shrub22km,data=Multi_Nom_Variables)
shrub14km<-multinom(formula = Multi_Class ~shrub14km, data=Multi_Nom_Variables)
shrub44km<-multinom(formula = Multi_Class ~shrub44km, data=Multi_Nom_Variables)
shrub_cover_nest<-multinom(formula = Multi_Class ~shrub_cover_nest, data=Multi_Nom_Variables)

AICc(shrub8km,shrub14km,shrub22km,shrub44km,shrub_cover_nest)


# Univariate models to compare correlated and functionally similar variables
tmin<-multinom(formula = Multi_Class ~ tmin,data=Multi_Nom_Variables)
tmean<-multinom(formula = Multi_Class ~ tmean,data=Multi_Nom_Variables)
tmax<-multinom(formula = Multi_Class ~ tmax,data=Multi_Nom_Variables)

AICc(tmin,tmax,tmean)


AvgOfMaxHt<-multinom(formula = Multi_Class ~ AvgOfMaxHt,data=Multi_Nom_Variables)
AvgOfMaxLe<-multinom(formula = Multi_Class ~ AvgOfMaxLe,data=Multi_Nom_Variables)
AvgOfRemov<-multinom(formula = Multi_Class ~ AvgOfRemov,data=Multi_Nom_Variables)
AvgOfEff_H<-multinom(formula = Multi_Class ~ AvgOfEff_H,data=Multi_Nom_Variables)

AICc(AvgOfMaxHt,AvgOfMaxLe,AvgOfRemov,AvgOfEff_H)


Peren<-multinom(formula=Multi_Class~NHD_Peren,data=Multi_Nom_Variables)
Inter<-multinom(formula=Multi_Class~Comb_Inter,data=Multi_Nom_Variables)
WaterGlobal<-multinom(formula=Multi_Class~Dist_Water,data=Multi_Nom_Variables)

cor(Multi_Nom_Variables$NHD_Peren,Multi_Nom_Variables$Comb_Inter,method="pearson")

AICc(Peren,Inter,WaterGlobal)


Powerlines<-multinom(formula=Multi_Class~Dist_PwrLn,data=Multi_Nom_Variables)
Nest<-multinom(formula=Multi_Class~Dist_Nest,data=Multi_Nom_Variables)
Perch<-multinom(formula=Multi_Class~Dist_Perch,data=Multi_Nom_Variables)

AICc(Powerlines,Nest,Perch)


Fences<-multinom(formula=Multi_Class~Dist_Fence,data=Multi_Nom_Variables)
Roads<-multinom(formula=Multi_Class~Dist_Rds,data=Multi_Nom_Variables)

cor(Multi_Nom_Variables$Dist_Perch,Multi_Nom_Variables$Dist_Rds,method="pearson")

AICc(Roads,Perch)

AIC(Fences,Roads)



# Set a reference For Multinomial Regression
Multi_Nom_Variables$Multi_Class<-relevel(Multi_Nom_Variables$Multi_Class,ref="Hatched")

# Run Global Model with Selected Variables

Multi_Nom_Mod<-multinom(formula=Multi_Class~
                          NewCattle+
                          OldCattle+
                          shrub8km+
                          tmin+
                          AvgOfRemov+
                          AvgOfEff_H+
                          AvgOfMaxHt+
                          NHD_Peren+
                          precip+
                          DSLG+
                          Dist_Perch+
                          Dist_Fence+
                          +Dist_Ag+
                          A_PCtCover+
                          Avg_PctCov_NESW,data=Multi_Nom_Variables)


library(MuMIn)
#Run all subsets of the global model

options(na.action = "na.fail") # Require for dredge to run

All_Models_AIC_MultiNom<-dredge(Multi_Nom_Mod, "sd",rank="AICc",evaluate=TRUE)

options(na.action = "na.omit") # set back to default

# Model averaging: Averaging the models that encompass 95% of the model weight.
model_avg_MultNom_All<-model.avg(All_Models_AIC_MultiNom,subset=delta <= 14.96516036086,fit=TRUE)

confint(model_avg_MultNom_All,full=TRUE) ##This is using the Full average
confint(model_avg_MultNom_All,full=FALSE) ##This is using the conditional average
