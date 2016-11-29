# TODO: Add comment
# 
# Author: Ricardo Cachucho
###############################################################################
##################################READ ME FIRST################################
#First run in you R console rQuickie.R, featureSelection.R and featureConstruction.R.
#Make sure you have all the necessary packages installed.
#The code is not optimized for speed, and we are not responsible for any errors that might occur in the code.
###############################################################################
###################################PACKAGES####################################
library(CORElearn)
library(RWeka)



###############################################################################
################################Snowboard Dataset##############################
WD <- "C:/Users/Στέλιος/Desktop/Stelios/MSc Computer Science/Courses Fall 16/Research Project/RAnalysisPlatform/data_arcodium"
if(getwd() != WD) setwd(WD)
# Target:
DData <- read.csv("snowTarget.csv")[,2]
summary(DData)
# Predictors:
IData <- read.csv("snowPredictors.csv")
summary(IData)
# Running Accordion:
AIData <- analyseIndependentData(IData)
AIData$Stable.Sampling.Rate <- "yes"
PParameter <- parameterFinder(AIData$Sampling.Rate, 0.0166, 5)
features <- embedded.IG.FS(DData, IData, AIData, PParameter)
write.csv(features, file="aggregateFeaturesSnow.csv", row.names=FALSE)


###############################################################################
################################Routines Dataset###############################
#WD <- "C:/Users/Cachucho&Martorelli/Dropbox/personal website/Publ"
WD <- "C:/Users/Στέλιος/Desktop/Stelios/MSc Computer Science/Courses Fall 16/Research Project/RAnalysisPlatform"
if(getwd() != WD) setwd(WD)
# Targets:
DData <- read.csv("sleepTargets.csv")
# Predictors:
IData <- read.csv("sleepPredictors.csv")
summary(IData)


AIData <- analyseIndependentData(IData)
PParameter <- parameterFinder(AIData$Sampling.Rate, 1.157407e-05, 10)
# Running Accordion:
dataset.wake <- embedded.CCF.FS(DData[[2]], IData, AIData, PParameter)
summary(dataset.wake)
dataset.light.sleep <- embedded.CCF.FS(DData[[3]], IData, AIData, PParameter)
summary(dataset.light.sleep)
dataset.REM <- embedded.CCF.FS(DData[[4]], IData, AIData, PParameter)
summary(dataset.REM)
dataset.deep.sleep <- embedded.CCF.FS(DData[[5]], IData, AIData, PParameter)
summary(dataset.deep.sleep)
