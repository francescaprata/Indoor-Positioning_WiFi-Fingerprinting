###################################################
# Name: Francesca Prata                           #
# Project: WiFi Locationing                       #
# Script: Importing dataset & initial exploration #
# Date: 4 March 2019                              #
# Version: 1                                      #
###################################################

##Installing packages and loading libraries
if(!require(pacman))install.packages("pacman")

pacman::p_load('readr', 'caret', 'ggplot2', 'plotly', 'anytime', 'C50')

######################
#Loading the datasets#
######################

Wifi_TrainSet <- read.csv(file = "trainingData.csv")
Wifi_TestSet <- read.csv(file = "validationData.csv")

##Change class of building and floor to factors

Wifi_TrainSet$FLOOR <- as.factor(Wifi_TrainSet$FLOOR)
Wifi_TrainSet$BUILDINGID <- as.factor(Wifi_TrainSet$BUILDINGID)
Wifi_TrainSet$RELATIVEPOSITION <- as.factor(Wifi_TrainSet$RELATIVEPOSITION) 
Wifi_TrainSet$USERID <- as.factor(Wifi_TrainSet$USERID)
Wifi_TrainSet$PHONEID <- as.factor(Wifi_TrainSet$PHONEID)

Wifi_TestSet$FLOOR <- as.factor(Wifi_TestSet$FLOOR)
Wifi_TestSet$BUILDINGID <- as.factor(Wifi_TestSet$BUILDINGID)

##Change time variable from integer to an actual datetime 
Wifi_TrainSet$TIMESTAMP <- anytime(Wifi_TrainSet$TIMESTAMP)
Wifi_TestSet$TIMESTAMP <- anytime(Wifi_TestSet$TIMESTAMP)

#sample from traning buildingiD OR choose 1 building and 1 floor 
#We want to predict y = buildingID 

#CHECK//////////////////////////////////////////////////////
##subset for smaller data frame
SubWifiTrain <- Wifi_TrainSet %>% filter(BUILDINGID == 0)


##Distinguish x & y values
WifiTrain_xvalues <- SubWifiTrain[,1:520]
WifiTrain_yvalues <- SubWifiTrain[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

WifiTest_xvalues <- Wifi_TestSet[,1:520]
WifiTest_yvalues <- Wifi_TestSet[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]
#CHECK//////////////////////////////////////////////////////
