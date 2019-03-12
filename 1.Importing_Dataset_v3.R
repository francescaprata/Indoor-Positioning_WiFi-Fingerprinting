#############################
# Name: Francesca Prata     #
# Project: WiFi Locationing #
# Script: Importing dataset #
# Date: 12 March 2019       #
# Version: 3                #
#############################

##Installing packages and loading libraries
if(!require(pacman))install.packages("pacman")

pacman::p_load('readr', 'caret', 'ggplot2', 'plotly', 'anytime', 'C50',
               'scatterplot3d', 'randomForest', 'e1071', 'dplyr', 'BBmisc',
               'tidyr')

######################
#Loading the datasets#
######################

Wifi_TrainSet <- read.csv(file = "trainingData.csv")
Wifi_TestSet <- read.csv(file = "validationData.csv")

##Change time variable from integer to an actual datetime 
Wifi_TrainSet$TIMESTAMP <- anytime(Wifi_TrainSet$TIMESTAMP)
Wifi_TestSet$TIMESTAMP <- anytime(Wifi_TestSet$TIMESTAMP)
