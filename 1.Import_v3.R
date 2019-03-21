#############################
# Name: Francesca Prata     #
# Project: WiFi Locationing #
# Script: Importing dataset #
# Date: 12 March 2019       #
# Version: 3                #
#############################

##Installing packages and loading libraries
if(!require(pacman))install.packages("pacman")

pacman::p_load('readr', 'caret', 'ggplot2', 'plotly', 'anytime',
               'scatterplot3d', 'dplyr', 'BBmisc',
               'tidyr', 'tidyverse')

######################
#Loading the datasets#
######################

Wifi_TrainSet <- read.csv(file = "trainingData.csv")
Wifi_ValidationSet <- read.csv(file = "validationData.csv")

##Change time variable from integer to an actual datetime 
Wifi_TrainSet$TIMESTAMP <- anytime(Wifi_TrainSet$TIMESTAMP)
Wifi_ValidationSet$TIMESTAMP <- anytime(Wifi_ValidationSet$TIMESTAMP)
