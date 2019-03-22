########################################
# Name: Francesca Prata                #
# Project: WiFi Locationing            #
# Script: Predicting LATITUDE with GBT #
# Date: 14 March 2019                  #
# Version: 1                           #
########################################

#Retrieving the necessary scripts
source("2.PreProcess_v2.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#############
# MODELLING #
#############

#Checking that LATITUDE and LONGITUDE are both numerical 
Wifi_TrainSetNOZero$LATITUDE <- as.numeric(Wifi_TrainSetNOZero$LATITUDE)
Wifi_TrainSetNOZero$LONGITUDE <- as.numeric(Wifi_TrainSetNOZero$LONGITUDE)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$LATITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#Removing all other variables except for WAPs 
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE)

#Setting cross validation parameters
fitcontrolGBT <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE) 

#Training GBT model
set.seed(123)
GbtFitlatitude <- train(LATITUDE~., 
                     training, 
                     method = "gbm",
                     trControl = fitcontrolGBT)

#Making predictions with model and predicting LATITUDE 
predLAT_Gbt <- predict(GbtFitlatitude, newdata = testing)

#Creating new column with predicted data
testing$predLAT_Gbt <- predLAT_Gbt

#Checking that LATITUDE and predLAT_Gbt are both numerical
testing$LATITUDE <- as.numeric(testing$LATITUDE)
testing$predLAT_Gbt <- as.numeric(testing$predLAT_Gbt)

#Checking the metrics 
postResample(testing$predLAT_Gbt, testing$LATITUDE)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Making predictions with model and predicting LATITUDE
predLAT_Gbt <- predict(GbtFitlatitude, Wifi_ValidationSetNOZero)

#Creating new column with predicted data
Wifi_ValidationSetNOZero$predLAT_Gbt <- predLAT_Gbt

#Checking that LATITUDE and predLAT_Gbt are both factors
Wifi_ValidationSetNOZero$predLAT_Gbt <- as.numeric(Wifi_ValidationSetNOZero$predLAT_Gbt)
Wifi_ValidationSetNOZero$LATITUDE <- as.numeric(Wifi_ValidationSetNOZero$LATITUDE)

#Checking the metrics
postResample(Wifi_ValidationSetNOZero$predLAT_Gbt, Wifi_ValidationSetNOZero$LATITUDE)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsLAT = predLAT_Gbt - LATITUDE) 

#Storing the predicted values, actual values and errors in a tibble
resultsLAT <- tibble(.rows = 1111)

#Adding LATITUDE and its prediction to the tibble
resultsLAT$predLAT_Gbt <- predLAT_Gbt
resultsLAT$LATITUDE <-Wifi_ValidationSetNOZero$LATITUDE

#Storing the file
saveRDS(resultsLAT, file = "resultsLATITUDEGBT(V1).rds")