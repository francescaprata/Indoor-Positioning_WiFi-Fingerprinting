##################################################
# Name: Francesca Prata                          #                            
# Project: WiFi Locationing                      #                  
# Script: Decision tree for predicting longitude #                  
# Date: 15 March 2019                            #                   
# Version: 1                                     #                   
##################################################

#Retrieving previous dataframe 
source(file = "2.PreProcess_v2.R")

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
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$LONGITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#We only want to use WAPs to predict LONGITUDE
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, - LATITUDE)

#Setting cross validation parameters
fitcontrolGBT <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE) 
#Training GBT model
set.seed(123)
GbtFitlongitude <- train(LONGITUDE~., 
                         training, 
                         method = "gbm",
                         trControl = fitcontrolGBT)

#Making predictions with model and predicting LONGITUDE
predLON_Gbt <- predict(GbtFitlongitude, newdata = testing)

#Creating new column with predicted data
testing$predLON_Gbt <- predLON_Gbt

#Checking that LONGITUDE and predLON_Gbt are both factors
testing$LONGITUDE <- as.numeric(testing$LONGITUDE)
testing$predLON_Gbt <- as.numeric(testing$predLON_Gbt)

#Checking the metrics 
postResample(testing$PredictionSvm_longitude, testing$LONGITUDE)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Making predictions with model and predicting LONGITUDE
predLON_Gbt <- predict(GbtFitlongitude, Wifi_ValidationSetNOZero)

#Creating new column with predicted data
Wifi_ValidationSetNOZero$predLON_Gbt <- predLON_Gbt

#Checking that LONGITUDE and predLON_Gbt are both factors
Wifi_ValidationSetNOZero$predLON_Gbt <- as.numeric(Wifi_ValidationSetNOZero$predLON_Gbt)
Wifi_ValidationSetNOZero$LONGITUDE <- as.numeric(Wifi_ValidationSetNOZero$LONGITUDE)

#Checking the metrics
postResample(Wifi_ValidationSetNOZero$predLON_Gbt, Wifi_ValidationSetNOZero$LONGITUDE)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsLON = predLON_Gbt - LONGITUDE) 

#Storing the predicted values, actual values and errors in a tibble
resultsLON <- tibble(.rows = 1111)

#Adding LONGITUDE and its prediction to the tibble
resultsLON$predLON_Gbt <- predLON_Gbt
resultsLON$LONGITUDE <-Wifi_ValidationSetNOZero$LONGITUDE

#Storing the file
saveRDS(resultsLON, file = "resultsLONGITUDEGBT(V1).rds")