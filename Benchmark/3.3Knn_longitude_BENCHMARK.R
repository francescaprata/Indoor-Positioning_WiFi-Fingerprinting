#########################################################
# Name: Francesca Prata                                 #                          
# Project: WiFi Locationing                             #                  
# Script: Benchmark KNN model for predicting longitude  #                  
# Date: 12 March 2019                                   #                  
# Version: 5                                            #                  
#########################################################

#Retrieving previous dataframe 
source(file = "1.Import_v3.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Turning LONGITUDE and LATITUDE into numerical 
Wifi_TrainSet$LONGITUDE <- as.numeric(Wifi_TrainSet$LONGITUDE)
Wifi_TrainSet$LATITUDE <- as.numeric(Wifi_TrainSet$LATITUDE)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSet$LONGITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSet[indexTrain,]
testing <- Wifi_TrainSet[-indexTrain,]

#Only use WAPs to predict LONGITUDE
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, - LATITUDE)

#Setting cross validation parameters
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              verboseIter = TRUE) 

#Training KNN model
set.seed(123)
KnnFitlongitude <- train(LONGITUDE~., 
                        training, 
                        method = "knn",
                        metric = "RMSE",
                        trControl = fitcontrolKNN)

#Predicting LONGITUDE from the training data
predLON_KNN <- predict(KnnFitlongitude, newdata = testing)

#Creating new column with predicted data
testing$PredictionKnn_longitude <- predLON_KNN

#Checking metrics 
postResample(testing$predLON_KNN, testing$LONGITUDE)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting LONGITUDE from the validation data
predLON_KNN <- predict(KnnFitlongitude, Wifi_ValidationSet)

#Creating a new column with the predictions
Wifi_ValidationSet$predLON_KNN <- predLON_KNN

#Checking the metrics
postResample(Wifi_ValidationSet$predLON_KNN, Wifi_ValidationSet$LONGITUDE)
