#########################################################
# Name: Francesca Prata                                 #                          
# Project: WiFi Locationing                             #                  
# Script: Benchmark KNN model for predicting latitude  #                  
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
indexTrain <- createDataPartition(y = Wifi_TrainSet$LATITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSet[indexTrain,]
testing <- Wifi_TrainSet[-indexTrain,]

#We only want to use WAPs to predict LATITUDE
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, - LONGITUDE)

#Setting cross validation parameters
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              verboseIter = TRUE) 

#Training KNN model
set.seed(123)
KnnFitlatitude <- train(LATITUDE~., 
                         training, 
                         method = "knn",
                         metric = "RMSE",
                         trControl = fitcontrolKNN)

#Making predictions of LATITUDE from the training data
PredictionKnn_latitude <- predict(KnnFitlatitude, newdata = testing)

#Creating new column with predicted data
testing$PredictionKnn_latitude <- PredictionKnn_latitude

#Checking the  metrics 
postResample(testing$PredictionKnn_latitude, testing$LATITUDE)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting LATITUDE from the validation data
predLAT_KNN <- predict(KnnFitlatitude, Wifi_ValidationSet)

#Creating new column with predicted data
Wifi_ValidationSet$predLAT_KNN <- predLAT_KNN

#Checking the metrics
postResample(Wifi_ValidationSet$predLAT_KNN, Wifi_ValidationSet$LATITUDE)
