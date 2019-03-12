#########################################################
# Name: Francesca Prata                                 #                          
# Project: WiFi Locationing                             #  
# Script: Creating pipeline KNN for predicting latitude #                  
# Date: 11 March 2019                                   #                  
# Version: 4                                            #                  
#########################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v2.R")

#Ensure that latitude is numerical 
Wifi_TrainSet$LATITUDE <- as.numeric(Wifi_TrainSet$LATITUDE)

#Use only WAPs column to predict latitude
trainingDataWAP <- select(Wifi_TrainSet, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LONGITUDE)
#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = trainingDataWAP$LATITUDE,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- trainingDataWAP[inTrain,]
testing <- trainingDataWAP[-inTrain,]

#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitLat <- train(LATITUDE~., 
                     data = training, 
                     method = "knn",
                     metric = "RMSE",
                     tuneLength = 1,
                     trControl = fitControlKnn)

#Predict outcomes on the testing data
PredKnn_lat <- predict(KnnFitLat, newdata = WifiTest_xvalues)
postResample(PredKnn_lat, WifiTest_yvalues$LATITUDE)

#Add predicted to test values
WifiTest_yvalues$PredKnn_lat <- PredKnn_lat
