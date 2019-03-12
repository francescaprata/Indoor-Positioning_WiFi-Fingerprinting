##########################################################
# Name: Francesca Prata                                  #                          
# Project: WiFi Locationing                              #  
# Script: Creating pipeline KNN for predicting longitude #                  
# Date: 11 March 2019                                    #                  
# Version: 4                                             #                  
##########################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v2.R")

#Ensure that longitude is numerical 
Wifi_TrainSet$LONGITUDE <- as.numeric(Wifi_TrainSet$LONGITUDE)

#Use only WAPs column to predict longitude
trainingDataWAP <- select(Wifi_TrainSet, -FLOOR, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LATITUDE)
#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = trainingDataWAP$LONGITUDE,
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
KnnFitLon <- train(LONGITUDE~., 
                   data = training, 
                   method = "knn",
                   metric = "RMSE",
                   tuneLength = 1,
                   trControl = fitControlKnn)

#Predict outcomes on the testing data
PredKnn_lon <- predict(KnnFitLon, newdata = WifiTest_xvalues)
postResample(PredKnn_lon, WifiTest_yvalues$LONGITUDE)

#Add predicted to test values
WifiTest_yvalues$PredKnn_lon <- PredKnn_lon

