####################################################
# Name: Francesca Prata                            #                          
# Project: WiFi Locationing                        #  
# Script: Benchmark KNN model for predicting Floor #                  
# Date: 12 March 2019                              #                  
# Version: 5                                       #                  
####################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_v3.R")

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$FLOOR,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- Wifi_TrainSet[inTrain,]
testing <- Wifi_TrainSet[-inTrain,]

#distinguish x & y values
WifiTrain_xvalues <- training[,1:521]
WifiTrain_yvalues <- training[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE", "PredictionKnn_building")]

WifiTest_xvalues <- testing[,1:521]
WifiTest_yvalues <- testing[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE", "PredictionKnn_building")]

#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitFloor <- train(x = WifiTrain_xvalues, 
                     y = WifiTrain_yvalues$FLOOR,
                     method = "knn",
                     metric = "Accuracy",
                     tuneLength = 1,
                     trControl = fitControlKnn)

#Predict outcomes on the testing data
Prediction_Knn_floor <- predict(KnnFitFloor, newdata = WifiTest_xvalues)
postResample(Prediction_Knn_floor, WifiTest_yvalues$FLOOR)

#See the most important predictors
varImp(KnnFitFloor)

#Show values in confusion matrix
confusionMatrix(data = Prediction_Knn_floor, WifiTest_yvalues$FLOOR)

#Add predicted to test values
WifiTest_yvalues$PredKnn_floor <- Prediction_Knn_floor
