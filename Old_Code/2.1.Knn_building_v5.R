###########################################################
# Name: Francesca Prata                                   #                          
# Project: WiFi Locationing                               #                  
# Script: Benchmark KNN model for predicting building ID  #                  
# Date: 12 March 2019                                     #                  
# Version: 5                                              #                  
###########################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_v3.R")

#Set building ID and floor as factors
Wifi_TrainSet$BUILDINGID <- as.factor(Wifi_TrainSet$BUILDINGID)
Wifi_TrainSet$FLOOR <- as.factor(Wifi_TrainSet$FLOOR)

Wifi_TestSet$BUILDINGID <- as.factor(Wifi_TestSet$BUILDINGID)
Wifi_TestSet$FLOOR <- as.factor(Wifi_TestSet$FLOOR)

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$BUILDINGID,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- Wifi_TrainSet[inTrain,]
testing <- Wifi_TrainSet[-inTrain,]

#distinguish x & y values
WifiTrain_xvalues <- training[,1:520]
WifiTrain_yvalues <- training[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

WifiTest_xvalues <- testing[,1:520]
WifiTest_yvalues <- testing[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitbuilding <- train(x = WifiTrain_xvalues, 
                        y = WifiTrain_yvalues$BUILDINGID,
                        method = "knn",
                        metric = "Accuracy",
                        tuneLength = 1,
                        trControl = fitControlKnn)

#Predict outcomes on the testing data
PredictionKnn_building <- predict(KnnFitbuilding, newdata = WifiTest_xvalues)
postResample(PredictionKnn_building, WifiTest_yvalues$BUILDINGID)

#See the most important predictors
varImp(KnnFitbuilding)

#Show values in confusion matrix
confusionMatrix(data = PredictionKnn_building, WifiTest_yvalues$BUILDINGID)

#Add predicted to test values
WifiTest_yvalues$PredictionKnn_building <- PredictionKnn_building
