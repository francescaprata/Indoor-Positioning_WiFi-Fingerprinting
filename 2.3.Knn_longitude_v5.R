########################################################
# Name: Francesca Prata                                #                          
# Project: WiFi Locationing                            #  
# Script: Benchmark KNN model for predicting Longitude #                  
# Date: 12 March 2019                                  #                  
# Version: 5                                           #                  
########################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_v3.R")

#Set building longitude and latitude as numerical for both train and test set
Wifi_TrainSet$LONGITUDE <- as.numeric(Wifi_TrainSet$LONGITUDE)
Wifi_TrainSet$LATITUDE <- as.numeric(Wifi_TrainSet$LATITUDE)

Wifi_TestSet$LONGITUDE <- as.numeric(Wifi_TestSet$LONGITUDE)
Wifi_TestSet$LATITUDE <- as.numeric(Wifi_TestSet$LATITUDE)

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$LONGITUDE,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- Wifi_TrainSet[inTrain,]
testing <- Wifi_TrainSet[-inTrain,]

#Setting cross validation parameters
fitControlLon <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitLongitude<- train(x = WifiTrain_xvalues, 
                        y = WifiTrain_yvalues$LONGITUDE,
                        method = "knn",
                        metric = "RMSE",
                        tuneLength = 1,
                        trControl = fitControlLon)

#Provide statistics of training data
print(KnnFitLongitude)

#Predict outcomes on the testing data
Prediction_Knn_lon <- predict(KnnFitLongitude, newdata = WifiTest_xvalues)
postResample(Prediction_Knn_lon, WifiTest_yvalues$LONGITUDE)

#Add predicted to test values
WifiTest_yvalues$Pred_lon <- Prediction_Knn_lon
