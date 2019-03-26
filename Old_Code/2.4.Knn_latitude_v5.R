#######################################################
# Name: Francesca Prata                               #                          
# Project: WiFi Locationing                           #  
# Script: Benchmark KNN model for predicting Latitude #                  
# Date: 12 March 2019                                 #                  
# Version: 5                                          #                  
#######################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_v3.R")

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$LATITUDE,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- Wifi_TrainSet[inTrain,]
testing <- Wifi_TrainSet[-inTrain,]

#Setting cross validation parameters
fitControlLat <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitLatitude<- train(x = WifiTrain_xvalues, 
                       y = WifiTrain_yvalues$LATITUDE,
                       method = "knn",
                       metric = "RMSE",
                       tuneLength = 1,
                       trControl = fitControlLon)

#Provide statistics of training data
print(KnnFitLatitude)

#Predict outcomes on the testing data
Prediction_Knn_lat <- predict(KnnFitLatitude, newdata = WifiTest_xvalues)
postResample(Prediction_Knn_lat, WifiTest_yvalues$LATITUDE)

#Add predicted to test values
WifiTest_yvalues$Pred_lat <- Prediction_Knn_lat

#Saving this file as .rds
saveRDS(WifiTest_yvalues, file = "actualVSpredicted_KNN.rds")  
