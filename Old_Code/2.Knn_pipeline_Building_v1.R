#############################################################
# Name: Francesca Prata                                     #
# Project: WiFi Locationing                                 #
# Script: Creating pipeline KNN for predicting building     #                  
# Date: 6 March 2019                                        # 
# Version: 1                                                #
#############################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v1.R")

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$BUILDINGID,
                              p = (0.25),
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
fitControlKnnB <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitBuilding <- train(x = WifiTrain_xvalues, 
                       y = WifiTrain_yvalues$BUILDINGID,
                       method = "knn",
                       metric = "Accuracy",
                       tuneLength = 1,
                       trControl = fitControlKnnB)

#Provide statistics of training data
print(KnnFitBuilding)

#Predict outcomes on the testing data
Prediction_Knn_building <- predict(KnnFitBuilding, newdata = WifiTest_xvalues) 
postResample(Prediction_Knn_building, WifiTest_yvalues$BUILDINGID)

#Add predicted to test values
WifiTest_yvalues$Pred_building <- Prediction_Knn_building
