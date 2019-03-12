############################################################
# Name: Francesca Prata                                    #                          
# Project: WiFi Locationing                                #  
# Script: Creating pipeline KNN for predicting Building ID #                  
# Date: 11 March 2019                                      #                  
# Version: 4                                               #                  
############################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v2.R")

#Ensure that building ID is a factor 
Wifi_TrainSet$BUILDINGID <- as.factor(Wifi_TrainSet$BUILDINGID)

#Use only WAPs column to predict longitude
trainingDataWAP <- select(Wifi_TrainSet, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LATITUDE, -LONGITUDE)



#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = trainingDataWAP$BUILDINGID,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- trainingDataWAP[inTrain,]
testing <- trainingDataWAP[-inTrain,]

#distinguish x & y values
#WifiTrain_xvalues <- training[,1:520]
#WifiTrain_yvalues <- training[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

#WifiTest_xvalues <- testing[,1:520]
#WifiTest_yvalues <- testing[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1)

#Training KNN model
set.seed(123)
KnnFitBuilding <- train(BUILDINGID~., 
                        data = training,
                        method = "knn",
                        metric = "Accuracy",
                        tuneLength = 1,
                        trControl = fitControlKnn)

#Predict outcomes on the testing data
PredKnn_building <- predict(KnnFitBuilding, newdata = WifiTest_xvalues)
postResample(PredKnn_building, WifiTest_yvalues$BUILDINGID)

#Add predicted to test values
WifiTest_yvalues$PredKnn_building <- PredKnn_building

#See the most important predictors
#varImp(KnnFitFloor)

#Show values in confusion matrix
#confusionMatrix(data = Prediction_Knn_floor, WifiTest_yvalues$FLOOR)

#Saving as RDS object
# Save an object to a file
saveRDS(WifiTest_yvalues, file = "actualVSpredicted_KNN.rds")
