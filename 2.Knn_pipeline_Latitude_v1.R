######################################################################
# Name: Francesca Prata                                              #                          
# Project: WiFi Locationing                                          #  
# Script: Creating pipeline KNN for predicting Latitude (regression) #                  
# Date: 6 March 2019                                                 #                  
# Version: 1                                                         #                  
######################################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v2.R")

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$LATITUDE,
                              p = (0.25),
                              list = FALSE)

## Separating the data into training and testing 
training <- Wifi_TrainSet[inTrain,]
testing <- Wifi_TrainSet[-inTrain,]

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
KnnFitLatitude<- train(x = WifiTrain_xvalues, 
                y = WifiTrain_yvalues$LATITUDE,
                method = "knn",
                metric = "RMSE",
                tuneLength = 1,
                trControl = fitControlKnn)

#Provide statistics of training data
print(KnnFitLatitude)

#Predict outcomes on the testing data
Prediction_Knn_lat <- predict(KnnFitLatitude, newdata = WifiTest_xvalues) 
postResample(Prediction_Knn_lat, WifiTest_yvalues$LATITUDE)

#Add predicted to test values
WifiTest_yvalues$Pred_lat <- Prediction_Knn_lat


# error visualization

ggplot(WifiTest_yvalues) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(aes(x = Pred_lon, y = Pred_lat), color = "red", alpha = 0.1) 

