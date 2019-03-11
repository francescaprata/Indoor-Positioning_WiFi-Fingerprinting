###########################################################
# Name: Francesca Prata                                   #
# Project: WiFi Locationing                               #
# Script: Creating pipeline Random Forest for building ID #                  
# Date: 5 March 2019                                      #                  
# Version: 1                                              #                  
###########################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v1.R")

#Ensure that building ID is a factor 
Wifi_TrainSet$BUILDINGID <- as.factor(Wifi_TrainSet$BUILDINGID)

#Use only WAPs column to predict longitude
trainingDataWAP <- select(Wifi_TrainSet, -FLOOR, -SPACEID, -RELATIVEPOSITION, -USERID, 
                          -PHONEID, -TIMESTAMP, -LATITUDE, -LONGITUDE)

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = trainingDataWAP$BUILDINGID,
                              p = 0.25,
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
fitControlRF <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 1,
                             search = "random",
                             preProc = c("center", "scale"))

#Training Random Forest model
set.seed(123)
RFFitBuilding<- train(BUILDINGID ~., 
                      data = training,
                      method = "rf",
                      metric = "Accuracy",
                      trControl = fitControlRF)

#See results 
print(RFFitBuilding)
plot(RFFitBuilding)

#predict outcomes on the testing data
PredRF_Building <- predict(RFFitBuilding, newdata = WifiTest_xvalues)
postResample(PredRF_Building, WifiTest_yvalues$BUILDINGID)

#See the most important predictors
#varImp(RFFitBuilding)

#Show values in confusion matrix
confusionMatrix(data = PredRF_Building, WifiTest_yvalues$BUILDINGID)

#Add predicted to test values
WifiTest_yvalues$PredRF_building <- PredRF_Building

