#####################################################
# Name: Francesca Prata                             #
# Project: WiFi Locationing                         #
# Script: Creating pipeline Random Forest for floor #                  
# Date: 6 March 2019                                #                  
# Version: 1                                        #                  
#####################################################

#Retrieving previous dataframe 
source(file = "1.Importing_Dataset_&_Initial_Exploration_v1.R")

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = Wifi_TrainSet$FLOOR,
                              p = 0.25,
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
fitControlRF <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 1,
                             search = "random",
                             preProc = c("center", "scale"))

#Training Random Forest model
set.seed(123)
RFFitFloor<- train(x = WifiTrain_xvalues, 
                      y = WifiTrain_yvalues$FLOOR,
                      method = "rf",
                      metric = "Accuracy",
                      trControl = fitControlRF)

#See results 
print(RFFitFloor)
plot(RFFitFloor)

#Predict outcomes on the testing data
PredRF_Floor <- predict(RFFitFloor, newdata = WifiTest_xvalues)
postResample(PredRF_Floor, WifiTest_yvalues$FLOOR)

#Add predicted to test values
WifiTest_yvalues$PredRF_floor <- PredRF_Floor

#Plot errors
ggplot(WifiTest_yvalues) +
  geom_point(aes(x = BUILDINGID, y = FLOOR)) +
  geom_point(aes(x = PredRF_building, y = PredRF_floor), color = "red", alpha = 0.1)
