###########################################
# Name: Francesca Prata                   #
# Project: WiFi Locationing               #
# Script: Predicting building ID with C.5 #                  
# Date: 5 March 2019                      # 
# Version: 1                              #
###########################################

#Retrieving the necessary scripts
source(file = "2.PreProcess_v2.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#############
# MODELLING #
#############

#Checking that BUILDINGID and FLOOR are both factors
Wifi_TrainSetNOZero$BUILDINGID <- as.factor(Wifi_TrainSetNOZero$BUILDINGID)
Wifi_TrainSetNOZero$FLOOR <- as.factor(Wifi_TrainSetNOZero$FLOOR)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$BUILDINGID, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#Predicting BUILDINGID by only using the WAPs
training <- select(training, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)

#Setting cross validation parameters
fitcontrolC5 <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 1,
                             preProc = c("center", "scale", "range"),
                             verboseIter = TRUE) 

#Training C5.0 model
set.seed(123)
C5FitBuilding <- train(BUILDINGID~., 
                       training,
                       method = "C5.0",
                       metric = "Accuracy",
                       tuneLength = 1,
                       trControl = fitcontrolC5)

#Predicting the BUILDING ID from the training data 
predBUILD_C5 <- predict(C5FitBuilding, newdata = testing)

#Creating a new column with the predictions
testing$predBUILD_C5 <- predBUILD_C5

#Checking the metrics 
confusionMatrix(testing$predBUILD_C5, testing$BUILDINGID)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting BUILDINGID from the validation data
predBUILD_C5 <- predict(C5FitBuilding, Wifi_ValidationSetNOZero)
plot(predBUILD_C5)

#Creating a new column with the predictions
Wifi_ValidationSetNOZero$predBUILD_C5 <- predBUILD_C5

#Checking that BUILDINGID and predBUILD_C5 are both factors
Wifi_ValidationSetNOZero$predBUILD_C5 <- as.factor(Wifi_ValidationSetNOZero$predBUILD_C5)
Wifi_ValidationSetNOZero$BUILDINGID <- as.factor(Wifi_ValidationSetNOZero$BUILDINGID)

#Checking the metrics
confusionMatrix(Wifi_ValidationSetNOZero$predBUILD_C5, Wifi_ValidationSetNOZero$BUILDINGID)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsBUILD = predBUILD_C5 - BUILDINGID) 

#Storing the predicted values, actual values and errors in a tibble
resultsBUILDINGID <- tibble(.rows = 1111)

#Adding FLOOR and its prediction to the tibble
resultsBUILDINGID$predBUILD_C5 <- predBUILD_C5
resultsBUILDINGID$BUILDINGID <-Wifi_ValidationSetNOZero$BUILDINGID

#Storing the file
saveRDS(resultsBUILDINGID, file = "resultsBUILDC5(V1).rds")