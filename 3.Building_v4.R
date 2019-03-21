############################################
# Name: Francesca Prata                    #
# Project: WiFi Locationing                #
# Script: Predicting Buildiing ID with SVM #
# Date: 15 March 2019                      #
# Version: 6                               #
############################################

#Retrieving the necessary scripts
source(file = "2.PreProcess_v2.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

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
fitcontrolSVM <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"),
                              verboseIter = TRUE) 

#Training the SVM model
set.seed(123)
SvmFitbuilding <- train(BUILDINGID~., 
                        training, 
                        method = "svmLinear",
                        trControl = fitcontrolSVM)

#Predicting the BUILDING ID from the training data
predBUILD_Svm <- predict(SvmFitbuilding, newdata = testing)

#Creating a new column with the predictions
testing$predBUILD_Svm <- predBUILD_Svm

#Checking the metrics 
confusionMatrix(testing$predBUILD_Svm, testing$BUILDINGID)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting BUILDINGID from the validation data
predBUILD_Svm <- predict(SvmFitbuilding,Wifi_ValidationSetNOZero)

#Creating a new column with the predictions
Wifi_ValidationSetNOZero$predBUILD_Svm <- predBUILD_Svm

#Checking that BUILDINGID and predBUILD_Svm are both factors
Wifi_ValidationSetNOZero$predBUILD_Svm <- as.factor(Wifi_ValidationSetNOZero$predBUILD_Svm)
Wifi_ValidationSetNOZero$BUILDINGID <- as.factor(Wifi_ValidationSetNOZero$BUILDINGID)

#Checking the metrics
confusionMatrix(Wifi_ValidationSetNOZero$predBUILD_Svm,Wifi_ValidationSetNOZero$BUILDINGID)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero$predBUILD_Svm <- as.integer(Wifi_ValidationSetNOZero$predBUILD_Svm)
Wifi_ValidationSetNOZero$BUILDINGID <- as.integer(Wifi_ValidationSetNOZero$BUILDINGID)
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsBUILD = predBUILD_Svm - BUILDINGID) 

#Storing the predicted values, actual values and errors in a tibble 
resultsBUILDINGID <- tibble(.rows = 1111)

#Adding BUILDINGID and its prediction to the tibble 
resultsBUILDINGID$predBUILD_Svm <- predBUILD_Svm
resultsBUILDINGID$BUILDINGID <-Wifi_ValidationSetNOZero$BUILDINGID

#Turning them numerical in order to mutate the errors
#and add them to the tibble
resultsBUILDINGID$predBUILD_Svm <- as.integer(resultsBUILDINGID$predBUILD_Svm)
resultsBUILDINGID$BUILDINGID <- as.integer(resultsBUILDINGID$BUILDINGID)
resultsBUILDINGID  <- mutate(resultsBUILDINGID, errorsBUILD = predBUILD_Svm - BUILDINGID) 

#Storing the file
saveRDS(resultsBUILDINGID, file = "resultsBUILDINGID(V4).rds")