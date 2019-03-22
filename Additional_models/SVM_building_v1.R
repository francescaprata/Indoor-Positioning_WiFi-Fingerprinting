##########################################
# Name: Francesca Prata                  #
# Project: WiFi Locationing              #
# Script: Predicting BUILDINGID with SVM #
# Date: 14 March 2019                    #
# Version: 1                             #
##########################################

#Retrieving the necessary scripts
source("2.PreProcess_v2.R")

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

#Removing all other variables except for WAPs 
training <- select(training, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)

#Setting cross validation parameters
fitcontrolSVM <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE) 

#Training SVM model
set.seed(123)
SvmFitbuilding <- train(BUILDINGID~., 
                     training, 
                     method = "svmLinear",
                     trControl = fitcontrolSVM)

#Making predictions with model and predicting BUILDINGID 
predBUILD_Svm <- predict(SvmFitbuilding, newdata = testing)

#Creating new column with predicted data
testing$predBUILD_Svm <- predBUILD_Svm

#Checking that BUILDINGID and predBUILD_Svm are both factors
testing$BUILDINGID <- as.factor(testing$BUILDINGID)
testing$predBUILD_Svm <- as.factor(testing$predBUILD_Svm)

#Checking the metrics 
confusionMatrix(testing$predBUILD_Svm, testing$BUILDINGID)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting BUILDINGID from the validation data
predBUILD_Svm <- predict(SvmFitbuilding, Wifi_ValidationSetNOZero)

#Creating new column with predicted data
Wifi_ValidationSetNOZero$predBUILD_Svm <- predBUILD_Svm

#Checking that BUILDINGID and predBUILD_Svm are both factors
Wifi_ValidationSetNOZero$predBUILD_Svm <- as.factor(Wifi_ValidationSetNOZero$predBUILD_Svm)
Wifi_ValidationSetNOZero$BUILDINGID <- as.factor(Wifi_ValidationSetNOZero$BUILDINGID)

#Checking the metrics
confusionMatrix(Wifi_ValidationSetNOZero$predBUILD_Svm, Wifi_ValidationSetNOZero$BUILDINGID)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsBUILD = predBUILD_Svm - BUILDINGID) 

#Storing the predicted values, actual values and errors in a tibble
resultsBUILDINGID <- tibble(.rows = 1111)

#Adding FLOOR and its prediction to the tibble
resultsBUILDINGID$predBUILD_Svm <- predBUILD_Svm
resultsBUILDINGID$BUILDINGID <-Wifi_ValidationSetNOZero$BUILDINGID

#Storing the file
saveRDS(resultsBUILDINGID, file = "resultsBUILDSVM(V1).rds")