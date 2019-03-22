#####################################
# Name: Francesca Prata             #
# Project: WiFi Locationing         #
# Script: Predicting FLOOR with SVM #
# Date: 14 March 2019               #
# Version: 1                        #
#####################################

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
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$FLOOR, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#Removing all other variables except for WAPs 
training <- select(training, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)

#Setting cross validation parameters
fitcontrolSVM <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE) 

#Training SVM model
set.seed(123)
SvmFitfloor <- train(FLOOR~., 
                     training, 
                     method = "svmLinear",
                     trControl = fitcontrolSVM)

#Making predictions with model and predicting FLOOR 
predFLOOR_Svm <- predict(SvmFitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_Svm <- predFLOOR_Svm

#Checking that FLOOR and predFLOOR_Svm are both factors
testing$FLOOR <- as.factor(testing$FLOOR)
testing$predFLOOR_Svm <- as.factor(testing$predFLOOR_Svm)

#Checking the metrics 
confusionMatrix(testing$predFLOOR_Svm, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_Svm <- predict(SvmFitfloor, Wifi_ValidationSetNOZero)

#Creating new column with predicted data
Wifi_ValidationSetNOZero$predFLOOR_Svm <- predFLOOR_Svm

#Checking that FLOOR and predFLOOR_Svm are both factors
Wifi_ValidationSetNOZero$predFLOOR_Svm <- as.factor(Wifi_ValidationSetNOZero$predFLOOR_Svm)
Wifi_ValidationSetNOZero$FLOOR <- as.factor(Wifi_ValidationSetNOZero$FLOOR)

#Checking the metrics
confusionMatrix(Wifi_ValidationSetNOZero$predFLOOR_Svm, Wifi_ValidationSetNOZero$FLOOR)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsFLOOR = predFLOOR_Svm - FLOOR) 

#Storing the predicted values, actual values and errors in a tibble
resultsFLOOR <- tibble(.rows = 1111)

#Adding FLOOR and its prediction to the tibble
resultsFLOOR$predFLOOR_Svm <- predFLOOR_Svm
resultsFLOOR$FLOOR <-Wifi_ValidationSetNOZero$FLOOR

#Storing the file
saveRDS(resultsFLOOR, file = "resultsFLOORSVM(V1).rds")