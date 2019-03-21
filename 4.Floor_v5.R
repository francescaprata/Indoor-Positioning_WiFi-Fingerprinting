#####################################
# Name: Francesca Prata             #
# Project: WiFi Locationing         #
# Script: Predicting FLOOR with KNN #
# Date: 14 March 2019               #
# Version: 5                        #
#####################################

#Retrieving the necessary scripts
source("2.PreProcess.R")

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
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE) 

#Training KNN model
set.seed(123)
KnnFitfloor <- train(FLOOR~., 
                     training, 
                     method = "knn",
                     metric = "Accuracy",
                     trControl = fitcontrolKNN)

#Making predictions with model and predicting FLOOR 
predFLOOR_KNN <- predict(KnnFitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and predFLOOR_KNN are both factors
testing$FLOOR <- as.factor(testing$FLOOR)
testing$predFLOOR_KNN <- as.factor(testing$predFLOOR_KNN)

#Checking the metrics 
confusionMatrix(testing$predFLOOR_KNN, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_KNN <- predict(KnnFitfloor, Wifi_ValidationSetNOZero)

#Creating new column with predicted data
Wifi_ValidationSetNOZero$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and PredFLOOR_KNN are both factors
Wifi_ValidationSetNOZero$predFLOOR_KNN <- as.factor(Wifi_ValidationSetNOZero$predFLOOR_KNN)
Wifi_ValidationSetNOZero$FLOOR <- as.factor(Wifi_ValidationSetNOZero$FLOOR)

#Checking the metrics
confusionMatrix(Wifi_ValidationSetNOZero$predFLOOR_KNN, Wifi_ValidationSetNOZero$FLOOR)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero$predFLOOR_KNN <- as.integer(Wifi_ValidationSetNOZero$predFLOOR_KNN)
Wifi_ValidationSetNOZero$FLOOR <- as.integer(Wifi_ValidationSetNOZero$FLOOR)
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsFLOOR = predFLOOR_KNN - FLOOR) 

#Storing the predicted values, actual values and errors in a tibble
resultsFLOOR <- tibble(.rows = 1111)

#Adding FLOOR and its prediction to the tibble
resultsFLOOR$predFLOOR_KNN <- predFLOOR_KNN
resultsFLOOR$FLOOR <- Wifi_ValidationSetNOZero$FLOOR

#Turning them numerical in order to mutate the errors
#and add them to the tibble
resultsFLOOR$predFLOOR_KNN <- as.integer(resultsFLOOR$predFLOOR_KNN)
resultsFLOOR$FLOOR <- as.integer(resultsFLOOR$FLOOR)
resultsFLOOR  <- mutate(resultsFLOOR, errorsFLOOR = predFLOOR_KNN - FLOOR) 

#Saving the file
saveRDS(resultsFLOOR, file = "resultsFLOOR(V5).rds")