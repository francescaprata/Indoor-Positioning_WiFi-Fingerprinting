###############################################
# Name: Francesca Prata                       #                          
# Project: WiFi Locationing                   #                  
# Script:  KNN model for predicting LATITUDE  #                  
# Date: 15 March 2019                         #                  
# Version: 4                                  #                  
###############################################

#Retrieving previous dataframe 
source(file = "2.PreProcess_v2.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Checking that LONGITUDE and LATITUDE are both numerical 
Wifi_TrainSetNOZero$LONGITUDE <- as.numeric(Wifi_TrainSetNOZero$LONGITUDE)
Wifi_TrainSetNOZero$LATITUDE <- as.numeric(Wifi_TrainSetNOZero$LATITUDE)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$LATITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#Removing all other variables except for WAPs 
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE)

#Setting cross validation parameters
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE)

#Training KNN model
set.seed(123)
KnnFitlatitude <- train(LATITUDE~., 
                         training, 
                         method = "knn",
                         metric = "RMSE",
                         trControl = fitcontrolKNN)

#Making predictions with model and predicting LATITUDE
predLAT_KNN <- predict(KnnFitlatitude, newdata = testing)

#Creating new column with predicted data
testing$predLAT_KNN <- predLAT_KNN

#Checking the metrics 
postResample(testing$predLAT_KNN, testing$LATITUDE)

############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting LATITUDE from the validation data
predLAT_KNN <- predict(KnnFitlatitude, Wifi_ValidationSetNOZero)

#Creating a new column with predicted data
Wifi_ValidationSetNOZero$predLAT_KNN <- predLAT_KNN

#Checking that LATITUDE and predLAT_KNN are both numerical
Wifi_ValidationSetNOZero$predLAT_KNN <- as.numeric(Wifi_ValidationSetNOZero$predLAT_KNN)
Wifi_ValidationSetNOZero$LATITUDE <- as.numeric(Wifi_ValidationSetNOZero$LATITUDE)

#Checking the metrics
postResample(Wifi_ValidationSetNOZero$predLAT_KNN, Wifi_ValidationSetNOZero$LATITUDE)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsLAT = predLAT_KNN - LATITUDE) 

#Storing the predicted values, actual values and errors in a tibble
resultsLAT <- tibble(.rows = 1111)

#Adding LATITUDE and its prediction to the tibble
resultsLAT$predLAT_KNN <- predLAT_KNN
resultsLAT$LATITUDE <-Wifi_ValidationSetNOZero$LATITUDE

#Storing the file
saveRDS(resultsLAT, file = "resultsLATITUDEKNN(V4).rds")