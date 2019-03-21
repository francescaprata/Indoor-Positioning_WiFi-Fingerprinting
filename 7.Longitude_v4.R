################################################
# Name: Francesca Prata                        #                          
# Project: WiFi Locationing                    #                  
# Script:  KNN model for predicting longitude  #                  
# Date: 15 March 2019                          #                  
# Version: 5                                   #                  
################################################

#Retrieving previous dataframe 
source(file = "2.PreProcess_v2.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Checking that LONGITUDE and LATITUDE are numeric
Wifi_TrainSetNOZero$LONGITUDE <- as.numeric(Wifi_TrainSetNOZero$LONGITUDE)
Wifi_TrainSetNOZero$LATITUDE <- as.numeric(Wifi_TrainSetNOZero$LATITUDE)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$LONGITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#Removing all other variables except for WAPs 
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, - LATITUDE)

#Setting cross validation parameters
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              preProc = c("center", "scale", "range"), 
                              verboseIter = TRUE)

#Training KNN model
set.seed(123)
KnnFitlongitude <- train(LONGITUDE~., 
                         training, 
                         method = "knn",
                         metric = "RMSE",
                         trControl = fitcontrolKNN)


#Making predictions with model and predicting LONGITUDE
predLON_KNN <- predict(KnnFitlongitude, newdata = testing)

#Creating new column with predicted data
testing$predLON_KNN <- predLON_KNN

#Checking metrics 
postResample(testing$predLON_KNN, testing$LONGITUDE)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting LATITUDE from the validation data
predLON_KNN <- predict(KnnFitlongitude, Wifi_ValidationSetNOZero)

#Creating a new column with predicted data
Wifi_ValidationSetNOZero$predLON_KNN <- predLON_KNN

#Checking that LONGITIUDE and predLON_KNN are both numerical
Wifi_ValidationSetNOZero$predLON_KNN <- as.numeric(Wifi_ValidationSetNOZero$predLON_KNN)
Wifi_ValidationSetNOZero$LONGITUDE <- as.numeric(Wifi_ValidationSetNOZero$LONGITUDE)

#Checking the metrics
postResample(Wifi_ValidationSetNOZero$predLON_KNN, Wifi_ValidationSetNOZero$LONGITUDE)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsLON = predLON_KNN - LONGITUDE) 

#Storing the predicted values, actual values and errors in a tibble
resultsLON <- tibble(.rows = 1111)

#Adding LONGITUDE and its prediction to the tibble 
resultsLON$predLON_KNN <- predLON_KNN
resultsLON$LONGITUDE <-Wifi_ValidationSetNOZero$LONGITUDE

#Storing the file
saveRDS(resultsLON, file = "resultsLONGITUDE(V4).rds")