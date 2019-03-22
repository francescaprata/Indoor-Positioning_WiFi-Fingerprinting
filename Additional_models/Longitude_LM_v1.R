##################################################
# Name: Francesca Prata                          #                            
# Project: WiFi Locationing                      #                  
# Script:  Linear model for predicting longitude #                  
# Date: 15 March 2019                            #                   
# Version: 1                                     #                   
##################################################

#Retrieving previous dataframe 
source(file = "1.Import_v3.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Checking that LONGITUDE and LATITUDE are both numerical
Wifi_TrainSetNOZero$LONGITUDE <- as.numeric(Wifi_TrainSetNOZero$LONGITUDE)
Wifi_TrainSetNOZero$LATITUDE <- as.numeric(Wifi_TrainSetNOZero$LATITUDE)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$LONGITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#We only want to use WAPs to predict LONGITUDE
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, - LATITUDE)

#Setting cross validation parameters
fitcontrolLM <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 1,
                             preProc = c("center", "scale", "range"), 
                             verboseIter = TRUE)

#Training Linear model
set.seed(123)
modelLMLon <- train(LONGITUDE~., 
                    data = training, 
                    method = "lm", 
                    trControl = fitcontrolLM)

#Predicting LONGITUDE from the training data
predLON_LM <- predict(modelLMLon, newdata = testing)

#Creating a new column with the predictions
testing$predLON_LM <- predLON_LM

#Checking the  metrics 
postResample(testing$predLON_LM, testing$LONGITUDE)

############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting LONGITUDE from the validation data
predLON_LM <- predict(modelLMLon, Wifi_ValidationSetNOZero)

#Creating a new column with the predictions
Wifi_ValidationSetNOZero$predLON_LM <- predLON_LM

#Checking that LONGITUDE and predLON_LM are both numerical
Wifi_ValidationSetNOZero$predLON_LM <- as.numeric(Wifi_ValidationSetNOZero$predLON_LM)
Wifi_ValidationSetNOZero$LONGITUDE <- as.numeric(Wifi_ValidationSetNOZero$LONGITUDE)

#Checking the metrics
postResample(Wifi_ValidationSetNOZero$predLON_LM, Wifi_ValidationSetNOZero$LONGITUDE)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsLON = predLON_LM - LONGITUDE) 

#Storing the predicted values, actual values and errors in a tibble
resultsLON <- tibble(.rows = 1111)

#Adding LONGITUDE and its prediction to the tibble
resultsLON$predLON_LM <- predLON_LM
resultsLON$LONGITUDE <-Wifi_ValidationSetNOZero$LONGITUDE

#Storing the file
saveRDS(resultsLON, file = "resultsLONGITUDELM(V1).rds")