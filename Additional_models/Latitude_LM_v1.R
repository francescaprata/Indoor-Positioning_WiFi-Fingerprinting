#################################################
# Name: Francesca Prata                         #                            
# Project: WiFi Locationing                     #                  
# Script: Linear model for predicting latitude  #                  
# Date: 15 March 2019                           #                   
# Version: 1                                    #                   
#################################################

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
indexTrain <- createDataPartition(y = Wifi_TrainSetNOZero$LATITUDE, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSetNOZero[indexTrain,]
testing <- Wifi_TrainSetNOZero[-indexTrain,]

#Predicting LATITUDE by only using WAPs
training <- select(training, -BUILDINGID, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, - LONGITUDE)

#Setting cross validation parameters
fitcontrolLM <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 1,
                             preProc = c("center", "scale", "range"), 
                             verboseIter = TRUE)

#Training Linear Model
set.seed(123)
modelLMLat <- train(LATITUDE~., 
                    data = training, 
                    method = "lm", 
                    trControl = fitcontrolLM)

#Predicting LATITUDE from the training data
predLAT_LM <- predict(modelLMLat, newdata = testing)

#Creating a new column with the predictions
testing$predLAT_LM <- predLAT_LM

#Checking the  metrics 
postResample(testing$predLAT_LM, testing$LATITUDE)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting LATITUDE from the validation data
predLAT_LM <- predict(modelLMLat, Wifi_ValidationSetNOZero)

#Creating a new column with the predictions
Wifi_ValidationSetNOZero$predLAT_LM <- predLAT_LM

#Checking that LATITUDE and predLAT_LM are both numerical
Wifi_ValidationSetNOZero$predLAT_LM <- as.numeric(Wifi_ValidationSetNOZero$predLAT_LM)
Wifi_ValidationSetNOZero$LATITUDE <- as.numeric(Wifi_ValidationSetNOZero$LATITUDE)

#Checking the metrics
postResample(Wifi_ValidationSetNOZero$predLAT_LM, Wifi_ValidationSetNOZero$LATITUDE)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsLAT = predLAT_LM - LATITUDE) 

#Storing the predicted values, actual values and errors in a tibble
resultsLAT <- tibble(.rows = 1111)

#Adding LATITUDE and its prediction to the tibble
resultsLAT$predLAT_LM <- predLAT_LM
resultsLAT$LATITUDE <-Wifi_ValidationSetNOZero$LATITUDE

#Storing the file
saveRDS(resultsLAT, file = "resultsLATITUDELM(V1).rds")