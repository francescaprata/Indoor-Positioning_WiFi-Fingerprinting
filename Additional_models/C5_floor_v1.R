#####################################
# Name: Francesca Prata             #
# Project: WiFi Locationing         #
# Script: Predicting FLOOR with C.5 #
# Date: 14 March 2019               #
# Version: 5                        #
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
fitcontrolC5 <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 1,
                             preProc = c("center", "scale", "range"), 
                             verboseIter = TRUE) 

#Training C5.0 model
set.seed(123)
C5Fitfloor <- train(FLOOR~., 
                    training,
                    method = "C5.0",
                    metric = "Accuracy",
                    trControl = fitcontrolC5)

#Making predictions with model and predicting FLOOR 
predFLOOR_C5 <- predict(C5Fitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_C5 <- predFLOOR_C5

#Checking that FLOOR and predFLOOR_C5 are both factors
testing$FLOOR <- as.factor(testing$FLOOR)
testing$predFLOOR_C5 <- as.factor(testing$predFLOOR_C5)

#Checking the metrics 
confusionMatrix(testing$predFLOOR_C5, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_C5 <- predict(C5Fitfloor, Wifi_ValidationSetNOZero)
plot(predFLOOR_C5)

#Creating new column with predicted data
Wifi_ValidationSetNOZero$predFLOOR_C5 <- predFLOOR_C5

#Checking that FLOOR and PredFLOOR_C5 are both factors
Wifi_ValidationSetNOZero$predFLOOR_C5 <- as.factor(Wifi_ValidationSetNOZero$predFLOOR_C5)
Wifi_ValidationSetNOZero$FLOOR <- as.factor(Wifi_ValidationSetNOZero$FLOOR)

#Checking the metrics
confusionMatrix(Wifi_ValidationSetNOZero$predFLOOR_C5, Wifi_ValidationSetNOZero$FLOOR)

#Adding column with errors to the dataframe
Wifi_ValidationSetNOZero  <- mutate(Wifi_ValidationSetNOZero, errorsFLOOR = predFLOOR_C5 - FLOOR) 

#Storing the predicted values, actual values and errors in a tibble
resultsFLOOR <- tibble(.rows = 1111)

#Adding FLOOR and its prediction to the tibble
resultsFLOOR$predFLOOR_C5 <- predFLOOR_C5
resultsFLOOR$FLOOR <-Wifi_ValidationSetNOZero$FLOOR

#Storing the file
saveRDS(resultsFLOOR, file = "resultsFLOORC5(V1).rds")