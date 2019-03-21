#########################################################
# Name: Francesca Prata                                 #
# Project: WiFi Locationing                             #
# Script: Subsetting by BUILDINGID and predicting FLOOR #
# Date: 14 March 2019                                   #
# Version: 3                                            #
#########################################################

#Retrieving the necessary scripts
source(file = "2.PreProcess_v2.R")

#Turning FLOOR into numeric before subsetting
Wifi_TrainSetNOZero$FLOOR <- as.numeric(Wifi_TrainSetNOZero$FLOOR)

#Subsetting according to BUILDINGID
build0 <- subset(Wifi_TrainSetNOZero, BUILDINGID == 0)
build1 <- subset(Wifi_TrainSetNOZero, BUILDINGID == 1)
build2 <- subset(Wifi_TrainSetNOZero, BUILDINGID == 2)

#Turning FLOOR back to factor
build0$FLOOR <- as.factor(build0$FLOOR)
build1$FLOOR <- as.factor(build1$FLOOR)
build2$FLOOR <- as.factor(build2$FLOOR)

#Do the same for validation set
Wifi_ValidationSetNOZero$FLOOR <- as.numeric(Wifi_ValidationSetNOZero$FLOOR)

Valbuild0 <- subset(Wifi_ValidationSetNOZero, BUILDINGID == 0)
Valbuild1 <- subset(Wifi_ValidationSetNOZero, BUILDINGID == 1)
Valbuild2 <- subset(Wifi_ValidationSetNOZero, BUILDINGID == 2)

Valbuild0$FLOOR <- as.factor(Valbuild0$FLOOR)
Valbuild1$FLOOR <- as.factor(Valbuild1$FLOOR)
Valbuild2$FLOOR <- as.factor(Valbuild2$FLOOR)

##############
# BUILDING 0 #
##############

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = build0$FLOOR,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- build0[inTrain,]
testing <- build0[-inTrain,]

training <- select(training, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)


#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
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
                     trControl = fitControlKnn)

#Making predictions with model and predicting FLOOR 
predFLOOR_KNN <- predict(KnnFitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and PredictionKnn_floor are both factors
testing$FLOOR <- as.factor(testing$FLOOR)
testing$predFLOOR_KNN <- as.factor(testing$predFLOOR_KNN)

#Checking the metrics 
confusionMatrix(testing$predFLOOR_KNN, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_KNN <- predict(KnnFitfloor,Valbuild0)

#Creating a new column with the predictions
Valbuild0$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and predFLOOR_KNN are both factors
Valbuild0$predFLOOR_KNN <- as.factor(Valbuild0$predFLOOR_KNN)
Valbuild0$FLOOR <- as.factor(Valbuild0$FLOOR)

#Checking the metrics 
confusionMatrix(Valbuild0$predFLOOR_KNN,Valbuild0$FLOOR)

##############
# BUILDING 1 #
##############

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = build1$FLOOR,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- build1[inTrain,]
testing <- build1[-inTrain,]

training <- select(training, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)


#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
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
                     trControl = fitControlKnn)
                    
#Making predictions with model and predicting FLOOR 
predFLOOR_KNN <- predict(KnnFitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and PredictionKnn_floor are both factors
testing$FLOOR <- as.factor(testing$FLOOR)
testing$predFLOOR_KNN <- as.factor(testing$predFLOOR_KNN)

#Checking the metrics 
confusionMatrix(testing$predFLOOR_KNN, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_KNN <- predict(KnnFitfloor,Valbuild1)

#Creating a new column with the predictions
Valbuild1$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and predFLOOR_KNN are both factors
Valbuild1$predFLOOR_KNN <- as.factor(Valbuild1$predFLOOR_KNN)
Valbuild1$FLOOR <- as.factor(Valbuild1$FLOOR)

#Checking the metrics 
confusionMatrix(Valbuild1$predFLOOR_KNN,Valbuild1$FLOOR)

##############
# BUILDING 2 #
##############

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Data Partition
set.seed(123)
inTrain<- createDataPartition(y = build2$FLOOR,
                              p = 0.4,
                              list = FALSE)

## Separating the data into training and testing 
training <- build2[inTrain,]
testing <- build2[-inTrain,]

training <- select(training, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)


#Setting cross validation parameters
fitControlKnn <- trainControl(method = "repeatedcv",
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
                     trControl = fitControlKnn)

#Making predictions with model and predicting FLOOR 
predFLOOR_KNN <- predict(KnnFitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and PredictionKnn_floor are both factors
testing$FLOOR <- as.factor(testing$FLOOR)
testing$PredictionKnn_floor <- as.factor(testing$predFLOOR_KNN)

#Checking the metrics 
confusionMatrix(testing$predFLOOR_KNN, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_KNN <- predict(KnnFitfloor,Valbuild2)

#Creating a new column with the predictions
Valbuild2$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and predFLOOR_KNN are both factors
Valbuild2$predFLOOR_KNN <- as.factor(Valbuild2$predFLOOR_KNN)
Valbuild2$FLOOR <- as.factor(Valbuild2$FLOOR)

#Checking the metrics 
confusionMatrix(Valbuild2$predFLOOR_KNN,Valbuild2$FLOOR)

