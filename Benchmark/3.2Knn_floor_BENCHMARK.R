####################################################
# Name: Francesca Prata                            #                          
# Project: WiFi Locationing                        #                  
# Script: Benchmark KNN model for predicting floor #                  
# Date: 12 March 2019                              #                  
# Version: 5                                       #                  
####################################################

#Retrieving previous dataframe 
source(file = "1.Import_v3.R")

##########################################################
#                       TRAINING DATA                    #
##########################################################

#Turning BUILDINGID and FLOOR into factors
Wifi_TrainSet$BUILDINGID <- as.factor(Wifi_TrainSet$BUILDINGID)
Wifi_TrainSet$FLOOR <- as.factor(Wifi_TrainSet$FLOOR)

#Data partition
set.seed(123)
indexTrain <- createDataPartition(y = Wifi_TrainSet$FLOOR, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSet[indexTrain,]
testing <- Wifi_TrainSet[-indexTrain,]

#Only use WAPs to predict FLOOR
training <- select(training, -BUILDINGID, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)

#Setting cross validation parameters
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              verboseIter = TRUE) 

#Training KNN model
set.seed(123)
KnnFitfloor <- train(FLOOR~., 
                        training, 
                        method = "knn",
                        metric = "Accuracy",
                        trControl = fitcontrolKNN)

#Predicting FLOOR from the training data
predFLOOR_KNN <- predict(KnnFitfloor, newdata = testing)

#Creating new column with predicted data
testing$predFLOOR_KNN <- predFLOOR_KNN

#Checking metrics 
confusionMatrix(testing$predFLOOR_KNN, testing$FLOOR)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting FLOOR from the validation data
predFLOOR_KNN <- predict(KnnFitfloor, Wifi_ValidationSet)
plot(predFLOOR_KNN)

#Creating a new column with the predictions
Wifi_ValidationSet$predFLOOR_KNN <- predFLOOR_KNN

#Checking that FLOOR and predBFLOOR_KNN are both factors
Wifi_ValidationSet$predFLOOR_KNN <- as.factor(Wifi_ValidationSet$predFLOOR_KNN)
Wifi_ValidationSet$FLOOR <- as.factor(Wifi_ValidationSet$FLOOR)

#Checking the metrics
confusionMatrix(Wifi_ValidationSet$predFLOOR_KNN,Wifi_ValidationSet$FLOOR)

#Adding column with errors to the dataframe
Wifi_ValidationSet$predFLOOR_KNN <- as.integer(Wifi_ValidationSet$predFLOOR_KNN)
Wifi_ValidationSet$FLOOR <- as.integer(Wifi_ValidationSet$FLOOR)
Wifi_ValidationSet  <- mutate(Wifi_ValidationSet, errorsFLOOR = predFLOOR_KNN - FLOOR) 

#Turning errors into factors to produce a plot
Wifi_ValidationSet$errorsFLOOR <- as.factor(Wifi_ValidationSet$errorsFLOOR)
plot(Wifi_ValidationSet$errorsFLOOR,
     main = "FLOOR predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")

#Storing the predicted values, actual values and errors in a tibble
resultsFLOOR <- tibble(.rows = 1111)

#Adding FLOOR and its prediction to the tibble
resultsFLOOR$predFLOOR_KNN <- predFLOOR_KNN
resultsFLOOR$FLOOR <-Wifi_ValidationSet$FLOOR

#Turning them numerical in order to mutate the errors 
#and add them to the tibble
resultsFLOOR$predFLOOR_KNN <- as.integer(resultsFLOOR$predFLOOR_KNN)
resultsFLOOR$FLOOR <- as.integer(resultsFLOOR$FLOOR)
resultsFLOOR  <- mutate(resultsFLOOR, errorsFLOOR = predFLOOR_KNN - FLOOR) 

#Storing the file
saveRDS(resultsFLOOR, file = "resultsFLOOR(V4).rds")

#Visual exploration of errors
plot_ly(Wifi_ValidationSet, 
        x = ~LATITUDE, 
        y = ~LONGITUDE, 
        z = ~FLOOR, 
        colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0, size = 1) %>%
  layout(title = "Wrongly predicted FLOORs",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

Wifi_ValidationSet$predFLOOR_KNN <- as.factor(Wifi_ValidationSet$predFLOOR_KNN)
ggplot(Wifi_ValidationSet, aes(x=LONGITUDE, y=LATITUDE, color = predFLOOR_KNN))+
  geom_point() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Wrongly predicted FLOORs")
