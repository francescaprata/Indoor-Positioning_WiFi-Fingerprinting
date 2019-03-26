###########################################################
# Name: Francesca Prata                                   #                          
# Project: WiFi Locationing                               #                  
# Script: Benchmark KNN model for predicting building ID  #                  
# Date: 12 March 2019                                     #                  
# Version: 5                                              #                  
###########################################################

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
indexTrain <- createDataPartition(y = Wifi_TrainSet$BUILDINGID, 
                                  p = .4, 
                                  list = FALSE)

training <- Wifi_TrainSet[indexTrain,]
testing <- Wifi_TrainSet[-indexTrain,]

#Only use WAPs to predict BUILDING ID
training <- select(training, -FLOOR, -SPACEID, -RELATIVEPOSITION, 
                   -USERID, -PHONEID, -TIMESTAMP, -LONGITUDE, - LATITUDE)

#Setting cross validation parameters
fitcontrolKNN <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 1,
                              verboseIter = TRUE) 

#Training KNN model
set.seed(123)
KnnFitbuilding <- train(BUILDINGID~., 
                        training, 
                        method = "Knn",
                        metric = "Accuracy",
                        trControl = fitcontrolKNN)

#Predicting BUILDING ID from the training data
predBUILD_KNN <- predict(KnnFitbuilding, newdata = testing)

#Creating new column with predicted data
testing$predBUILD_KNN <- predBUILD_KNN

#Checking metrics 
confusionMatrix(testing$predBUILD_KNN, testing$BUILDINGID)


############################################################
#                       VALIDATION DATA                    #
############################################################

#Predicting BUILDINGID from the validation data
predBUILD_KNN <- predict(KnnFitbuilding,Wifi_ValidationSet)

#Creating a new column with the predictions
Wifi_ValidationSet$predBUILD_KNN <- predBUILD_KNN

#Checking that BUILDINGID and predBUILD_KNN are both factors
Wifi_ValidationSet$predBUILD_KNN <- as.factor(Wifi_ValidationSet$predBUILD_KNN)
Wifi_ValidationSet$BUILDINGID <- as.factor(Wifi_ValidationSet$BUILDINGID)

#Checking the metrics
confusionMatrix(Wifi_ValidationSet$predBUILD_KNN,Wifi_ValidationSet$BUILDINGID)

#Adding column with errors to the dataframe
Wifi_ValidationSet$predBUILD_KNN <- as.integer(Wifi_ValidationSet$predBUILD_KNN)
Wifi_ValidationSet$BUILDINGID <- as.integer(Wifi_ValidationSet$BUILDINGID)
Wifi_ValidationSet  <- mutate(Wifi_ValidationSet, errorsBUILD = predBUILD_KNN - BUILDINGID) 

#Turning errors into factors to produce a plot
Wifi_ValidationSet$errorsBUILD <- as.factor(Wifi_ValidationSet$errorsBUILD)
plot(Wifi_ValidationSet$errorsBUILD,
     main = "BUILDING predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")

#Storing the predicted values, actual values and errors in a tibble
resultsBUILDINGID <- tibble(.rows = 1111)

#Adding BUILDINGID and its prediction to the tibble 
resultsBUILDINGID$predBUILD_KNN <- predBUILD_KNN
resultsBUILDINGID$BUILDINGID <-Wifi_ValidationSet$BUILDINGID

#Turning them numerical in order to mutate the errors 
#and add them to the tibble
resultsBUILDINGID$predBUILD_KNN <- as.integer(resultsBUILDINGID$predBUILD_KNN)
resultsBUILDINGID$BUILDINGID <- as.integer(resultsBUILDINGID$BUILDINGID)
resultsBUILDINGID  <- mutate(resultsBUILDINGID, errorsBUILD = predBUILD_KNN - BUILDINGID) 

#Storing the file
saveRDS(resultsBUILDINGID, file = "resultsBUILDINGID(V4).rds")

#Visual exploration of errors 
plot_ly(Wifi_ValidationSet, 
        x = ~LATITUDE, 
        y = ~LONGITUDE, 
        z = ~BUILDINGID, 
        colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsBUILD == 0, size = 1) %>%
  layout(title = "Wrongly predicted BUILDINGIDs",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'BUILDING ID'))) 

Wifi_ValidationSet$predBUILD_KNN <- as.factor(Wifi_ValidationSet$predBUILD_KNN)
ggplot(Wifi_ValidationSet, aes(x=LONGITUDE, y=LATITUDE, color = predBUILD_KNN))+
  geom_point() + 
  facet_wrap(~BUILDINGID) +
  theme_light() +
  labs(title="Wrongly predicted BUILDINGIDs")
