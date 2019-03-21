#######################################################
# Name: Francesca Prata                               #
# Project: WiFi Locationing                           #
# Script: Plots of errors for each variable predicted #
# Date: 14 March 2019                                 #
# Version: 1                                          #
#######################################################

#The required previous to be loaded depends on which 
#is the variable of interest

#source(file = "3.Building_v4.R")
#source(file = "4.Floor_v5.R")
#source(file = "5.Subsetting_building_v3.R")
#source(file = "6.Latitude_v4.R")
#source(file = "7.Longitude_v4.R")

##########################
# PREDICTING BUILDING ID #
##########################

#Visual exploration of errors 
plot_ly(Wifi_ValidationSetNOZero, 
        x = ~LATITUDE, 
        y = ~LONGITUDE, 
        z = ~BUILDINGID, 
        colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsBUILD == 0, size = 1) %>%
  layout(title = "Wrongly predicted BUILDINGIDs",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'BUILDING ID'))) 



####################
# PREDICTING FLOOR #
####################

#Visual exploration of errors
plot_ly(Wifi_ValidationSetNOZero, 
        x = ~LATITUDE, 
        y = ~LONGITUDE, 
        z = ~FLOOR, 
        colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0, size = 1) %>%
  layout(title = "Wrongly predicted FLOORs",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

Wifi_ValidationSetNOZero$predFLOOR_KNN <- as.factor(Wifi_ValidationSetNOZero$predFLOOR_KNN)
ggplot(Wifi_ValidationSetNOZero, 
       aes(x=LONGITUDE, 
           y=LATITUDE, 
           color = predFLOOR_KNN))+
  geom_point() + 
  facet_wrap(~FLOOR) +
  theme_light() +
  labs(title="Wrongly predicted FLOORs")


###############################################
# VISUALIZING THE ERRORS - SUBSET OF BUILDING #
###############################################

#Let us take a closer look at what is going on in building 1

#Adding column with errors to the dataframe
Valbuild1$predFLOOR_KNN <- as.integer(Valbuild1$predFLOOR_KNN)
Valbuild1$FLOOR <- as.integer(Valbuild1$FLOOR)
Valbuild1 <- mutate(Valbuild1, errorsFLOOR = predFLOOR_KNN - FLOOR) 

#Turning errorsFLOOR back into factors to produce plot
Valbuild1$errorsFLOOR <- as.factor(Valbuild1$errorsFLOOR)

plot(Valbuild1$errorsFLOOR,
     main = "FLOOR predictions",
     xlab = "correct = 0 | incorrect != 0",
     ylab = "count")

#Where are the errors exactly?
plot_ly(Valbuild1, 
        x = ~LATITUDE, 
        y = ~LONGITUDE,
        z = ~FLOOR,
        colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorsFLOOR == 0, size = 1) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'))) 

#Subsetting the errors  
wrongFLOOR <- Valbuild1 %>%
  filter(errorsFLOOR != 0)
rightFLOOR <-Valbuild1 %>%
  filter(errorsFLOOR == 0)

#What do the errors have in common?
wrongFLOOR[,466:476]

#Saving the file
saveRDS(wrongFLOOR, file = "errorsFLOOR-validationKNNB1.rds")

#######################
# PREDICTING LATITUDE #
#######################

#Visual exploration of errors
plot_ly(testing, 
        x = ~LATITUDE, 
        y = ~LONGITUDE, 
        z = ~FLOOR, 
        colors = c('#ffb600','#00ff5d','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  #add_markers(color = ~SPACEID) %>%
  add_markers(color = testing$errorsLATITUDE > 8 | testing$errorsLATITUDE < -8) %>%
  # add_markers(color = ~errorsLONGITUDE > 8 | ~errorsLONGITUDE < -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'),
                      title = "Wrongly predicted latitude")) 

########################
# PREDICTING LONGITUDE #
########################

#Visual exploration of errors
plot_ly(Wifi_ValidationSetNOZero, 
        x = ~LATITUDE, 
        y = ~LONGITUDE, 
        z = ~FLOOR, 
        colors = c('#ffb600','#00ff5d','#0800ff')) %>%
  #add_markers(color = ~errorsBUILDING != 0) %>%
  #add_markers(color = ~SPACEID) %>%
  # add_markers(color = ~errorsLATITUDE > 8 | ~errorsLATITUDE < -8) %>%
  add_markers(color = ~errorsLON >= 8 | ~errorsLON <= -8) %>%
  layout(scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR'),
                      titel = "Wrongly predicted longitude"))
