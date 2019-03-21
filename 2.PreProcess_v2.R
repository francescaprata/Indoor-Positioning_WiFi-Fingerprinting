#############################
# Name: Francesca Prata     #
# Project: WiFi Locationing #
# Script: Pre-processing    #
# Date: 12 March 2019       #
# Version: 3                #
############################

#Retrieving the necessary scripts
source(file = "1.Import_v3.R")

#####################################
# Modifying the values of the  WAPs #
#####################################

#Subsetting the waps 
Sub_WAPS <- select(Wifi_TrainSet, WAP001:WAP520)

#Replacing the value of 100 to -106 as 100 is no signal and values
#higher than 90 mean that the signal is very low  
Sub_WAPS <- replace(Sub_WAPS, Sub_WAPS==100, -106)

#Creating a new dataframe with the modified variables
Extract <- select(Wifi_TrainSet, LONGITUDE, LATITUDE, FLOOR, 
                  BUILDINGID, SPACEID, RELATIVEPOSITION, 
                  USERID, PHONEID,TIMESTAMP)
Wifi_TrainSet2 <- bind_cols(Sub_WAPS, Extract) 
rm(Extract)
rm(Sub_WAPS)

#Doing the same for the validation set
Sub_WAPS <- select(Wifi_ValidationSet, WAP001:WAP520)
Sub_WAPS <- replace(Sub_WAPS, Sub_WAPS==100, -106)
Extract <- select(Wifi_ValidationSet, LONGITUDE, LATITUDE, FLOOR, 
                  BUILDINGID, SPACEID, RELATIVEPOSITION, 
                  USERID, PHONEID,TIMESTAMP)
Wifi_ValidationSet2 <- bind_cols(Sub_WAPS, Extract) 
rm(Extract)
rm(Sub_WAPS)

############################
# Values between 0 and -30 #
############################

#Values between 0 and -30 Mbps are outside the range of WiFi signals
#Thus, let us convert them to no signal (-106)
Sub_WAPS <- select(Wifi_TrainSet2, WAP001:WAP520)
Sub_WAPS <- as.data.frame(lapply(Sub_WAPS, function(x){ifelse(x > -30 & x <0, -106, x+0)}))
Extract <- select(Wifi_TrainSet2, LONGITUDE, LATITUDE, FLOOR, 
                  BUILDINGID, SPACEID, RELATIVEPOSITION, 
                  USERID, PHONEID,TIMESTAMP)
Wifi_TrainSet2 <- bind_cols(Sub_WAPS, Extract) 
rm(Extract)
rm(Sub_WAPS)

#Same for validation
Sub_WAPS <- select(Wifi_ValidationSet2, WAP001:WAP520)
Sub_WAPS <- as.data.frame(lapply(Sub_WAPS, function(x){ifelse(x > -30 & x <0, -106, x+0)}))
Extract <- select(Wifi_ValidationSet2, LONGITUDE, LATITUDE, FLOOR, 
                  BUILDINGID, SPACEID, RELATIVEPOSITION, 
                  USERID, PHONEID,TIMESTAMP)
Wifi_ValidationSet2 <- bind_cols(Sub_WAPS, Extract) 
rm(Extract)
rm(Sub_WAPS)

#################
# Zero variance # 
#################

#Removing the COLUMNS with zero variance
Wifi_ValidationSetNOZero <- Wifi_ValidationSet2[, - as.numeric(which
                                                               (apply
                                                                 (Wifi_TrainSet2,
                                                                           2, 
                                                                           var) == 0))]
Wifi_TrainSetNOZero <- Wifi_TrainSet2[, - as.numeric(which(apply(Wifi_TrainSet2,
                                                                 2, 
                                                                 var) == 0))]

#Checking if both training and validation have same number of columns 
all.equal(colnames(Wifi_TrainSetNOZero),
          colnames(Wifi_ValidationSetNOZero))

#Removing the ROWS with zero variance in both training and validation 
listZVRows <- apply(Wifi_TrainSetNOZero %>% select(starts_with("WAP")),
                    1, 
                    var ) == 0
Wifi_TrainSetNOZero <- Wifi_TrainSetNOZero[!listZVRows,]

listZVRows <- apply(Wifi_ValidationSetNOZero %>% select(starts_with("WAP")),
                    1, 
                    var ) == 0
Wifi_ValidationSetNOZero <- Wifi_ValidationSetNOZero[!listZVRows,]

#Removing redundant dataframes 
rm(Wifi_TrainSet2)
rm(Wifi_ValidationSet2)








