
# data description

rm(list=ls()); gc(); 

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/')

combinedData.orig <- read.csv2("finalizedData29122018.csv")

# kalajärvi dropped because the neighborhood is not present in the maps / total of 3 sales 
combinedData.orig <- combinedData.orig[combinedData.orig$NeighborhoodFinalized != "Kalajärvi",]; 
nrow(combinedData.orig)

# alppila renamed to "alppiharju"
combinedData.orig$NeighborhoodFinalized[combinedData.orig$NeighborhoodFinalized == "Alppila"] <- "Alppiharju";
combinedData.orig$NeighborhoodFinalized <- factor(combinedData.orig$NeighborhoodFinalized);

# summary statistics for the price

combinedData <- data.frame(Price = combinedData.orig$Price, 
                           Sqm = combinedData.orig$SquareMeters, 
                           CondGoodDummySqm = combinedData.orig$ConditionGoodDummy*combinedData.orig$SquareMeters,
                           Age = combinedData.orig$AgeOfTheBuilding,
                           TwoRoomsDummy = combinedData.orig$TwoRoomsDummy, 
                           ThreeRoomsDummy = combinedData.orig$ThreeRoomsDummy, 
                           FourRoomsOrMoreDummy = combinedData.orig$FourRoomsOrMoreDummy,
                           OwnFloor = combinedData.orig$ownFloor.fixed,
                           SaunaDummy = combinedData.orig$SaunaDummy,
                           NeighborhoodAssignment = as.numeric(combinedData.orig$NeighborhoodFinalized));


table(combinedData.orig$ConditionGoodDummy)

# summary for the explanatory variables

summary(combinedData.orig)
summary(combinedData)

# geographical distribution of the sales
  # \item The neighborhood name 
library(xtable)
neighborhoodTable <- table(combinedData.orig$NeighborhoodFinalized)
 

neighborhoodTable <- data.frame(Name = names(neighborhoodTable), 
                                Frequency = as.numeric(neighborhoodTable))


readyTable <- cbind(neighborhoodTable[1:43,], 
                    neighborhoodTable[44:(44+42),], 
                    neighborhoodTable[87:(87+42),]);

rownames(readyTable) <- NULL; 


xtable(cbind(neighborhoodTable[1:43,], 
             neighborhoodTable[44:(44+42),], 
             neighborhoodTable[87:(87+42),]))

table(combinedData$NeighborhoodAssignment)

# \item The type of the apartment
table(combinedData.orig$ApartmentTypeRaw)

# \item The type of the building	
xtable(table(combinedData.orig$HouseType))

# \item The year the building was built  
yearTable <- table(combinedData.orig$YearBuilt)
yearTable <- data.frame(Year = names(yearTable), Frequency = as.numeric(yearTable))
  
xtable(cbind(yearTable[1:45,], 
             yearTable[46:(46+44),], 
             yearTable[91:(91+44),]))


# \item Information whether the building has a elevator
xtable(table(combinedData.orig$ElevatorDummy))


# \item The condition of the apartment
xtable(table(combinedData.orig$ConditionRaw))


# \item The energy classification of the building
xtable(table(combinedData.orig$EnergyClassificationLetterClass))


# \item The square meters of the apartment
# \item The sales price 
# \item The sales price per square meters
# \item The floor where the house is located 

xtable(table(combinedData.orig$NumberOfRooms))

xtable(table(combinedData.orig$SaunaDummy))

##################
# 16.9.2019 descriptive figures for distance data


setwd('/home/asdf/Desktop/gradu/git/housingprices/data preparation/')

rm(list=ls()); gc(); 

combinedData.orig <- read.csv2("/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/finalizedData29122018.csv")
oceanRoadDistanceData <- read.csv("/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/oceanDistancesComplete.csv") # for neighborhood keys
distanceMatrixData <- read.csv("/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/distanceMatrix.csv") # for distance metrics for the covariance structure

summary(oceanRoadDistanceData)

scalingCoef <- 1.3; 
png('./figures/oceanDistHist.png', width = 600*scalingCoef, height = 400*scalingCoef);  
hist(oceanRoadDistanceData$distance, 
     nclass = 20, 
     xlab = "Distance to ocean polygon (meters)", 
     main = "Ocean distance histogram", 
     ylim = c(0,50),
     xlim = c(0, 20000))
dev.off();

png('./figures/roadDistHist.png', width = 600*scalingCoef, height = 400*scalingCoef);  
hist(oceanRoadDistanceData$roadDistanceToCenter, 
     nclass = 20, 
     xlab = "Distance to center of Helsinki over road graph (meters)", 
     main = "Road distance to center histogram",
     ylim = c(0, 25))
dev.off()

distanceMatrixData <- distanceMatrixData[,-1]
colnames.distMat <-colnames(distanceMatrixData)
chomp <- function(string) substr(string, 2, nchar(string))
colnames.distMat <- sapply(colnames.distMat, chomp)
colnames.distMat <- as.numeric(colnames.distMat)
distanceMatrixData <- distanceMatrixData[, order(colnames.distMat)]


distances <- distanceMatrixData[lower.tri(distanceMatrixData)]


png('./figures/pairwiseHist.png', width = 600*scalingCoef, height = 400*scalingCoef);  
hist(distances/1000, 
     nclass = 20, 
     xlab = "Distance (kilometers)", 
     main = "Pairwise distances between neighborhood centroids\n")
dev.off()


##################
# 6.11.2019 descriptive figures for distance data as a table 


setwd('/home/asdf/Desktop/gradu/git/housingprices/data preparation/')

rm(list=ls()); gc(); 

combinedData.orig <- read.csv2("/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/finalizedData29122018.csv")
oceanRoadDistanceData <- read.csv("/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/oceanDistancesComplete.csv") # for neighborhood keys
distanceMatrixData <- read.csv("/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/distanceMatrix.csv") # for distance metrics for the covariance structure


distanceMatrixData <- distanceMatrixData[,-1]
colnames.distMat <- colnames(distanceMatrixData)
chomp <- function(string) substr(string, 2, nchar(string))
colnames.distMat <- sapply(colnames.distMat, chomp)
colnames.distMat <- as.numeric(colnames.distMat)
distanceMatrixData <- distanceMatrixData[, order(colnames.distMat)]

distances <- distanceMatrixData[lower.tri(distanceMatrixData)]

xtable(rbind(
  summary(oceanRoadDistanceData$distance),
  summary(oceanRoadDistanceData$roadDistanceToCenter),
  summary(distances/1000)))


