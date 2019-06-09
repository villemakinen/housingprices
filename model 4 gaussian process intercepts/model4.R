
library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/')

rm(list=ls()); gc(); 

combinedData.orig <- read.csv2("finalizedData29122018.csv")
oceanRoadDistanceData <- read.csv("oceanDistancesComplete.csv") # for neighborhood keys
distanceMatrixData <- read.csv("distanceMatrix.csv") # for distance metrics for the covariance structure

# kalajärvi dropped because the neighborhood is not present in the maps / total of 3 sales 
combinedData.orig <- combinedData.orig[combinedData.orig$NeighborhoodFinalized != "Kalajärvi",]; 
nrow(combinedData.orig)

# renaming and transforming the variables for EV calculations
combinedData <- data.frame(Price = combinedData.orig$Price, 
                           Sqm = combinedData.orig$SquareMeters,
                           CondGoodDummySqm = combinedData.orig$ConditionGoodDummy*combinedData.orig$SquareMeters,
                           Age = combinedData.orig$AgeOfTheBuilding,
                           TwoRoomsDummy = combinedData.orig$TwoRoomsDummy,
                           ThreeRoomsDummy = combinedData.orig$ThreeRoomsDummy, 
                           FourRoomsOrMoreDummy = combinedData.orig$FourRoomsOrMoreDummy,
                           OwnFloor = combinedData.orig$ownFloor.fixed,
                           SaunaDummy = combinedData.orig$SaunaDummy,
                           NeighborhoodFinalized = combinedData.orig$NeighborhoodFinalized);

combinedData$NeighborhoodFinalized[combinedData$NeighborhoodFinalized == "Alppila"] <- "Alppiharju";

combinedData$NeighborhoodFinalized <- factor(combinedData$NeighborhoodFinalized); 
combinedData$NeighborhoodId <- as.numeric(combinedData$NeighborhoodFinalized); 

library(hash); 
distanceIdHashNameToId <- hash(keys = tolower(oceanRoadDistanceData$nimiYhd), 
                               values = oceanRoadDistanceData$idYhd)

combinedData.hashDf <- unique(combinedData[,c("NeighborhoodId", "NeighborhoodFinalized")])

combinedDataIdHashNameToId <- hash(keys = tolower(combinedData.hashDf$NeighborhoodFinalized), 
                                   values = combinedData.hashDf$NeighborhoodId)


idMapping <- data.frame(originalName = tolower(combinedData$NeighborhoodFinalized), 
                        originalId = sapply(tolower(combinedData$NeighborhoodFinalized), function(x) combinedDataIdHashNameToId[[x]]),
                        distanceId = sapply(tolower(combinedData$NeighborhoodFinalized), function(x) distanceIdHashNameToId[[x]]))

limitedDistanceMatrix <- distanceMatrixData[distanceMatrixData$ID %in% idMapping$distanceId,]

allNames <- colnames(limitedDistanceMatrix)

colNameBooleans <- data.frame(orig = allNames, transformed = allNames, stringsAsFactors = F) 
colNameBooleans$transformed <- sapply(colNameBooleans$transformed, FUN = function(x) substr(x, 2, nchar(x)))
colNameBooleans$transformed[colNameBooleans$orig == "ID"] <- "-1"
colNameBooleans$transformed <- as.numeric(colNameBooleans$transformed)
colNameBooleans$inclusionBoolean <- colNameBooleans$transformed %in% limitedDistanceMatrix$ID; 

limitedDistanceMatrix <- limitedDistanceMatrix[,colNameBooleans$inclusionBoolean];

colnames(limitedDistanceMatrix) <- sapply(colnames(limitedDistanceMatrix), function(x) substr(x, 2, nchar(x))); 
limitedDistanceMatrix <- limitedDistanceMatrix[,order(as.numeric(colnames(limitedDistanceMatrix)))];

idHash <- hash(keys = as.character(idMapping$distanceId), values = idMapping$originalId); 

salesIdsForDistances <- sapply(rownames(limitedDistanceMatrix), FUN = function(x) idHash[[x]]);

rownames(limitedDistanceMatrix) <- salesIdsForDistances;
colnames(limitedDistanceMatrix) <- salesIdsForDistances;

shuffleOrder <- order(as.numeric(rownames(limitedDistanceMatrix)))
limitedDistanceMatrix <- limitedDistanceMatrix[shuffleOrder,]
limitedDistanceMatrix <- limitedDistanceMatrix[,shuffleOrder]

# scale to kilo meters
limitedDistanceMatrix <- limitedDistanceMatrix/1000;

############################
# fake data generation

nFakeObs <- 3000; 

# sampling the fake data
#   sampling parameters chosen roughly based on combinedData means and variances where appropriate
set.seed(123)

SquareMeters.fakeData <- rgamma(n = nFakeObs, shape = 67^2/1073, rate = 67/1073)
AgeOfTheBuilding.fakeData <- rnbinom(nFakeObs, size = 2, prob = 41/850)
SaunaDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData.orig$SaunaDummy)/nrow(combinedData.orig), replace = T)
ownFloor.fixed.fakeData <- rpois(n = nFakeObs, lambda = 3)
ConditionGoodDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData.orig$ConditionGoodDummy)/nrow(combinedData.orig), replace = T)

NumberOfRooms.fakeData <- sample(1:8, nFakeObs, prob = table(combinedData.orig$NumberOfRooms)/nrow(combinedData.orig), replace = T)
TwoRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 2);
ThreeRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 3);
FourRoomsOrMoreDummy.fakeData <- 1*(NumberOfRooms.fakeData >= 4);

# priors 
Sqm_coef <- rnorm(n = 1, mean = 6000, sd = 3e3);
CondGoodDummySqm_coef <- rnorm(n = 1, mean = 1e3, sd = 1.5e3);
Age_coef <- rnorm(n = 1, mean = -2000, sd = 2.5e3);
TwoRoomsDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 1e4);
ThreeRoomsDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
FourRoomsOrMoreDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
SaunaDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 2.5e3);
OwnFloor_coef <- rnorm(n = 1, mean = 1e3, sd = 1e3); 

# covariance matrix for intercept draws
N_Neighborhoods <- nrow(limitedDistanceMatrix)

covarianceMatrix <- matrix(0, nrow = nrow(limitedDistanceMatrix), ncol = ncol(limitedDistanceMatrix))

rhosq <- rhcauchy(n = 1, 1);
etasq <- rhcauchy(n = 1, 300)

# sigmaInterceptDiag <- 50000 + rhcauchy(1, 1000000);

for(k in 1:(nrow(covarianceMatrix)-1)) {
  for(l in (k+1):ncol(covarianceMatrix)) {
    #cat('k:', k, 'l:',l,'\n'); 
    covarianceMatrix[k,l] <- etasq*exp(-rhosq*(limitedDistanceMatrix[k,l]^2));
    covarianceMatrix[l,k] <- covarianceMatrix[k,l];
  }
}

for ( k in 1:ncol(covarianceMatrix) ) {
  # covarianceMatrix[k,k] = etasq + sigmaInterceptDiag;
  covarianceMatrix[k,k] = etasq + 0.01;
}

# drawing group assignments
numberOfGroups <- ncol(covarianceMatrix); 

groupNames <- 1:numberOfGroups;  
groupDist <- table(combinedData$NeighborhoodFinalized)
groupDist <- groupDist/sum(groupDist);

groupAssignments <- sample(groupNames, size = nFakeObs, replace = T, prob = groupDist)

library(MASS)

interceptEv <- 70; 
groupIntercepts.unscaled <- mvrnorm(n = 1, mu = rep(interceptEv, length(groupNames)), Sigma = covarianceMatrix);
groupIntercepts.unscaled

groupIntercepts <- groupIntercepts.unscaled*1000;  

EV.fakeData <- groupIntercepts[groupAssignments] + 
  TwoRoomsDummy_coef*TwoRoomsDummy.fakeData + 
  ThreeRoomsDummy_coef*ThreeRoomsDummy.fakeData + 
  FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy.fakeData + 
  (Sqm_coef + CondGoodDummySqm_coef*ConditionGoodDummy.fakeData)*SquareMeters.fakeData +  
  Age_coef*AgeOfTheBuilding.fakeData + 
  SaunaDummy_coef*SaunaDummy.fakeData + 
  OwnFloor_coef*ownFloor.fixed.fakeData;


sigma <- 10000 + rhcauchy(1, 5000); 
nu <- rgamma(1, shape = 2, rate = 0.1)
Price.fakeData <- EV.fakeData + sigma*rt(n = length(EV.fakeData), df = nu);

par(mfrow=c(1,2)); 
hist(Price.fakeData)
hist(combinedData$Price)
par(mfrow=c(1,1));

fakeData <- data.frame(Price = Price.fakeData,
                       Group = groupAssignments,
                       Sqm = SquareMeters.fakeData, 
                       Age = AgeOfTheBuilding.fakeData, 
                       NoOfRooms = NumberOfRooms.fakeData, 
                       SaunaDummy = SaunaDummy.fakeData, 
                       OwnFloor = ownFloor.fixed.fakeData, 
                       CondGoodDummy = ConditionGoodDummy.fakeData,
                       TwoRoomsDummy = TwoRoomsDummy.fakeData,
                       ThreeRoomsDummy = ThreeRoomsDummy.fakeData, 
                       FourRoomsOrMoreDummy = FourRoomsOrMoreDummy.fakeData)

estimationIndeces <- sample(1:nrow(fakeData), size = round(0.7*nrow(fakeData)))

estimationFakeData <- fakeData[estimationIndeces,];
testFakeData <- fakeData[-estimationIndeces,];

############################
# estimating the model with fake data

library(rstan)

# estimating the model specification for the fake data 
model4.stanObj <- stan_model(file = 'model4.stan');

stanFit.fakeData <- sampling(object = model4.stanObj, 
                             data = list(N = nrow(estimationFakeData), 
                                         N_neighborhood = numberOfGroups,
                                         Price = estimationFakeData$Price, 
                                         Sqm = estimationFakeData$Sqm,
                                         CondGoodDummySqm = estimationFakeData$CondGoodDummy*estimationFakeData$Sqm,
                                         Age = estimationFakeData$Age,
                                         TwoRoomsDummy = estimationFakeData$TwoRoomsDummy,
                                         ThreeRoomsDummy = estimationFakeData$ThreeRoomsDummy, 
                                         FourRoomsOrMoreDummy = estimationFakeData$FourRoomsOrMoreDummy,
                                         OwnFloor = estimationFakeData$OwnFloor,
                                         SaunaDummy = estimationFakeData$SaunaDummy,
                                         NeighborhoodAssignment = estimationFakeData$Group,
                                         Dmat = limitedDistanceMatrix
                             ),
                             iter = 2000, verbose = T, cores = 2, chains = 4)

print(stanFit.fakeData)
summary(stanFit.fakeData)
plot(stanFit.fakeData)
traceplot(stanFit.fakeData)

posteriorSamples.fakeData <- as.matrix(stanFit.fakeData)

colnames(posteriorSamples.fakeData)

trueValues <- c(Sqm_coef, CondGoodDummySqm_coef, Age_coef, TwoRoomsDummy_coef, ThreeRoomsDummy_coef, FourRoomsOrMoreDummy_coef, SaunaDummy_coef, OwnFloor_coef, sigma, nu, etasq, rhosq)
names(trueValues) <- c("Sqm_coef", "CondGoodDummySqm_coef", "Age_coef", "TwoRoomsDummy_coef", "ThreeRoomsDummy_coef", "FourRoomsOrMoreDummy_coef", "SaunaDummy_coef", "OwnFloor_coef",  "sigma", "nu", "etasq", "rhosq")

for(k in 1:length(trueValues)) {
  hist(posteriorSamples.fakeData[,k], main = colnames(posteriorSamples.fakeData)[k])
  cat("parameter", names(trueValues)[k], "value", trueValues[k], "\n")
  abline(v = trueValues[k], col = 'red', lty = 2, lwd = 2)
  checkEnd <- readline(prompt = "q to end: "); 
  
  if(checkEnd == 'q') {
    break; 
  }
}

# for(k in 3187:(3187 + 171)) {
#   hist(posteriorSamples.fakeData[,k], main = colnames(posteriorSamples.fakeData)[k])
#   abline(v = groupIntercepts[k - 3186], col = 'red', lty = 2, lwd = 2)
#   checkEnd <- readline(prompt = "q to end: "); 
#   
#   if(checkEnd == 'q') {
#     break; 
#   }
# }


# checking loo statistics
library(loo)
looObj.fakeData <- loo(stanFit.fakeData)
looObj.fakeData

# true data
set.seed(123); 
testSetIndeces <- sample(1:nrow(combinedData.orig), round(0.3*nrow(combinedData.orig)), replace = F)

estimationSet <- combinedData[-testSetIndeces,]
testSet <- combinedData[testSetIndeces,]

stanFit.trueData <- sampling(object = model3.stanObj, 
                             data = list(N = nrow(estimationSet), 
                                         N_neighborhood = nrow(distanceData),
                                         OceanDistance = distanceData$oceanDistance, 
                                         RoadDistance = distanceData$roadDistanceToCenter, 
                                         Price = estimationSet$Price, 
                                         Sqm = estimationSet$Sqm,
                                         CondGoodDummySqm = estimationSet$CondGoodDummySqm,
                                         Age = estimationSet$Age,
                                         TwoRoomsDummy = estimationSet$TwoRoomsDummy,
                                         ThreeRoomsDummy = estimationSet$ThreeRoomsDummy, 
                                         FourRoomsOrMoreDummy = estimationSet$FourRoomsOrMoreDummy,
                                         OwnFloor = estimationSet$OwnFloor,
                                         SaunaDummy = estimationSet$SaunaDummy, 
                                         NeighborhoodAssignment = estimationSet$NeighborhoodAssignment), 
                             iter = 1000,
                             cores = 4,
                             # iter = 500,
                             # cores = 4,
                             seed = 1234,
                             control = list(max_treedepth = 15))
print(stanFit.trueData)
traceplot(stanFit.trueData)

# save.image("modelFit.RData")

posteriorSamples.trueData <- as.matrix(stanFit.trueData)

