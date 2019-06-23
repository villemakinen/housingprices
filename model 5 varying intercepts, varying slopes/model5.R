
# simple varying intercept, varying slopes model with t-distribution likelihood

library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/')

rm(list=ls()); gc(); 

combinedData.orig <- read.csv2("finalizedData29122018.csv")

nrow(combinedData.orig)
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
                           NeighborhoodAssignment = as.numeric(combinedData.orig$NeighborhoodFinalized));

############################
# fake data generation

nFakeObs <- 3000; 

set.seed(123);

Sqm.fakeData <- rgamma(n = nFakeObs, shape = 67^2/1073, rate = 67/1073)
Age.fakeData <- rnbinom(nFakeObs, size = 2, prob = 41/850)
SaunaDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData$SaunaDummy)/nrow(combinedData), replace = T)
OwnFloor.fakeData <- rpois(n = nFakeObs, lambda = 3)
ConditionGoodDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData.orig$ConditionGoodDummy)/nrow(combinedData.orig), replace = T)

NumberOfRooms.fakeData <- sample(1:8, nFakeObs, prob = table(combinedData.orig$NumberOfRooms)/nrow(combinedData.orig), replace = T)
TwoRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 2);
ThreeRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 3);
FourRoomsOrMoreDummy.fakeData <- 1*(NumberOfRooms.fakeData >= 4);

Age_coef <- rnorm(n = 1, mean = -2000, sd = 2.5e3);
TwoRoomsDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 1e4);
ThreeRoomsDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
FourRoomsOrMoreDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
SaunaDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 2.5e3);
OwnFloor_coef <- rnorm(n = 1, mean = 1e3, sd = 1e3); 

# drawing group assignments
numberOfGroups <- max(combinedData$NeighborhoodAssignment); 

groupNames <- 1:numberOfGroups;  

groupDist <- rep(0, length(groupNames)); 
names(groupDist) <- groupNames; 
groupDist[names(groupDist) %in% names(table(combinedData$NeighborhoodAssignment))] <- table(combinedData$NeighborhoodAssignment); 
groupDist <- groupDist + 0.01; 
groupDist <- groupDist/sum(groupDist);

groupAssignments <- sample(x = groupNames, size = nFakeObs, replace = T, prob = groupDist)

library(rethinking); 
library(extraDistr); 

# Sigma matrix for group specific coefficients
RhoMatrix <- rlkjcorr(n = 1, K = 3, eta = 2);

# how to parametrize???
# sigma_Neighborhood_Intercept <- 7000 + rhcauchy(n = 1, 1000);
sigma_Neighborhood_Intercept <- rhcauchy(n = 1, 7000);
# sigma_Neighborhood_Sqm <- 1000 + rhcauchy(n = 1, sigma = 500);
sigma_Neighborhood_Sqm <- rhcauchy(n = 1, sigma = 1500);
# sigma_Neighborhood_SqmGoodCond <- 200 + rhcauchy(n = 1, sigma = 150);
sigma_Neighborhood_SqmGoodCond <-rhcauchy(n = 1, sigma = 350);

sigma_Neighborhood <- c(sigma_Neighborhood_Intercept, sigma_Neighborhood_Sqm, sigma_Neighborhood_SqmGoodCond)

SigmaMatrix <- diag(sigma_Neighborhood) %*% RhoMatrix %*% diag(sigma_Neighborhood);

# Mean for groups
EV_betaIntercept <- rnorm(n = 1, mean = 40000, sd = 10000); 
EV_betaSquareMeters <- rnorm(n = 1, mean = 4000,  sd = 1000);
EV_betaSquareMetersGoodCond <- rnorm(n = 1, mean = 1000, sd = 500);

Mean <- c(EV_betaIntercept, EV_betaSquareMeters, EV_betaSquareMetersGoodCond)

# drawing the group specific coefficients
groupCoeffs <- rmvnorm(n = numberOfGroups, mean = Mean, sigma = SigmaMatrix)

# sigma <- 10000 + rhcauchy(1, 5000);
sigma <- rhcauchy(1, 15000);
nu <- rgamma(1, shape = 2, rate = 0.1)

EV.fakeData <- groupCoeffs[groupAssignments,1] + 
  groupCoeffs[groupAssignments,2]*Sqm.fakeData +
  groupCoeffs[groupAssignments,3]*Sqm.fakeData*ConditionGoodDummy.fakeData + 
  Age_coef*Age.fakeData + 
  SaunaDummy_coef*SaunaDummy.fakeData + 
  OwnFloor_coef*OwnFloor.fakeData + 
  TwoRoomsDummy_coef*TwoRoomsDummy.fakeData + 
  ThreeRoomsDummy_coef*ThreeRoomsDummy.fakeData +  
  FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy.fakeData

Price.fakeData <- EV.fakeData + sigma*rt(n = length(EV.fakeData), df = nu)
hist(Price.fakeData)

fakeData <- data.frame(Price = Price.fakeData, 
                       Sqm = Sqm.fakeData,
                       CondGoodDummySqm = Sqm.fakeData*ConditionGoodDummy.fakeData,
                       Age = Age.fakeData,
                       TwoRoomsDummy = TwoRoomsDummy.fakeData,
                       ThreeRoomsDummy = ThreeRoomsDummy.fakeData, 
                       FourRoomsOrMoreDummy = FourRoomsOrMoreDummy.fakeData,
                       OwnFloor = OwnFloor.fakeData,
                       SaunaDummy = SaunaDummy.fakeData,
                       NeighborhoodAssignment = groupAssignments);

############################
# estimating the model with fake data

library(rstan)

model5.stanObj <- stan_model(file = 'model5.stan');


