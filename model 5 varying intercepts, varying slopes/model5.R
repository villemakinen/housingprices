
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

# fake data explanatory variables generation

Sqm.fakeData <- rgamma(n = nFakeObs, shape = 67^2/1073, rate = 67/1073)
Age.fakeData <- rnbinom(nFakeObs, size = 2, prob = 41/850)
SaunaDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData$SaunaDummy)/nrow(combinedData), replace = T)
OwnFloor.fakeData <- rpois(n = nFakeObs, lambda = 3)
ConditionGoodDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData.orig$ConditionGoodDummy)/nrow(combinedData.orig), replace = T)

NumberOfRooms.fakeData <- sample(1:8, nFakeObs, prob = table(combinedData.orig$NumberOfRooms)/nrow(combinedData.orig), replace = T)
TwoRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 2);
ThreeRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 3);
FourRoomsOrMoreDummy.fakeData <- 1*(NumberOfRooms.fakeData >= 4);

# drawing "true" coefficient values 

Age_coef <- rnorm(n = 1, mean = -2000, sd = 1.5e3);
TwoRoomsDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 5e3);
ThreeRoomsDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 5e3);
FourRoomsOrMoreDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 5e3);
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

# coefficient draws 
library(rethinking); # see https://github.com/rmcelreath/statrethinking_winter2019
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

par(mfrow=c(1,2))
hist(Price.fakeData)
hist(combinedData.orig$Price)
par(mfrow=c(1,1))

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

stanFit.fakeData <- sampling(object = model5.stanObj, 
                             data = list(N = nrow(fakeData), 
                                         N_neighborhood = numberOfGroups,
                                         Price = fakeData$Price, 
                                         Sqm = fakeData$Sqm,
                                         CondGoodDummySqm = fakeData$CondGoodDummySqm,
                                         Age = fakeData$Age,
                                         TwoRoomsDummy = fakeData$TwoRoomsDummy,
                                         ThreeRoomsDummy = fakeData$ThreeRoomsDummy, 
                                         FourRoomsOrMoreDummy = fakeData$FourRoomsOrMoreDummy,
                                         OwnFloor = fakeData$OwnFloor,
                                         SaunaDummy = fakeData$SaunaDummy,
                                         NeighborhoodAssignment = fakeData$NeighborhoodAssignment),
                             cores = 4)

print(stanFit.fakeData)
traceplot(stanFit.fakeData)

# dev.off()
# for(name in stanFit.fakeData@model_pars) {
#   plot(traceplot(stanFit.fakeData, pars = name))
#   readline(prompt = "traceplot next...")  
# }

posteriorSamples.fakeData <- as.matrix(stanFit.fakeData)

trueValues <- c(EV_betaSquareMetersGoodCond, 
                EV_betaSquareMeters, 
                EV_betaIntercept, 
                sigma_Neighborhood_SqmGoodCond, 
                sigma_Neighborhood_Sqm, 
                sigma_Neighborhood_Intercept, 
                Age_coef,    
                TwoRoomsDummy_coef,  
                ThreeRoomsDummy_coef,   
                FourRoomsOrMoreDummy_coef,   
                OwnFloor_coef,    
                SaunaDummy_coef, 
                sigma, 
                nu,
                RhoMatrix[1,1],
                RhoMatrix[2,1],
                RhoMatrix[3,1],
                RhoMatrix[1,2],
                RhoMatrix[2,2],
                RhoMatrix[3,2],
                RhoMatrix[1,3],
                RhoMatrix[2,3],
                RhoMatrix[3,3])
names(trueValues) <- c("Mu_CondGoodSqm_coef", 
                       "Mu_Sqm_coef",
                       "Mu_Intercept_coef",
                       "Sigma_CondGoodSqm_coef",
                       "Sigma_Sqm_coef",
                       "Sigma_Intercept_coef",
                       "Age_coef",
                       "TwoRoomsDummy_coef",
                       "ThreeRoomsDummy_coef",
                       "FourRoomsOrMoreDummy_coef",
                       "OwnFloor_coef",
                       "SaunaDummy_coef",
                       "sigma",
                       "nu",
                       "Rho[1,1]",
                       "Rho[2,1]",                 
                       "Rho[3,1]",
                       "Rho[1,2]",
                       "Rho[2,2]",
                       "Rho[3,2]",
                       "Rho[1,3]",                 
                       "Rho[2,3]",
                       "Rho[3,3]")

for(k in 1:length(trueValues)) {
  hist(posteriorSamples.fakeData[,names(trueValues)[k]], main = names(trueValues)[k])
  cat("parameter", names(trueValues)[k], "value", trueValues[k], "\n")
  abline(v = trueValues[k], col = 'red', lty = 2, lwd = 2)
  checkEnd <- readline(prompt = "q to end: "); 
  
  if(checkEnd == 'q') {
    break; 
  }
}

# checking loo statistics
library(loo)
looObj.fakeData <- loo(stanFit.fakeData, cores = 4)
looObj.fakeData

############################
# estimating the model with true data

set.seed(123); 
testSetIndeces <- sample(1:nrow(combinedData.orig), round(0.3*nrow(combinedData.orig)), replace = F)

estimationSet <- combinedData[-testSetIndeces,]
testSet <- combinedData[testSetIndeces,]

stanFit.trueData <- sampling(object = model5.stanObj, 
                             data = list(N = nrow(estimationSet), 
                                         N_neighborhood = max(combinedData$NeighborhoodAssignment),
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
                             iter = 4000,
                             cores = 4,
                             seed = 1234)
print(stanFit.trueData)
traceplot(stanFit.trueData)

# for(name in stanFit.trueData@model_pars) {
#     plot(traceplot(stanFit.trueData, pars = name))
#     readline(prompt = "traceplot next...")
# }
  

posteriorSamples.trueData <- as.matrix(stanFit.trueData)

# k <- 1; 
# # k <- 4163; # for the varying intercept, slopes 
# while(T) {
#   hist(posteriorSamples.trueData[,k], main = colnames(posteriorSamples.trueData)[k])
#   readline(prompt = "traceplot next...")
#   plot(traceplot(stanFit.trueData, par = colnames(posteriorSamples.trueData)[k]))
#   
#   checkEnd <- readline(prompt = "q to end: "); 
#   if(checkEnd == 'q') {
#     break; 
#   }
#   k <- k + 1; 
# }

############################################################################################################
# loo statistics necessary for model comparions and calculating stacking weights 

# checking loo statistics
looObj.trueData <- loo(stanFit.trueData, cores = 4)
looObj.trueData

############################################################################################################
# functions for generating poterior predictive functions for model stacking  

getPosteriorPredictiveDraws <- function(dataSet, postSample, likelihoodSigmaName, likelihoodNuName) {
  
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  
  coefDraws <- coefDraws[,-grep("Mu", colnames(coefDraws))]
  coefDraws <- coefDraws[,-grep("Sigma", colnames(coefDraws))]
  
  muData <- dataSet[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  nonGroupVaryingPredictorContribution <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  # adding the effects of varying intercep  
  interceptEstimateDraws <- postSample[,grep("VaryingSlopes\\[\\d+,1\\]", colnames(postSample))]
  interceptContribution <- t(interceptEstimateDraws[,dataSet$NeighborhoodAssignment])
  
  # adding the effects of Sqm
  sqmCoefEstimateDraws <- postSample[,grep("VaryingSlopes\\[\\d+,2\\]", colnames(postSample))]
  sqmData <- dataSet[,"Sqm"]; 
  
  sqmContribution <- sqmData*t(sqmCoefEstimateDraws[,dataSet$NeighborhoodAssignment]);
  
  # adding the effects of CondGoodSqm
  condGoodSqmCoefEstimateDraws <- postSample[,grep("VaryingSlopes\\[\\d+,3\\]", colnames(postSample))]
  condGoodSqmData <- dataSet[,"CondGoodDummySqm"]; 
  
  condGoodSqmContribution <-  condGoodSqmData*t(condGoodSqmCoefEstimateDraws[,dataSet$NeighborhoodAssignment]);
  
  muEstimateDraws <- nonGroupVaryingPredictorContribution + interceptContribution + sqmContribution + condGoodSqmContribution; 
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  predictiveDraws <- apply(muEstimateDraws, 1, function(x) {x + sigmaEstimateDraws*rt(n = length(x), df = nuEstimateDraws)})
  
  return(predictiveDraws)
}


############################################################################################################
# storing posterior draws, loo object, function for drawing from posterior predictice distribution for further use   

save.image("modelFit5.RData");

############################################################################################################

# estimation set means
postPredDistDraws.estimation <- getPosteriorPredictiveDraws(dataSet = estimationSet, 
                                                            postSample = posteriorSamples.trueData, 
                                                            likelihoodSigmaName = "sigma", 
                                                            likelihoodNuName = "nu")

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

plot(estimationSet$Price, predDistMean.estimation)
abline(a = 0, b = 1, lty = 2, col = 'red')

# test set means 
postPredDistDraws.test <- getPosteriorPredictiveDraws(dataSet = testSet, 
                                                      postSample = posteriorSamples.trueData, 
                                                      likelihoodSigmaName = "sigma", 
                                                      likelihoodNuName = "nu")

predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

plot(testSet$Price, predDistMean.test)
abline(a = 0, b = 1, lty = 2, col = 'red')


drawVariancePostSample <- function(postSample, likelihoodSigmaName, likelihoodNuName) {
  sigmaPostSample <- postSample[,likelihoodSigmaName];
  nuPostSample <- postSample[,likelihoodNuName];
  
  varSample <- (sigmaPostSample^2) * (nuPostSample/(nuPostSample-2));
  
  return(varSample); 
}

variancePostSample <- drawVariancePostSample(postSample = posteriorSamples.trueData, 
                                             likelihoodSigmaName = "sigma", 
                                             likelihoodNuName = "nu")
hist(variancePostSample)
summary(variancePostSample)

largDifIndeces.test <- order(abs(testSet$Price - predDistMean.test), decreasing = T);


k <- 151; 
targetIndex <- largDifIndeces.test[k];
hist(postPredDistDraws.test[,targetIndex], nclass = 50)
abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);




