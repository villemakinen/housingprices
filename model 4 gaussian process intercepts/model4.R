
library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/')

rm(list=ls()); gc(); 
# load(file = 'modelFit4.RData')


combinedData.orig <- read.csv2("finalizedData29122018.csv")
oceanRoadDistanceData <- read.csv("oceanDistancesComplete.csv") # for neighborhood keys
distanceMatrixData <- read.csv("distanceMatrix.csv") # for distance metrics for the covariance structure

# kalajärvi dropped because the neighborhood is not present in the maps / total of 3 sales 
combinedData.orig <- combinedData.orig[combinedData.orig$NeighborhoodFinalized != "Kalajärvi",]; 
nrow(combinedData.orig)

# alppila renamed to "alppiharju"
combinedData.orig$NeighborhoodFinalized[combinedData.orig$NeighborhoodFinalized == "Alppila"] <- "Alppiharju";
combinedData.orig$NeighborhoodFinalized <- factor(combinedData.orig$NeighborhoodFinalized);

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

combinedData$NeighborhoodFinalized <- factor(combinedData$NeighborhoodFinalized); 
combinedData$NeighborhoodAssignment <- as.numeric(combinedData$NeighborhoodFinalized); 

################################################
# preparing distance data for the covariance structure

library(hash); 
distanceIdHashNameToId <- hash(keys = tolower(oceanRoadDistanceData$nimiYhd), 
                               values = oceanRoadDistanceData$idYhd)

combinedData.hashDf <- unique(combinedData[,c("NeighborhoodAssignment", "NeighborhoodFinalized")])

combinedDataIdHashNameToId <- hash(keys = tolower(combinedData.hashDf$NeighborhoodFinalized), 
                                   values = combinedData.hashDf$NeighborhoodAssignment)

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

# scale to kilometers
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
OwnFloor.fakeData <- rpois(n = nFakeObs, lambda = 3)
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
  OwnFloor_coef*OwnFloor.fakeData;

# sigma <- 10000 + rhcauchy(1, 5000);
sigma <- rhcauchy(1, 15000); 
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
                       OwnFloor = OwnFloor.fakeData, 
                       CondGoodDummy = ConditionGoodDummy.fakeData,
                       TwoRoomsDummy = TwoRoomsDummy.fakeData,
                       ThreeRoomsDummy = ThreeRoomsDummy.fakeData, 
                       FourRoomsOrMoreDummy = FourRoomsOrMoreDummy.fakeData)


############################
# estimating the model with fake data

library(rstan)

# estimating the model specification for the fake data 
model4.stanObj <- stan_model(file = 'model4.stan');

stanFit.fakeData <- sampling(object = model4.stanObj, 
                             data = list(N = nrow(fakeData), 
                                         N_neighborhood = numberOfGroups,
                                         Price = fakeData$Price, 
                                         Sqm = fakeData$Sqm,
                                         CondGoodDummySqm = fakeData$CondGoodDummy*fakeData$Sqm,
                                         Age = fakeData$Age,
                                         TwoRoomsDummy = fakeData$TwoRoomsDummy,
                                         ThreeRoomsDummy = fakeData$ThreeRoomsDummy, 
                                         FourRoomsOrMoreDummy = fakeData$FourRoomsOrMoreDummy,
                                         OwnFloor = fakeData$OwnFloor,
                                         SaunaDummy = fakeData$SaunaDummy,
                                         NeighborhoodAssignment = fakeData$Group,
                                         Dmat = limitedDistanceMatrix),
                             cores = 4,
                             control = list(adapt_delta = 0.95) # to avoid divergent transitions
                             )

print(stanFit.fakeData)
traceplot(stanFit.fakeData)

posteriorSamples.fakeData <- as.matrix(stanFit.fakeData)

# checking that mass actually concentrates around the true values... 

# trueValues <- c(Sqm_coef,
#                 CondGoodDummySqm_coef,
#                 Age_coef,
#                 TwoRoomsDummy_coef,
#                 ThreeRoomsDummy_coef,
#                 FourRoomsOrMoreDummy_coef,
#                 SaunaDummy_coef,
#                 OwnFloor_coef,
#                 rhosq,
#                 etasq,
#                 sigma,
#                 nu)
# names(trueValues) <- c("Sqm_coef",
#                        "CondGoodDummySqm_coef",
#                        "Age_coef",
#                        "TwoRoomsDummy_coef",
#                        "ThreeRoomsDummy_coef",
#                        "FourRoomsOrMoreDummy_coef",
#                        "SaunaDummy_coef",
#                        "OwnFloor_coef",
#                        "rhosq",
#                        "etasq",
#                        "sigma",
#                        "nu")
# 
# for(k in 1:length(trueValues)) {
#   hist(posteriorSamples.fakeData[,names(trueValues)[k]], main = names(trueValues)[k])
#   cat("parameter", names(trueValues)[k], "value", trueValues[k], "\n")
#   abline(v = trueValues[k], col = 'red', lty = 2, lwd = 2)
#   checkEnd <- readline(prompt = "q to end: "); 
#   
#   if(checkEnd == 'q') {
#     break; 
#   }
# }

# checking loo statistics
library(loo)
looObj.fakeData <- loo(stanFit.fakeData, cores = 4)
looObj.fakeData

############################################################################################

# true data
set.seed(9); 
testSetIndeces <- sample(1:nrow(combinedData.orig), round(0.3*nrow(combinedData.orig)), replace = F)

estimationSet <- combinedData[-testSetIndeces,]
testSet <- combinedData[testSetIndeces,]

stanFit.trueData <- sampling(object = model4.stanObj, 
                             data = list(N = nrow(estimationSet), 
                                         N_neighborhood = nrow(limitedDistanceMatrix),
                                         Price = estimationSet$Price, 
                                         Sqm = estimationSet$Sqm,
                                         CondGoodDummySqm = estimationSet$CondGoodDummy*estimationSet$Sqm,
                                         Age = estimationSet$Age,
                                         TwoRoomsDummy = estimationSet$TwoRoomsDummy,
                                         ThreeRoomsDummy = estimationSet$ThreeRoomsDummy, 
                                         FourRoomsOrMoreDummy = estimationSet$FourRoomsOrMoreDummy,
                                         OwnFloor = estimationSet$OwnFloor,
                                         SaunaDummy = estimationSet$SaunaDummy,
                                         NeighborhoodAssignment = estimationSet$NeighborhoodAssignment,
                                         Dmat = limitedDistanceMatrix),
                             iter = 4000,
                             cores = 4,
                             seed = 1234)
print(stanFit.trueData)
traceplot(stanFit.trueData)

# save.image("modelFit.RData")

posteriorSamples.trueData <- as.matrix(stanFit.trueData)
############################################################################################################
# loo statistics necessary for model comparions and calculating stacking weights 

looObj.trueData <- loo(stanFit.trueData)
looObj.trueData

# used for stacking later 
pointwiseElpdLooVectorForStacking <- looObj.trueData$pointwise[,"elpd_loo"]; 

############################################################################################################
# functions for generating poterior predictive functions for model stacking  

getPosteriorPredictiveDraws <- function(dataSet, 
                                        postSample, 
                                        likelihoodSigmaName, 
                                        likelihoodNuName) {
  # group intercepts 
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  muData <- dataSet[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  nonInterceptPredictorContribution <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  # adding the intercepts
  interceptEstimateDraws <- postSample[,grep("intercepts\\[",colnames(postSample))]
  interceptContribution <- t(interceptEstimateDraws[,dataSet$NeighborhoodAssignment])
  
  # final parameters
  muEstimateDraws <- nonInterceptPredictorContribution + interceptContribution; 
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  # sample draw
  predictiveDraws <- apply(muEstimateDraws, 1, function(x) {x + sigmaEstimateDraws*rt(n = length(x), df = nuEstimateDraws)})
  
  return(predictiveDraws)
}

library(LaplacesDemon) # for the dst-function

evaluatePosteriorPredictive <- function(Price.pointEvaluation,  
                                        predictiveVariables, 
                                        postSample, 
                                        likelihoodSigmaName, 
                                        likelihoodNuName) {
  
  # group intercepts 
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  muData <- predictiveVariables[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  nonInterceptPredictorContribution <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  # adding the intercepts
  interceptEstimateDraws <- postSample[,grep("intercepts\\[",colnames(postSample))]
  interceptContribution <- t(interceptEstimateDraws[,predictiveVariables$NeighborhoodAssignment])
  
  # final parameters
  muEstimateDraws <- nonInterceptPredictorContribution + interceptContribution; 
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  lpd <- dst(x = Price.pointEvaluation, mu = muEstimateDraws, sigma = sigmaEstimateDraws, nu = nuEstimateDraws); 
  
  return(mean(lpd));
}

############################################################################################################
# storing posterior draws, loo object, function for drawing from posterior predictice distribution for further use   

save.image("modelFit4.RData");

############################################################################################################
# replicated price histograms compared to true price histogram   

dataReplications <- getPosteriorPredictiveDraws(dataSet = estimationSet, 
                                                postSample = posteriorSamples.trueData, 
                                                likelihoodSigmaName = "sigma", 
                                                likelihoodNuName = "nu")

replicatedVector <- dataReplications[6,]
trueVector <- estimationSet$Price;

hist(replicatedVector)
hist(trueVector)

hist(replicatedVector[replicatedVector >= quantile(replicatedVector, probs = 0.05) & replicatedVector <= quantile(replicatedVector, probs = 0.95)]);
hist(trueVector[trueVector >= quantile(trueVector, probs = 0.05) & trueVector <= quantile(trueVector, probs = 0.95)])

# distribution of mean, median, standard deviation of  prices  

replicatedPrices.mean <- apply(dataReplications, 1, mean);
summary(replicatedPrices.mean)
hist(replicatedPrices.mean)
abline(v = mean(estimationSet$Price), col = 'red', lwd = 1, lty = 2)

replicatedPrices.median <- apply(dataReplications, 1, median);
summary(replicatedPrices.median)
hist(replicatedPrices.median)
abline(v = median(estimationSet$Price), col = 'red', lwd = 1, lty = 2)

replicatedPrices.sd <- apply(dataReplications, 1, sd);
summary(replicatedPrices.sd)
hist(replicatedPrices.sd)
abline(v = sd(estimationSet$Price), col = 'red', lwd = 1, lty = 2)

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

k <- 16; 
targetIndex <- largDifIndeces.test[k];
hist(postPredDistDraws.test[,targetIndex], nclass = 50)
abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);
predDistMean.test[targetIndex]
testSet[targetIndex,]


getBayesianR2Draws <- function(postPredictiveDistDraws, residualVarianceDraws) {
  # following (3) and appendix of http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
  var_fit <- apply(X = postPredictiveDistDraws, MARGIN = 1, FUN = var)
  return(var_fit/(var_fit + residualVarianceDraws))
}

BayesianR2Draws <- getBayesianR2Draws(postPredictiveDistDraws = postPredDistDraws.estimation, 
                                      residualVarianceDraws = variancePostSample);
hist(BayesianR2Draws);
abline(v = median(BayesianR2Draws), lty = 2, col = 'red')
summary(BayesianR2Draws);




