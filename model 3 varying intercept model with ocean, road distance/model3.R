
# varying intercept model with t-distribution likelihood s.t. group means are dependent on ocean and road distances

library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/')

rm(list=ls()); gc(); 
# load(file = 'modelFit3.RData')


combinedData.orig <- read.csv2("finalizedData29122018.csv")

nrow(combinedData.orig)
# kalajärvi dropped because the neighborhood is not present in the maps / total of 3 sales 
combinedData.orig <- combinedData.orig[combinedData.orig$NeighborhoodFinalized != "Kalajärvi",]; 
nrow(combinedData.orig)

# alppila renamed to "alppiharju"
combinedData.orig$NeighborhoodFinalized[combinedData.orig$NeighborhoodFinalized == "Alppila"] <- "Alppiharju";

# distance data 
distanceData.orig <- read.csv("oceanDistancesComplete.csv")

combinedData.orig$NeighborhoodFinalized <- tolower(combinedData.orig$NeighborhoodFinalized)
distanceData.orig$nimiYhd <- factor(tolower(distanceData.orig$nimiYhd)); 

combinedData.orig$NeighborhoodFinalized <- factor(combinedData.orig$NeighborhoodFinalized, levels =  levels(distanceData.orig$nimiYhd))

# simple distance data
distanceData <- data.frame(identifier = distanceData.orig$nimiYhd, 
                           oceanDistance = distanceData.orig$distance, 
                           roadDistanceToCenter = distanceData.orig$roadDistanceToCenter)

distanceData <- distanceData[order(as.numeric(distanceData$identifier)),]


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
set.seed(12345)

nFakeObs <- 3000;

Intercept_pop_coef <- rnorm(1, mean = 150000, sd = 50000)
RoadDistance_pop_coef <- rnorm(1, mean = -5, sd = 3)
OceanDistance_pop_coef <- rnorm(1, mean = -5, sd = 3)

# sigma_pop <- 10000 + rhcauchy(n=1, sigma = 10000)
sigma_pop <- rhcauchy(n=1, sigma = 20000)

groupMu <- Intercept_pop_coef + distanceData$oceanDistance*OceanDistance_pop_coef + distanceData$roadDistanceToCenter*RoadDistance_pop_coef

groupIntercepts <- rnorm(n = nrow(distanceData), mean = groupMu, sd = sigma_pop)

# assigning groups
numberOfGroups <- length(levels(distanceData$identifier))

groupNames <- 1:numberOfGroups; 
groupDist <- table(combinedData.orig$NeighborhoodFinalized)
groupDist <- groupDist + 0.1; # to allow for fake data to come from neighborhoods where there were no sales
groupDist <- groupDist/sum(groupDist);

groupAssignments <- sample(groupNames, size = nFakeObs, replace = T, prob = groupDist)

# generating other covariates 
Sqm.fakeData <- rgamma(n = nFakeObs, shape = 67^2/1073, rate = 67/1073)
Age.fakeData <- rnbinom(nFakeObs, size = 2, prob = 41/850)
SaunaDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData.orig$SaunaDummy)/nrow(combinedData.orig), replace = T)
OwnFloor.fakeData <- rpois(n = nFakeObs, lambda = 3)
ConditionGoodDummy.fakeData <- sample(0:1, nFakeObs, prob = table(combinedData.orig$ConditionGoodDummy)/nrow(combinedData.orig), replace = T)

NumberOfRooms.fakeData <- sample(1:8, nFakeObs, prob = table(combinedData.orig$NumberOfRooms)/nrow(combinedData.orig), replace = T)
TwoRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 2);
ThreeRoomsDummy.fakeData <- 1*(NumberOfRooms.fakeData == 3);
FourRoomsOrMoreDummy.fakeData <- 1*(NumberOfRooms.fakeData >= 4);

# priors where the "true" parameter values are drawn
Sqm_coef <- rnorm(n = 1, mean = 5000, sd = 1e3);
CondGoodDummySqm_coef <- rnorm(n = 1, mean = 2e3, sd = 1e3);
Age_coef <- rnorm(n = 1, mean = -1000, sd = 1e3);
TwoRoomsDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 1e4);
ThreeRoomsDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
FourRoomsOrMoreDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
SaunaDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 2.5e3);
OwnFloor_coef <- rnorm(n = 1, mean = 1e3, sd = 1e3); 

# sigma <- 10000 + rhcauchy(1, 5000);
sigma <- rhcauchy(1, 15000);
nu <- rgamma(1, shape = 2, rate = 0.1)

EV.fakeData <- groupIntercepts[groupAssignments] + 
  Sqm_coef*Sqm.fakeData +
  CondGoodDummySqm_coef*Sqm.fakeData*ConditionGoodDummy.fakeData + 
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

###########
# fitting the model - fake data 

library(rstan)
model3.stanObj <- stan_model(file = 'model3.stan');

stanFit.fakeData <- sampling(object = model3.stanObj, 
                             data = list(N = nrow(fakeData), 
                                         N_neighborhood = nrow(distanceData),
                                         OceanDistance = distanceData$oceanDistance, 
                                         RoadDistance = distanceData$roadDistanceToCenter, 
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
# plot(stanFit.fakeData)
traceplot(stanFit.fakeData)

posteriorSamples.fakeData <- as.matrix(stanFit.fakeData)

# checking that mass actually concentrates around the true values... 
trueValues <- c(Sqm_coef,
                CondGoodDummySqm_coef,
                Age_coef,
                TwoRoomsDummy_coef,
                ThreeRoomsDummy_coef,
                FourRoomsOrMoreDummy_coef,
                SaunaDummy_coef,
                OwnFloor_coef,
                sigma,
                nu,
                Intercept_pop_coef,
                RoadDistance_pop_coef,
                OceanDistance_pop_coef,
                sigma_pop)
names(trueValues) <- c("Sqm_coef",
                       "CondGoodDummySqm_coef",
                       "Age_coef",
                       "TwoRoomsDummy_coef",
                       "ThreeRoomsDummy_coef",
                       "FourRoomsOrMoreDummy_coef",
                       "SaunaDummy_coef",
                       "OwnFloor_coef",
                       "sigma",
                       "nu",
                       "Intercept_pop_coef",
                       "RoadDistance_pop_coef",
                       "OceanDistance_pop_coef",
                       "sigma_pop")

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
looObj.fakeData <- loo(stanFit.fakeData)
looObj.fakeData

###########
# fitting the model - true data 

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

############################################################################################################
# functions for generating poterior predictive functions for model stacking  

getPosteriorPredictiveDraws <- function(dataSet, 
                                        distanceDataSet, 
                                        postSample, 
                                        likelihoodSigmaName, 
                                        likelihoodNuName) {
  # group intercepts 
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  coefDraws <- coefDraws[,-grep("Intercept_coef", colnames(coefDraws))]
  muData <- dataSet[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  nonInterceptPredictorContribution <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  # adding the intercepts
  interceptEstimateDraws <- postSample[,grep("Intercept_coef",colnames(postSample))]
  interceptContribution <- t(interceptEstimateDraws[,dataSet$NeighborhoodAssignment])
  
  # final parameters
  muEstimateDraws <- nonInterceptPredictorContribution + interceptContribution; 
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  # sample draw
  predictiveDraws <- apply(muEstimateDraws, 1, function(x) {x + sigmaEstimateDraws*rt(n = length(x), df = nuEstimateDraws)})
  
  return(predictiveDraws)
}


############################################################################################################
# storing posterior draws, loo object, function for drawing from posterior predictice distribution for further use   

save.image("modelFit3.RData");

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
k <- 6; 
targetIndex <- largDifIndeces.test[k];
hist(postPredDistDraws.test[,targetIndex], nclass = 50)
abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);

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



