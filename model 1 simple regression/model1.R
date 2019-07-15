
# simple model with t-distribution likelihood

library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/')

rm(list=ls()); gc();
# load(file = 'modelFit1.RData')

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
                           SaunaDummy = combinedData.orig$SaunaDummy);

############################
# fake data generation

nFakeObs <- 3000; 

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
Intercept_coef <- rnorm(n = 1, mean = 0.7e5, sd = 5e3);
Sqm_coef <- rnorm(n = 1, mean = 4.5e3, sd = 1e3);
CondGoodDummySqm_coef <- rnorm(n = 1, mean = 0.5e3, sd = 1e3);
Age_coef <- rnorm(n = 1, mean = -1.5e3, sd = 1e3);
TwoRoomsDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 1e4);
ThreeRoomsDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
FourRoomsOrMoreDummy_coef <- rnorm(n = 1, mean = 7.5e3, sd = 1e4);
SaunaDummy_coef <- rnorm(n = 1, mean = 5e3, sd = 2.5e3);
OwnFloor_coef <- rnorm(n = 1, mean = 7e3, sd = 1e3); 

mu <- Intercept_coef + 
  (Sqm_coef + CondGoodDummySqm_coef*ConditionGoodDummy.fakeData)*SquareMeters.fakeData + 
  Age_coef*AgeOfTheBuilding.fakeData + 
  TwoRoomsDummy_coef*TwoRoomsDummy.fakeData +
  ThreeRoomsDummy_coef*ThreeRoomsDummy.fakeData +
  FourRoomsOrMoreDummy_coef*FourRoomsOrMoreDummy.fakeData + 
  SaunaDummy_coef*SaunaDummy.fakeData +
  OwnFloor_coef*OwnFloor.fakeData;

# sigma <- 10000 + rhcauchy(1, 5000);
sigma <- rhcauchy(1, 15000);

nu <- rgamma(1, shape = 2, rate = 0.1)

Price.fakeData <- mu + sigma*rt(n = length(mu), df = nu)

fakeData <- data.frame(Price = Price.fakeData,
                       Sqm = SquareMeters.fakeData, 
                       CondGoodDummySqm = SquareMeters.fakeData*ConditionGoodDummy.fakeData,
                       Age = AgeOfTheBuilding.fakeData, 
                       NoOfRooms = NumberOfRooms.fakeData, 
                       SaunaDummy = SaunaDummy.fakeData, 
                       OwnFloor = OwnFloor.fakeData, 
                       CondGoodDummy = ConditionGoodDummy.fakeData,
                       TwoRoomsDummy = TwoRoomsDummy.fakeData,
                       ThreeRoomsDummy = ThreeRoomsDummy.fakeData, 
                       FourRoomsOrMoreDummy = FourRoomsOrMoreDummy.fakeData)

###########
# fitting the model - fake data 

library(rstan)
model1.stanObj <- stan_model(file = 'model1.stan'); 

stanFit.fakeData <- sampling(object = model1.stanObj, 
                             data = list(N = nrow(fakeData), 
                                         Price = fakeData$Price, 
                                         Sqm = fakeData$Sqm,
                                         CondGoodDummySqm = fakeData$CondGoodDummySqm,
                                         Age = fakeData$Age,
                                         TwoRoomsDummy = fakeData$TwoRoomsDummy,
                                         ThreeRoomsDummy = fakeData$ThreeRoomsDummy, 
                                         FourRoomsOrMoreDummy = fakeData$FourRoomsOrMoreDummy,
                                         OwnFloor = fakeData$OwnFloor,
                                         SaunaDummy = fakeData$SaunaDummy),
                             cores = 4)

print(stanFit.fakeData)
# plot(stanFit.fakeData)
traceplot(stanFit.fakeData)

posteriorSamples.fakeData <- as.matrix(stanFit.fakeData)

# checking that mass actually concentrates around the true values... 
trueValues <- c(Intercept_coef,
                Sqm_coef,
                CondGoodDummySqm_coef,
                Age_coef,
                TwoRoomsDummy_coef,
                ThreeRoomsDummy_coef,
                FourRoomsOrMoreDummy_coef,
                SaunaDummy_coef,
                OwnFloor_coef,
                sigma, 
                nu)
names(trueValues) <- c("Intercept_coef",
                       "Sqm_coef",
                       "CondGoodDummySqm_coef",
                       "Age_coef",
                       "TwoRoomsDummy_coef",
                       "ThreeRoomsDummy_coef",
                       "FourRoomsOrMoreDummy_coef",
                       "SaunaDummy_coef",
                       "OwnFloor_coef", 
                       "sigma", 
                       "nu")

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
testSetIndeces <- sample(1:nrow(combinedData), round(0.3*nrow(combinedData)), replace = F)

estimationSet <- combinedData[-testSetIndeces,]
testSet <- combinedData[testSetIndeces,]

stanFit.trueData <- sampling(object = model1.stanObj, 
                         data = list(N = nrow(estimationSet), 
                                     Price = estimationSet$Price, 
                                     Sqm = estimationSet$Sqm,
                                     CondGoodDummySqm = estimationSet$CondGoodDummySqm,
                                     Age = estimationSet$Age,
                                     TwoRoomsDummy = estimationSet$TwoRoomsDummy,
                                     ThreeRoomsDummy = estimationSet$ThreeRoomsDummy, 
                                     FourRoomsOrMoreDummy = estimationSet$FourRoomsOrMoreDummy,
                                     OwnFloor = estimationSet$OwnFloor,
                                     SaunaDummy = estimationSet$SaunaDummy), 
                         iter = 4000, 
                         cores = 4,
                         seed = 1234);

print(stanFit.trueData)
# plot(stanFit.trueData)
traceplot(stanFit.trueData)

############################################################################################################
# loo statistics necessary for model comparions and calculating stacking weights 

# checking loo statistics
looObj.trueData <- loo(stanFit.trueData)
looObj.trueData

############################################################################################################
# functions for generating poterior predictive functions for model stacking  

posteriorSamples.trueData <- as.matrix(stanFit.trueData)

getPosteriorPredictiveDraws <- function(dataSet, postSample, likelihoodSigmaName, likelihoodNuName) {
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  
  dataSet$Intercept <- rep(1,nrow(dataSet)); 
  
  muData <- dataSet[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  
  muEstimateDraws <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  predictiveDraws <-apply(muEstimateDraws, 1, function(x) {x + sigmaEstimateDraws*rt(n = length(x), df = nuEstimateDraws)})
  
  return(predictiveDraws)
}

############################################################################################################
# storing posterior draws, loo object, function for drawing from posterior predictice distribution for further use   

save.image("modelFit1.RData");

############################################################################################################
# graphing means  

# estimation set means
postPredDistDraws.estimation <- getPosteriorPredictiveDraws(dataSet = estimationSet, postSample = posteriorSamples.trueData, likelihoodSigmaName = "sigma", likelihoodNuName = "nu")

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

plot(estimationSet$Price, predDistMean.estimation)
abline(a = 0, b = 1, lty = 2, col = 'red')

# largDifIndeces.estimation <- order(abs(estimationSet$Price - predDistMean.estimation), decreasing = T);
# k <- 5; 
# targetIndex <- largDifIndeces.estimation[k];
# hist(postPredDistDraws.estimation[,targetIndex], nclass = 50)
# abline(v = estimationSet$Price[targetIndex], col = 'red', lty = 2);

# test set means 
postPredDistDraws.test <- getPosteriorPredictiveDraws(dataSet = testSet, postSample = posteriorSamples.trueData, likelihoodSigmaName = "sigma", likelihoodNuName = "nu")

predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

plot(testSet$Price, predDistMean.test)
abline(a = 0, b = 1, lty = 2, col = 'red')

# largDifIndeces.test <- order(abs(testSet$Price - predDistMean.test), decreasing = T);
# k <- 2; 
# targetIndex <- largDifIndeces.test[k];
# hist(postPredDistDraws.test[,targetIndex], nclass = 50)
# abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);

########################################################################################################################################################################

drawVariancePostSample <- function(postSample, likelihoodSigmaName, likelihoodNuName) {
  sigmaPostSample <- postSample[,likelihoodSigmaName];
  nuPostSample <- postSample[,likelihoodNuName];
  
  varSample <- (sigmaPostSample^2) * (nuPostSample/(nuPostSample-2));
  
  return(varSample); 
}

variancePostSample <- drawVariancePostSample(postSample = posteriorSamples.trueData, likelihoodSigmaName = "sigma", likelihoodNuName = "nu")
hist(variancePostSample)
summary(variancePostSample)

# - LOO R^2, avehtari.github.io/bayes_R2/bayes_R2.html
# http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf

getBayesianR2Draws <- function(postPredictiveDistDraws, residualVarianceDraws) {
  # following (3) and appendix of http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
  var_fit <- apply(X = postPredictiveDistDraws, MARGIN = 1, FUN = var)
  return(var_fit/(var_fit + residualVarianceDraws))
}

BayesianR2Draws <- getBayesianR2Draws(postPredictiveDistDraws = postPredDistDraws.estimation, residualVarianceDraws = variancePostSample);
hist(BayesianR2Draws); 
abline(v = median(BayesianR2Draws), lty = 2, col = 'red')





