
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
combinedData.orig$NeighborhoodFinalized <- factor(combinedData.orig$NeighborhoodFinalized); 

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
# trueValues <- c(Sqm_coef,
#                 CondGoodDummySqm_coef,
#                 Age_coef,
#                 TwoRoomsDummy_coef,
#                 ThreeRoomsDummy_coef,
#                 FourRoomsOrMoreDummy_coef,
#                 SaunaDummy_coef,
#                 OwnFloor_coef,
#                 sigma,
#                 nu,
#                 Intercept_pop_coef,
#                 RoadDistance_pop_coef,
#                 OceanDistance_pop_coef,
#                 sigma_pop)
# names(trueValues) <- c("Sqm_coef",
#                        "CondGoodDummySqm_coef",
#                        "Age_coef",
#                        "TwoRoomsDummy_coef",
#                        "ThreeRoomsDummy_coef",
#                        "FourRoomsOrMoreDummy_coef",
#                        "SaunaDummy_coef",
#                        "OwnFloor_coef",
#                        "sigma",
#                        "nu",
#                        "Intercept_pop_coef",
#                        "RoadDistance_pop_coef",
#                        "OceanDistance_pop_coef",
#                        "sigma_pop")
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
looObj.fakeData <- loo(stanFit.fakeData)
looObj.fakeData

###########
# fitting the model - true data 

set.seed(9); 
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
  coefDraws <- coefDraws[,-grep("Intercept_coef", colnames(coefDraws))]
  coefDraws <- coefDraws[,-grep("Distance", colnames(coefDraws))]
  coefDraws <- coefDraws[,-grep("Intercept_pop", colnames(coefDraws))]
  
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

library(LaplacesDemon) # for the dst-function

evaluatePosteriorPredictive <- function(Price.pointEvaluation,  
                                        predictiveVariables, 
                                        postSample, 
                                        likelihoodSigmaName, 
                                        likelihoodNuName) {
  
  # group intercepts 
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  coefDraws <- coefDraws[,-grep("Intercept_coef", colnames(coefDraws))]
  coefDraws <- coefDraws[,-grep("Distance", colnames(coefDraws))]
  coefDraws <- coefDraws[,-grep("Intercept_pop", colnames(coefDraws))]
  
  muData <- predictiveVariables[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  nonInterceptPredictorContribution <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  # adding the intercepts
  interceptEstimateDraws <- postSample[,grep("Intercept_coef",colnames(postSample))]
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

save.image("modelFit3.RData");

############################################################################################################
# replicated price histograms compared to true price histogram   

set.seed(123); 

dataReplications <- getPosteriorPredictiveDraws(dataSet = estimationSet, 
                                                postSample = posteriorSamples.trueData, 
                                                likelihoodSigmaName = "sigma", 
                                                likelihoodNuName = "nu")

replicatedVector <- dataReplications[10,]
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
# median not in the graph... 

############################################################################################################
# average price per neighborhood

estimationSetNeighborhoods <- combinedData.orig$NeighborhoodFinalized[-testSetIndeces]

replicatedMeansPerNeighborhood <- apply(X = dataReplications, 
                                        MARGIN = 1, 
                                        FUN = function(rivi) tapply(X = rivi, INDEX = estimationSetNeighborhoods, mean))

estimationSetMeansPerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, mean)
sampleSizePerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, length)

neighborhoodNames <- names(estimationSetMeansPerNeighborhood); 

# for(k in 1:nrow(replicatedMeansPerNeighborhood)) {
#   replicatedMeans <- replicatedMeansPerNeighborhood[k,]
#   
#   replicatedMeans.cens <- replicatedMeans[replicatedMeans > quantile(replicatedMeans, probs = 0.01) & replicatedMeans < quantile(replicatedMeans, probs = 0.99)]
#   
#   plottingRanges <- range(replicatedMeans.cens)
#   
#   if(estimationSetMeansPerNeighborhood[k] < plottingRanges[1]) {
#     plottingRanges[1] <- estimationSetMeansPerNeighborhood[k];
#   } else if(estimationSetMeansPerNeighborhood[k] > plottingRanges[2]) {
#     plottingRanges[2] <- estimationSetMeansPerNeighborhood[k];
#   }
#   
#   hist(replicatedMeans.cens, 
#        main = paste(neighborhoodNames[k],", n. obs.: ", sampleSizePerNeighborhood[k], "\nsmallest and largest 1 % values removed", sep =""),
#        xlim = plottingRanges, 
#        xlab = "replicated mean")
#   abline(h=0)
#   abline(v = estimationSetMeansPerNeighborhood[k], col = 'red', lwd = 2, lty = 2)
#   
#   checkEnd <- readline(prompt = "q to end: ");
#   
#   if(checkEnd == 'q') {
#     break;
#   }
# }


# plotting 5 best and worst neighborhoods, difference measured by  
averageOfReplications <- apply(replicatedMeansPerNeighborhood, 1, mean)

worst5names <- names(sort(abs(averageOfReplications - estimationSetMeansPerNeighborhood), decreasing = T)[1:5])
best5names <- names(sort(abs(averageOfReplications - estimationSetMeansPerNeighborhood), decreasing = F)[1:5])

plotMeanHistogram <- function(name) {
  replicatedMeans <- replicatedMeansPerNeighborhood[name,]
  
  replicatedMeans.cens <- replicatedMeans[replicatedMeans > quantile(replicatedMeans, probs = 0.01) & replicatedMeans < quantile(replicatedMeans, probs = 0.99)]
  
  plottingRanges <- range(replicatedMeans.cens)
  
  if(estimationSetMeansPerNeighborhood[name] < plottingRanges[1]) {
    plottingRanges[1] <- estimationSetMeansPerNeighborhood[name];
  } else if(estimationSetMeansPerNeighborhood[name] > plottingRanges[2]) {
    plottingRanges[2] <- estimationSetMeansPerNeighborhood[name];
  }
  
  hist(replicatedMeans.cens, 
       main = paste(name,", n. obs.: ", sampleSizePerNeighborhood[name], "\nsmallest and largest 1 % values removed", sep =""),
       xlim = plottingRanges, 
       xlab = "replicated mean")
  abline(h=0)
  abline(v = estimationSetMeansPerNeighborhood[name], col = 'red', lwd = 2, lty = 2)
}

par(mfrow=c(2,5))
for(x in worst5names) { plotMeanHistogram(x); }
for(x in best5names) { plotMeanHistogram(x); }
par(mfrow=c(1,1))


############################################################################################################
# R-hats and effective samples sizes 

rHatNEfftable <- summary(stanFit.trueData)$summary
rHatNEfftable <- rHatNEfftable[-grep("log_lik", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("lp__", rownames(rHatNEfftable)),]

library(xtable)

xtable(rHatNEfftable)


############################################################################################################
# predictive distribution samples

# estimation set draws from predictive distribution
postPredDistDraws.estimation <- getPosteriorPredictiveDraws(dataSet = estimationSet,
                                                            postSample = posteriorSamples.trueData,
                                                            likelihoodSigmaName = "sigma",
                                                            likelihoodNuName = "nu")


# test set draws from predictive distribution
postPredDistDraws.test <- getPosteriorPredictiveDraws(dataSet = testSet,
                                                      postSample = posteriorSamples.trueData,
                                                      likelihoodSigmaName = "sigma",
                                                      likelihoodNuName = "nu")

############################################################################################################
# PIT histograms

PITsample.estimation <- sapply(1:nrow(estimationSet), function(k) mean(postPredDistDraws.estimation[,k] <= estimationSet$Price[k]))

scalingCoef <- 1.3;  
png('./figures/model3EstimationSetPIT.png', width = 600*scalingCoef, height = 400*scalingCoef)
hist(PITsample.estimation, 
     xlab = "Probability Integral Transform", 
     main = "PIT histogram, estimation set, model 1",
     probability = T)
dev.off(); 

# PITsample.test <- sapply(1:nrow(testSet), function(k) mean(postPredDistDraws.test[,k] <= testSet$Price[k]))
# hist(PITsample.test)

############################################################################################################
# graphing mean prices 

# estimation set means
predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

plot(estimationSet$Price, predDistMean.estimation)
abline(a = 0, b = 1, lty = 2, col = 'red')

# test set means 
predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

plot(testSet$Price, predDistMean.test)
abline(a = 0, b = 1, lty = 2, col = 'red')


#################################################################################
# Bayesian R^2


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



