
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
# trueValues <- c(Intercept_coef,
#                 Sqm_coef,
#                 CondGoodDummySqm_coef,
#                 Age_coef,
#                 TwoRoomsDummy_coef,
#                 ThreeRoomsDummy_coef,
#                 FourRoomsOrMoreDummy_coef,
#                 SaunaDummy_coef,
#                 OwnFloor_coef,
#                 sigma, 
#                 nu)
# names(trueValues) <- c("Intercept_coef",
#                        "Sqm_coef",
#                        "CondGoodDummySqm_coef",
#                        "Age_coef",
#                        "TwoRoomsDummy_coef",
#                        "ThreeRoomsDummy_coef",
#                        "FourRoomsOrMoreDummy_coef",
#                        "SaunaDummy_coef",
#                        "OwnFloor_coef", 
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
# 

# checking loo statistics
library(loo)
looObj.fakeData <- loo(stanFit.fakeData)
looObj.fakeData

###########
# fitting the model - true data

set.seed(9); 
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

# used for stacking later 
pointwiseElpdLooVectorForStacking <- looObj.trueData$pointwise[,"elpd_loo"]; 

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

library(LaplacesDemon) # for the dst-function

evaluatePosteriorPredictive <- function(Price.pointEvaluation,  
                                        predictiveVariables, 
                                        postSample, 
                                        likelihoodSigmaName, 
                                        likelihoodNuName) {
  
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  
  predictiveVariables$Intercept <- rep(1,nrow(predictiveVariables)); 
  
  muData <- predictiveVariables[,substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)]; 
  
  muEstimateDraws <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  lpd <- dst(x = Price.pointEvaluation, mu = muEstimateDraws, sigma = sigmaEstimateDraws, nu = nuEstimateDraws); 
  
  return(mean(lpd));
}

############################################################################################################
# storing posterior draws, loo object, function for drawing from posterior predictice distribution for further use   

save.image("modelFit1.RData");

############################################################################################################
# replicated price histograms compared to true price histogram   

set.seed(123); 

dataReplications <- getPosteriorPredictiveDraws(dataSet = estimationSet, 
                                                postSample = posteriorSamples.trueData, 
                                                likelihoodSigmaName = "sigma", 
                                                likelihoodNuName = "nu")



# distribution of mean, median of replicated prices

scalingCoef <- 0.9;  
dir.create('./figures')

png('./figures/model1replicatedMeans.png', width = 600*scalingCoef, height = 400*scalingCoef)
replicatedPrices.mean <- apply(dataReplications, 1, mean);
range.mean <- range(replicatedPrices.mean)
hist(replicatedPrices.mean, 
     xlim = c(range.mean[1]*0.99, range.mean[2]*1.01),
     xlab = "Mean", 
     main = "Replicated means")
abline(v = mean(estimationSet$Price), col = 'red', lwd = 1, lty = 2)
abline(h = 0)
dev.off()

png('./figures/model1replicatedMedians.png', width = 600*scalingCoef, height = 400*scalingCoef)
replicatedPrices.median <- apply(dataReplications, 1, median);
range.median <- range(replicatedPrices.median)
hist(replicatedPrices.median,
     xlim = c(range.median[1]*0.98, range.median[2]*1.02),
     xlab = "Median",
     main = "Replicated medians")
abline(v = median(estimationSet$Price), col = 'red', lwd = 1, lty = 2)
abline(h = 0)
dev.off()

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

worst3names <- names(sort(abs(averageOfReplications - estimationSetMeansPerNeighborhood), decreasing = T)[1:3])
best3names <- names(sort(abs(averageOfReplications - estimationSetMeansPerNeighborhood), decreasing = F)[1:3])

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
       main = paste(name,", n. obs.: ", sampleSizePerNeighborhood[name], "\nleft-most, right-most 1 % values truncated", sep =""),
       xlim = plottingRanges, 
       xlab = "replicated mean")
  abline(h=0)
  abline(v = estimationSetMeansPerNeighborhood[name], col = 'red', lwd = 2, lty = 2)
}


scalingCoef <- 1.3;  

png('./figures/model1neighborhoodMeans.png', width = 600*scalingCoef, height = 400*scalingCoef)

par(mfrow=c(2,3))
for(x in worst3names) { plotMeanHistogram(x); }
for(x in best3names) { plotMeanHistogram(x); }
par(mfrow=c(1,1))

dev.off()


############################################################################################################
# R-hats and effective samples sizes 

rHatNEfftable <- summary(stanFit.trueData)$summary
rHatNEfftable <- rHatNEfftable[-grep("log_lik", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("lp__", rownames(rHatNEfftable)),]

library(xtable)

# se_mean = sd/sqrt(n_eff)

rHatNEfftable <- rHatNEfftable[,c("mean", "sd", "2.5%", "50%", "97.5%", "n_eff", "Rhat")] 

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
png('./figures/model1EstimationSetPIT.png', width = 600*scalingCoef, height = 400*scalingCoef)
hist(PITsample.estimation, 
     xlab = "Probability Integral Transform", 
     main = "PIT histogram, estimation set, model 1",
     probability = T)


# PITsample.test <- sapply(1:nrow(testSet), function(k) mean(postPredDistDraws.test[,k] <= testSet$Price[k]))
# hist(PITsample.test)

############################################################################################################
# sharpness box plots following Gneiting, Balabdaoui, Raftery 2007

# sharpness measured by the width of credible intervals with (5 %, 95 %)-cut 

credibleIntervalWidths.90.estimation <- apply(X = postPredDistDraws.estimation, 
                                              MARGIN = 2, 
                                              function(otos) quantile(x = otos, probs =  0.95) - quantile(x = otos, probs =  0.05));

credibleIntervalWidths.50.estimation <- apply(X = postPredDistDraws.estimation, 
                                              MARGIN = 2, 
                                              function(otos) quantile(x = otos, probs =  0.75) - quantile(x = otos, probs =  0.25));

scalingCoef <- 1.3; 
png('./figures/model1EstimationSet90CredIntSharpnessBoxplot.png', width = 600*scalingCoef, height = 400*scalingCoef);  
boxplot(credibleIntervalWidths.90.estimation, outline=F)
dev.off();

png('./figures/model1EstimationSet50CredIntSharpnessBoxplot.png', width = 600*scalingCoef, height = 400*scalingCoef); 
boxplot(credibleIntervalWidths.50.estimation, outline=F)
dev.off(); 

############################################################################################################
# graphing means  

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)


scalingCoef <- 1.3; 
png('./figures/model1EstimationSetMeanPredScatter.png', width = 600*scalingCoef, height = 400*scalingCoef);  
plot(estimationSet$Price, 
     predDistMean.estimation,
     xlab = "true price", 
     ylab = "mean of price predictive distribution",
     main = "True price vs. mean of predictive distributions\nestimation set")
abline(a = 0, b = 1, lty = 2, col = 'red')
dev.off();


# largDifIndeces.estimation <- order(abs(estimationSet$Price - predDistMean.estimation), decreasing = T);
# k <- 5; 
# targetIndex <- largDifIndeces.estimation[k];
# hist(postPredDistDraws.estimation[,targetIndex], nclass = 50)
# abline(v = estimationSet$Price[targetIndex], col = 'red', lty = 2);

predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

scalingCoef <- 1.3; 
png('./figures/model1TestSetMeanPredScatter.png', width = 600*scalingCoef, height = 400*scalingCoef);  
plot(testSet$Price, 
     predDistMean.test,
     xlab = "true price", 
     ylab = "mean of price predictive distribution",
     main = "True price vs. mean of predictive distributions\ntest set")
abline(a = 0, b = 1, lty = 2, col = 'red')
dev.off();

# largDifIndeces.test <- order(abs(testSet$Price - predDistMean.test), decreasing = T);
# k <- 2; 
# targetIndex <- largDifIndeces.test[k];
# hist(postPredDistDraws.test[,targetIndex], nclass = 50)
# abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);

#########################################################################################
# Bayesian R^2(?)
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





