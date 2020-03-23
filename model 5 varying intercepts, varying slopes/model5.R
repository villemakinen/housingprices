
# simple varying intercept, varying slopes model with t-distribution likelihood

library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/')

rm(list=ls()); gc(); 
# load(file = 'modelFit5.RData')
# looObj.trueData; plot(looObj.trueData); # EV p_loo ~333.2

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
library(rethinking); # see https://github.com/rmcelreath/statrethinking_winter2019, library needed for rlkjcorr
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


# check 2.10.2019 - compare with https://github.com/jgabry/bayes-vis-paper/blob/master/bayes-vis.R
# EV.fakeData <- groupCoeffs[estimationSet$NeighborhoodAssignment,1] + 
#   groupCoeffs[estimationSet$NeighborhoodAssignment,2]*estimationSet$Sqm +
#   groupCoeffs[estimationSet$NeighborhoodAssignment,3]*estimationSet$CondGoodDummySqm + 
#   Age_coef*estimationSet$Age + 
#   SaunaDummy_coef*estimationSet$SaunaDummy + 
#   OwnFloor_coef*estimationSet$OwnFloor + 
#   TwoRoomsDummy_coef*estimationSet$TwoRoomsDummy + 
#   ThreeRoomsDummy_coef*estimationSet$ThreeRoomsDummy +  
#   FourRoomsOrMoreDummy_coef*estimationSet$FourRoomsOrMoreDummy
# 
# Price.fakeData <- EV.fakeData + sigma*rt(n = length(EV.fakeData), df = nu)

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

# trueValues <- c(EV_betaSquareMetersGoodCond, 
#                 EV_betaSquareMeters, 
#                 EV_betaIntercept, 
#                 sigma_Neighborhood_SqmGoodCond, 
#                 sigma_Neighborhood_Sqm, 
#                 sigma_Neighborhood_Intercept, 
#                 Age_coef,    
#                 TwoRoomsDummy_coef,  
#                 ThreeRoomsDummy_coef,   
#                 FourRoomsOrMoreDummy_coef,   
#                 OwnFloor_coef,    
#                 SaunaDummy_coef, 
#                 sigma, 
#                 nu,
#                 RhoMatrix[1,1],
#                 RhoMatrix[2,1],
#                 RhoMatrix[3,1],
#                 RhoMatrix[1,2],
#                 RhoMatrix[2,2],
#                 RhoMatrix[3,2],
#                 RhoMatrix[1,3],
#                 RhoMatrix[2,3],
#                 RhoMatrix[3,3])
# names(trueValues) <- c("Mu_CondGoodSqm_coef", 
#                        "Mu_Sqm_coef",
#                        "Mu_Intercept_coef",
#                        "Sigma_CondGoodSqm_coef",
#                        "Sigma_Sqm_coef",
#                        "Sigma_Intercept_coef",
#                        "Age_coef",
#                        "TwoRoomsDummy_coef",
#                        "ThreeRoomsDummy_coef",
#                        "FourRoomsOrMoreDummy_coef",
#                        "OwnFloor_coef",
#                        "SaunaDummy_coef",
#                        "sigma",
#                        "nu",
#                        "Rho[1,1]",
#                        "Rho[2,1]",                 
#                        "Rho[3,1]",
#                        "Rho[1,2]",
#                        "Rho[2,2]",
#                        "Rho[3,2]",
#                        "Rho[1,3]",                 
#                        "Rho[2,3]",
#                        "Rho[3,3]")
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

############################
# estimating the model with true data

set.seed(9); 
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
# stacking weights are constructed in a separate exactLOO.R-script because there are nine observations with k > 0.7 

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


library(LaplacesDemon) # for the dst-function

evaluatePosteriorPredictive <- function(Price.pointEvaluation,  
                                        predictiveVariables, 
                                        postSample, 
                                        likelihoodSigmaName, 
                                        likelihoodNuName) {
  
  coefDraws <- postSample[,grep("_coef",colnames(postSample))]
  
  coefDraws <- coefDraws[,-grep("Mu", colnames(coefDraws))]
  coefDraws <- coefDraws[,-grep("Sigma", colnames(coefDraws))]
  
  muData <- predictiveVariables[substr(colnames(coefDraws), 1, nchar(colnames(coefDraws))-5)];
  
  nonGroupVaryingPredictorContribution <- as.matrix(muData) %*% t(as.matrix(coefDraws))
  
  # adding the effects of varying intercep  
  interceptEstimateDraws <- postSample[,grep("VaryingSlopes\\[\\d+,1\\]", colnames(postSample))]
  interceptContribution <- t(interceptEstimateDraws[,predictiveVariables$NeighborhoodAssignment])
  
  # adding the effects of Sqm
  sqmCoefEstimateDraws <- postSample[,grep("VaryingSlopes\\[\\d+,2\\]", colnames(postSample))]
  sqmData <- as.numeric(predictiveVariables["Sqm"]); 
  
  sqmContribution <- sqmData*t(sqmCoefEstimateDraws[,predictiveVariables$NeighborhoodAssignment]);
  
  # adding the effects of CondGoodSqm
  condGoodSqmCoefEstimateDraws <- postSample[,grep("VaryingSlopes\\[\\d+,3\\]", colnames(postSample))]
  condGoodSqmData <- as.numeric(predictiveVariables["CondGoodDummySqm"]); 
  
  condGoodSqmContribution <-  condGoodSqmData*t(condGoodSqmCoefEstimateDraws[,predictiveVariables$NeighborhoodAssignment]);
  
  muEstimateDraws <- nonGroupVaryingPredictorContribution + interceptContribution + sqmContribution + condGoodSqmContribution; 
  
  sigmaEstimateDraws <- postSample[,likelihoodSigmaName];
  nuEstimateDraws <- postSample[,likelihoodNuName];
  
  lpd <- dst(x = Price.pointEvaluation, mu = muEstimateDraws, sigma = sigmaEstimateDraws, nu = nuEstimateDraws); 
  
  return(mean(lpd)); 
}


############################################################################################################
# storing posterior draws, loo object, function for drawing from posterior predictice distribution for further use   

save.image("modelFit5.RData");

############################################################################################################
# replicated price histograms compared to true price histogram   

set.seed(123); 

dataReplications <- getPosteriorPredictiveDraws(dataSet = estimationSet, 
                                                postSample = posteriorSamples.trueData, 
                                                likelihoodSigmaName = "sigma", 
                                                likelihoodNuName = "nu")

# distribution of mean, median of replicated prices

scalingCoef <- 1.5;  
dir.create('./figures')

cexAxisConstant <- 1.5; 
cexLabConstant <- 1.5;
cexMainConstant <- 1.5; 
standardWidth <- 600;
standardHeight <- 150; 

png('./figures/model5replicatedMeans.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef)
replicatedPrices.mean <- apply(dataReplications, 1, mean);
range.mean <- range(replicatedPrices.mean)
par(mar = c(5,10,4,2) + 0.1)
hist(replicatedPrices.mean, 
     xlim = c(range.mean[1]*0.99, range.mean[2]*1.01),
     main = "Replicated means",
     xlab = "",
     ylab = "",
     cex.axis = cexAxisConstant, 
     cex.lab = cexLabConstant,
     cex.main = cexMainConstant, 
     probability = T,
     axes = F)
axis(side = 1, 
     at = seq(from = 255000, to = 290000, by = 5000), 
     cex.axis = cexAxisConstant)
axis(side = 2, 
     at = seq(from = 0, to = 0.00022, length.out = 3), 
     cex.axis = cexAxisConstant,
     las = 2)
title(ylab = "Density", cex.lab = cexLabConstant, line = 7)
title(xlab = "Mean", cex.lab = cexLabConstant)
abline(v = mean(estimationSet$Price), col = 'red', lwd = 1, lty = 2)
# abline(h = 0)
par(mar = c(5,4,4,2) + 0.1)
dev.off()

png('./figures/model5replicatedMedians.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef)
replicatedPrices.median <- apply(dataReplications, 1, median);
range.median <- range(replicatedPrices.median)
par(mar = c(5,10,4,2) + 0.1)
hist(replicatedPrices.median,
     xlim = c(range.median[1]*0.98, range.median[2]*1.02),
     main = "Replicated medians",
     xlab = "",
     ylab = "",
     cex.axis = cexAxisConstant, 
     cex.lab = cexLabConstant,
     cex.main = cexMainConstant, 
     probability = T,
     axes = F
     )
axis(side = 1, 
     at = seq(from = 235000, to = 250000, by = 5000), 
     cex.axis = cexAxisConstant)
axis(side = 2, 
     at = seq(from = 0, to = 0.00024, length.out = 3), 
     cex.axis = cexAxisConstant,
     las = 2)
title(ylab = "Density", cex.lab = cexLabConstant, line = 7)
title(xlab = "Mean", cex.lab = cexLabConstant)
abline(v = median(estimationSet$Price), col = 'red', lwd = 1, lty = 2)
# abline(h = 0)
dev.off()
par(mar = c(5,4,4,2) + 0.1)

############################################################################################################
# average price per neighborhood

estimationSetNeighborhoods <- combinedData.orig$NeighborhoodFinalized[-testSetIndeces]

replicatedMeansPerNeighborhood <- apply(X = dataReplications, 
                                        MARGIN = 1, 
                                        FUN = function(rivi) tapply(X = rivi, INDEX = estimationSetNeighborhoods, mean))

estimationSetMeansPerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, mean)
sampleSizePerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, length)

neighborhoodNames <- names(estimationSetMeansPerNeighborhood); 

library(hash)
neighborhoodToCityHash <- hash(keys = c('Alppiharju','Askisto','Asola','Eira','Espoon keskus','Espoonlahti','Etu-Töölö','Haaga','Hakunila','Hämeenkylä','Hämevaara','Haukilahti','Havukoski','Henttaa','Hermanni','Herttoniemi','Hiekkaharju','Ilola','Itä-Hakkila','Järvenperä','Jokiniemi','Kaarela','Kaartinkaupunki','Kaitaa','Kaivoksela','Kallio','Kamppi','Käpylä','Karakallio','Karhusuo','Karvasmäki','Katajanokka','Kauklahti','Kaupunginkallio','Keimola','Kilo','Kivistö','Kluuvi','Koivuhaka','Koivukylä','Kolmperä','Konala','Korso','Koskela','Kruununhaka','Kulosaari','Kumpula','Kuninkaala','Kuninkaanmäki','Kuurinniitty','Laajalahti','Laajasalo','Laakso','Laaksolahti','Lahnus','Länsimäki','Länsisatama','Latokaski','Lauttasaari','Leppäkorpi','Leppävaara','Lintuvaara','Lippajärvi','Malmi','Mankkaa','Martinlaakso','Matari','Matinkylä','Meilahti','Mellunkylä','Metsola','Mikkola','Munkkiniemi','Muurala','Myyrmäki','Niipperi','Niittykumpu','Nikinmäki','Nöykkiö','Nupuri','Olari','Otaniemi','Oulunkylä','Päiväkumpu','Pakila','Pakkala','Pasila','Perusmäki','Piispankylä','Pitäjänmäki','Pohjois-Tapiola','Pukinmäki','Punavuori','Rajakylä','Rekola','Ruskeasanta','Ruskeasuo','Saunalahti','Sepänkylä','Simonkylä','Sörnäinen','Soukka','Suurmetsä','Suutarila','Taka-Töölö','Tammisalo','Tammisto','Tapaninkylä','Tapiola','Tikkurila','Toukola','Tuomarinkylä','Ullanlinna','Vaarala','Vallila','Vanhakaupunki','Vantaanlaakso','Vanttila','Vapaala','Varisto','Vartiokylä','Viertola','Vierumäki','Viherlaakso','Viikki','Vuosaari','Westend','Ylästö'),
                               values = c('Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Helsinki','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Espoo','Espoo','Espoo','Helsinki','Espoo','Espoo','Vantaa','Espoo','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Vantaa','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Vantaa','Espoo','Espoo','Helsinki','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Vantaa','Espoo','Espoo','Espoo','Helsinki','Espoo','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Vantaa','Espoo','Espoo','Vantaa','Espoo','Espoo','Espoo','Espoo','Helsinki','Vantaa','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Helsinki','Vantaa','Espoo','Vantaa','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Espoo','Vantaa')) 

cityAssignments <- sapply(rownames(replicatedMeansPerNeighborhood), function(x) neighborhoodToCityHash[[x]])

replicatedMeansPerNeighborhood.Helsinki <- replicatedMeansPerNeighborhood[cityAssignments == "Helsinki",]
replicatedMeansPerNeighborhood.Espoo <- replicatedMeansPerNeighborhood[cityAssignments == "Espoo",]
replicatedMeansPerNeighborhood.Vantaa <- replicatedMeansPerNeighborhood[cityAssignments == "Vantaa",]

observerdMeans.Helsinki <- estimationSetMeansPerNeighborhood[names(estimationSetMeansPerNeighborhood) %in% rownames(replicatedMeansPerNeighborhood.Helsinki)]
observerdMeans.Espoo <- estimationSetMeansPerNeighborhood[names(estimationSetMeansPerNeighborhood) %in% rownames(replicatedMeansPerNeighborhood.Espoo)]
observerdMeans.Vantaa <- estimationSetMeansPerNeighborhood[names(estimationSetMeansPerNeighborhood) %in% rownames(replicatedMeansPerNeighborhood.Vantaa)]

png('./figures/model5replicatedMeansHelsinki.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Helsinki), outline=F, ylim = c(100000, 750000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nHelsinki")
points(observerdMeans.Helsinki, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), labels = rownames(replicatedMeansPerNeighborhood.Helsinki), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 100000, to = 750000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()


png('./figures/model5replicatedMeansEspoo.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Espoo), outline=F, ylim = c(50000, 650000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nEspoo")
points(observerdMeans.Espoo, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Espoo), labels = rownames(replicatedMeansPerNeighborhood.Espoo), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Espoo), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 50000, to = 650000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()


png('./figures/model5replicatedMeansVantaa.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Vantaa), outline=F, ylim = c(0, 600000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nVantaa")
points(observerdMeans.Vantaa, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), labels = rownames(replicatedMeansPerNeighborhood.Vantaa), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 0, to = 600000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

############################################################################################################
# R-hats and effective sample sizes 
library(rstan)

rHatNEfftable <- summary(stanFit.trueData)$summary
rHatNEfftable <- rHatNEfftable[-grep("log_lik", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("lp__", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("offset", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("choleskyFactor", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("Mu_VaryingSlopes", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("sigma_Neighborhood", rownames(rHatNEfftable)),]

# se_mean = sd/sqrt(n_eff)
rHatNEfftable <- rHatNEfftable[,c("mean", "sd", "2.5%", "50%", "97.5%", "n_eff", "Rhat")] 

parameterTable <- rHatNEfftable[-grep("VaryingSlopes", rownames(rHatNEfftable)),];

allSlopesTable <- rHatNEfftable[grep("VaryingSlopes", rownames(rHatNEfftable)),]; 
interceptCoefTable <- allSlopesTable[grep(",1\\]", rownames(allSlopesTable)),] 
sqmCoefTable <- allSlopesTable[grep(",2\\]", rownames(allSlopesTable)),]
goodCondSqmCoefTable <- allSlopesTable[grep(",3\\]", rownames(allSlopesTable)),]

neighborhoodIndexHash <- data.frame(neighborhoodName = as.character(combinedData.orig$NeighborhoodFinalized), 
                                    neighborhoodIndex =as.numeric(combinedData.orig$NeighborhoodFinalized), 
                                    stringsAsFactors = F)

neighborhoodIndexHash <- unique(neighborhoodIndexHash)
neighborhoodIndexHash <- neighborhoodIndexHash[order(neighborhoodIndexHash$neighborhoodIndex),]

neighborhoodIndexHash.hash <- hash(keys = neighborhoodIndexHash$neighborhoodIndex, 
                                   values = neighborhoodIndexHash$neighborhoodName)

indeces <- rownames(interceptCoefTable)
chopNfirst <- function(string, N) substring(text = string, first = N+1, last = nchar(string)) 
chopLast <- function(string) substring(text = string, first = 1, last = nchar(string)-1) 
indeces <- sapply(X = indeces, FUN = chopNfirst, N = 14)
indeces <- sapply(X = indeces, FUN = chopLast)
indeces <- sapply(X = indeces, FUN = chopLast)
indeces <- sapply(X = indeces, FUN = chopLast)
namesFromIndeces <- sapply(X = indeces, FUN = function(x) neighborhoodIndexHash.hash[[x]] )

interceptCoefTable <- cbind(data.frame(namesFromIndeces), interceptCoefTable)
sqmCoefTable <- cbind(data.frame(namesFromIndeces), sqmCoefTable)
goodCondSqmCoefTable <- cbind(data.frame(namesFromIndeces), goodCondSqmCoefTable)

rownames(interceptCoefTable) <- NULL;
rownames(sqmCoefTable) <- NULL;
rownames(goodCondSqmCoefTable) <- NULL;

library(xtable)
xtable(parameterTable)

xtable(interceptCoefTable)
xtable(sqmCoefTable)
xtable(goodCondSqmCoefTable)

############################################################################################################
# predictive distribution samples

set.seed(123); 

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

png('./figures/model5EstimationSetPIT.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef)
hist(PITsample.estimation, 
     xlab = "Probability Integral Transform", 
     main = "PIT histogram, estimation set, model 1",
     probability = T)
dev.off()

# PITsample.test <- sapply(1:nrow(testSet), function(k) mean(postPredDistDraws.test[,k] <= testSet$Price[k]))
# hist(PITsample.test)

############################################################################################################
# sharpness box plots following Gneiting, Balabdaoui, Raftery 2007

# sharpness measured by the width of credible intervals with (5 %, 95 %)-cut 

credibleIntervalWidths.90.estimation <- apply(X = postPredDistDraws.estimation, 
                                              MARGIN = 2, 
                                              function(otos) quantile(x = otos, probs =  0.95) - quantile(x = otos, probs =  0.05));

# credibleIntervalWidths.50.estimation <- apply(X = postPredDistDraws.estimation, 
#                                               MARGIN = 2, 
#                                               function(otos) quantile(x = otos, probs =  0.75) - quantile(x = otos, probs =  0.25));
# 
# scalingCoef <- 1.3; 
# png('./figures/model1EstimationSet90CredIntSharpnessBoxplot.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef);  
# boxplot(credibleIntervalWidths.90.estimation, outline=F)
# dev.off();

png('./figures/model5EstimationSet90CredIntSharpnessHistogram.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef);  
hist(credibleIntervalWidths.90.estimation, 
     xlab = "width", 
     main = "Sharpness histogram, 90 % credible interval width, model 1",
     nclass = 30)
abline(h=0)
dev.off();
mean(credibleIntervalWidths.90.estimation)


# png('./figures/model1EstimationSet50CredIntSharpnessBoxplot.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef); 
# boxplot(credibleIntervalWidths.50.estimation, outline=F)
# dev.off(); 

############################################################################################################
# graphing means  

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

png('./figures/model5EstimationSetMeanPredScatter.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef);  
plot(estimationSet$Price, 
     predDistMean.estimation,
     xlab = "true price", 
     ylab = "mean of price predictive distribution",
     main = "True price vs. mean of predictive distributions\nestimation set")
abline(a = 0, b = 1, lty = 2, col = 'red')
dev.off();

1- sum((estimationSet$Price -predDistMean.estimation)^2)/sum((estimationSet$Price-mean(estimationSet$Price))^2) 


# largDifIndeces.estimation <- order(abs(estimationSet$Price - predDistMean.estimation), decreasing = T);
# k <- 5; 
# targetIndex <- largDifIndeces.estimation[k];
# hist(postPredDistDraws.estimation[,targetIndex], nclass = 50)
# abline(v = estimationSet$Price[targetIndex], col = 'red', lty = 2);

predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

png('./figures/model5TestSetMeanPredScatter.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef);  
problematicObservations <- c(259, 1290, 225, 118, 518, 443, 708, 200, 63)
cat(problematicObservations) # to be used for labeling for other models 
# 259 1290 225 118 518 443 708 200 63
plot(testSet$Price, 
     predDistMean.test,
     xlab = "true price", 
     ylab = "mean of price predictive distribution",
     main = "True price vs. mean of predictive distributions\ntest set")
abline(a = 0, b = 1, lty = 2, col = 'red')
with(testSet[problematicObservations,], text(testSet$Price[problematicObservations], 
                                             predDistMean.test[problematicObservations],
                                             labels = rownames(testSet)[problematicObservations],
                                             pos = 1, 
                                             cex = 0.8))
dev.off();

1 - sum((testSet$Price - predDistMean.test)^2)/sum((testSet$Price-mean(testSet$Price))^2) 

#########################################################################################
# difficult test set observations, predictive distributions histograms 

testSetDifficultObsIndeces <- c(259, 1290, 225, 118, 518, 443, 708, 200, 63)

png('./figures/model5TestSetProblemObservations.png', width = standardWidth*scalingCoef, height = standardHeight*scalingCoef);  
par(mfrow=c(3,3)); 
for(testSetIndex in testSetDifficultObsIndeces) {
  valueVector <- postPredDistDraws.test[,testSetIndex];
  
  # removing 0.5 % extreme values from both sides
  valueVector <- valueVector[valueVector <= quantile(valueVector, probs = 1 - 0.005) & valueVector >= quantile(valueVector, probs = 0.005)]
  
  truePrice <- testSet$Price[testSetIndex]
  
  xlim.histogram <- range(valueVector)
  if(xlim.histogram[1] > truePrice) {
    xlim.histogram[1] <- truePrice
  } else if(xlim.histogram[2] < truePrice) {
    xlim.histogram[2] <- truePrice;
  }
  
  xlim.histogram[1] <- xlim.histogram[1] - 1000; 
  xlim.histogram[2] <- xlim.histogram[2] + 1000; 
  
  hist(valueVector, 
       xlim = xlim.histogram,
       main = paste("observation", rownames(testSet)[testSetIndex]),
       probability = T,
       xlab = "Price");
  abline(v = truePrice, lty = 2, col = 'red')
  abline(h=0);
}
par(mfrow=c(1,1)); 
dev.off();

#########################################################################################################
# 13.11.2019 - plotting for the neighborhood variant 

# names and cities for indeces
library(hash)

neighborhoodIndexHash <- data.frame(neighborhoodName = as.character(combinedData.orig$NeighborhoodFinalized), 
                                    neighborhoodIndex =as.numeric(combinedData.orig$NeighborhoodFinalized), 
                                    stringsAsFactors = F)

neighborhoodIndexHash <- unique(neighborhoodIndexHash)
neighborhoodIndexHash <- neighborhoodIndexHash[order(neighborhoodIndexHash$neighborhoodIndex),]

neighborhoodIndexHash.hash <- hash(keys = neighborhoodIndexHash$neighborhoodIndex, 
                                   values = neighborhoodIndexHash$neighborhoodName)

namesFromIndeces <- sapply(1:128, function(x) neighborhoodIndexHash.hash[[as.character(x)]])

neighborhoodToCityHash <- hash(keys = c('Alppiharju','Askisto','Asola','Eira','Espoon keskus','Espoonlahti','Etu-Töölö','Haaga','Hakunila','Hämeenkylä','Hämevaara','Haukilahti','Havukoski','Henttaa','Hermanni','Herttoniemi','Hiekkaharju','Ilola','Itä-Hakkila','Järvenperä','Jokiniemi','Kaarela','Kaartinkaupunki','Kaitaa','Kaivoksela','Kallio','Kamppi','Käpylä','Karakallio','Karhusuo','Karvasmäki','Katajanokka','Kauklahti','Kaupunginkallio','Keimola','Kilo','Kivistö','Kluuvi','Koivuhaka','Koivukylä','Kolmperä','Konala','Korso','Koskela','Kruununhaka','Kulosaari','Kumpula','Kuninkaala','Kuninkaanmäki','Kuurinniitty','Laajalahti','Laajasalo','Laakso','Laaksolahti','Lahnus','Länsimäki','Länsisatama','Latokaski','Lauttasaari','Leppäkorpi','Leppävaara','Lintuvaara','Lippajärvi','Malmi','Mankkaa','Martinlaakso','Matari','Matinkylä','Meilahti','Mellunkylä','Metsola','Mikkola','Munkkiniemi','Muurala','Myyrmäki','Niipperi','Niittykumpu','Nikinmäki','Nöykkiö','Nupuri','Olari','Otaniemi','Oulunkylä','Päiväkumpu','Pakila','Pakkala','Pasila','Perusmäki','Piispankylä','Pitäjänmäki','Pohjois-Tapiola','Pukinmäki','Punavuori','Rajakylä','Rekola','Ruskeasanta','Ruskeasuo','Saunalahti','Sepänkylä','Simonkylä','Sörnäinen','Soukka','Suurmetsä','Suutarila','Taka-Töölö','Tammisalo','Tammisto','Tapaninkylä','Tapiola','Tikkurila','Toukola','Tuomarinkylä','Ullanlinna','Vaarala','Vallila','Vanhakaupunki','Vantaanlaakso','Vanttila','Vapaala','Varisto','Vartiokylä','Viertola','Vierumäki','Viherlaakso','Viikki','Vuosaari','Westend','Ylästö'),
                               values = c('Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Helsinki','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Espoo','Espoo','Espoo','Helsinki','Espoo','Espoo','Vantaa','Espoo','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Vantaa','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Vantaa','Espoo','Espoo','Helsinki','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Vantaa','Espoo','Espoo','Espoo','Helsinki','Espoo','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Vantaa','Espoo','Espoo','Vantaa','Espoo','Espoo','Espoo','Espoo','Helsinki','Vantaa','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Helsinki','Vantaa','Espoo','Vantaa','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Espoo','Vantaa')) 

cityAssignments <- sapply(namesFromIndeces, function(x) neighborhoodToCityHash[[x]])

groupEffectsSample <- posteriorSamples.trueData[,grep("VaryingSlopes", colnames(posteriorSamples.trueData))]; 
groupEffectsSample <- groupEffectsSample[, -grep("Mu", colnames(groupEffectsSample))]


# intercepts - need to split city by city 
interceptRange <- quantile(x = groupEffectsSample[,(1:128)], probs = c(0.00001, 0.99999))

png('./figures/model5interceptsBoxplots.png', width = 1600, height = 1200)
par(mfrow=c(1,3))
par(mar = c(5,10.5,1,2)+0.1)

meanOrderIntercept.Helsinki <- order(interceptCoefTable[(1:128)[cityAssignments == "Helsinki"],1])
boxPlotData.Helsinki <- groupEffectsSample[,(1:128)[cityAssignments == "Helsinki"]] 

boxplot(boxPlotData.Helsinki[,meanOrderIntercept.Helsinki], 
        horizontal = T, 
        outline=F, 
        ylim = interceptRange,
        axes=F)
axis(side = 1, 
     round(seq(from = interceptRange[1], to = interceptRange[2], length.out = 5),-3),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Helsinki"]), 
     labels = (namesFromIndeces[cityAssignments == "Helsinki"])[meanOrderIntercept.Helsinki],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Helsinki"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Helsinki", 
      line = -2,
      cex.main = 1.5)


meanOrderIntercept.Espoo <- order(interceptCoefTable[(1:128)[cityAssignments == "Espoo"],1])
boxPlotData.Espoo <- groupEffectsSample[,(1:128)[cityAssignments == "Espoo"]] 

boxplot(boxPlotData.Espoo[,meanOrderIntercept.Espoo], 
        horizontal = T, 
        outline=F, 
        ylim = interceptRange,
        axes=F)
axis(side = 1, 
     round(seq(from = interceptRange[1], to = interceptRange[2], length.out = 5),-3),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Espoo"]), 
     labels = (namesFromIndeces[cityAssignments == "Espoo"])[meanOrderIntercept.Espoo],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Espoo"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Espoo", 
      line = -2,
      cex.main = 1.5)

meanOrderIntercept.Vantaa <- order(interceptCoefTable[(1:128)[cityAssignments == "Vantaa"],1])
boxPlotData.Vantaa <- groupEffectsSample[,(1:128)[cityAssignments == "Vantaa"]] 

boxplot(boxPlotData.Vantaa[,meanOrderIntercept.Vantaa], 
        horizontal = T, 
        outline=F, 
        ylim = interceptRange,
        axes=F)
axis(side = 1, 
     round(seq(from = interceptRange[1], to = interceptRange[2], length.out = 5),-3),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Vantaa"]), 
     labels = (namesFromIndeces[cityAssignments == "Vantaa"])[meanOrderIntercept.Vantaa],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Vantaa"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Vantaa", 
      line = -2,
      cex.main = 1.5)

par(mar = c(5,4,4,4)+0.1)
par(mfrow=c(1,1))
dev.off()

# sqm-coefs
sqmRange <- quantile(x = groupEffectsSample[,((128+1):(128+128))], probs = c(0.00001, 0.99999)) 

png('./figures/model5SqmBoxplots.png', width = 1600, height = 1200)
par(mfrow=c(1,3))
par(mar = c(5,10.5,1,2)+0.1)

meanOrderSqm.Helsinki <- order(sqmCoefTable[(1:128)[cityAssignments == "Helsinki"],1])
boxPlotData.Helsinki <- groupEffectsSample[,((128+1):(128+128))[cityAssignments == "Helsinki"]] 

boxplot(boxPlotData.Helsinki[,meanOrderSqm.Helsinki], 
        horizontal = T, 
        outline=F, 
        ylim = sqmRange,
        axes=F)
axis(side = 1, 
     round(seq(from = sqmRange[1], to = sqmRange[2], length.out = 5),-3),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Helsinki"]), 
     labels = (namesFromIndeces[cityAssignments == "Helsinki"])[meanOrderSqm.Helsinki],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Helsinki"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Helsinki", 
      line = -2,
      cex.main = 1.5)


meanOrderSqm.Espoo <- order(sqmCoefTable[(1:128)[cityAssignments == "Espoo"],1])
boxPlotData.Espoo <- groupEffectsSample[,((128+1):(128+128))[cityAssignments == "Espoo"]] 

boxplot(boxPlotData.Espoo[,meanOrderSqm.Espoo], 
        horizontal = T, 
        outline=F, 
        ylim = sqmRange,
        axes=F)
axis(side = 1, 
     round(seq(from = sqmRange[1], to = sqmRange[2], length.out = 5),-3),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Espoo"]), 
     labels = (namesFromIndeces[cityAssignments == "Espoo"])[meanOrderSqm.Espoo],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Espoo"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Espoo", 
      line = -2,
      cex.main = 1.5)

meanOrderSqm.Vantaa <- order(sqmCoefTable[(1:128)[cityAssignments == "Vantaa"],1])
boxPlotData.Vantaa <- groupEffectsSample[,((128+1):(128+128))[cityAssignments == "Vantaa"]] 

boxplot(boxPlotData.Vantaa[,meanOrderSqm.Vantaa], 
        horizontal = T, 
        outline=F, 
        ylim = sqmRange,
        axes=F)
axis(side = 1, 
     round(seq(from = sqmRange[1], to = sqmRange[2], length.out = 5),-3),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Vantaa"]), 
     labels = (namesFromIndeces[cityAssignments == "Vantaa"])[meanOrderSqm.Vantaa],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Vantaa"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Vantaa", 
      line = -2,
      cex.main = 1.5)

par(mar = c(5,4,4,4)+0.1)
par(mfrow=c(1,1))
dev.off()


# goodcondsqm-coefs
goodCondSqmRange <- quantile(x = groupEffectsSample[,((128+128+1):(128+128+128))], probs = c(0.0001, 0.9999)) 

png('./figures/model5GoodCondSqmBoxplots.png', width = 1600, height = 1200)
par(mfrow=c(1,3))
par(mar = c(5,10.5,1,2)+0.1)

meanOrderGoodCondSqm.Helsinki <- order(goodCondSqmCoefTable[(1:128)[cityAssignments == "Helsinki"],1])
boxPlotData.Helsinki <- groupEffectsSample[,((128+128+1):(128+128+128))[cityAssignments == "Helsinki"]] 

boxplot(boxPlotData.Helsinki[,meanOrderGoodCondSqm.Helsinki], 
        horizontal = T, 
        outline=F, 
        ylim = goodCondSqmRange,
        axes=F)
axis(side = 1, 
     round(seq(from = goodCondSqmRange[1], to = goodCondSqmRange[2], length.out = 5),-1),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Helsinki"]), 
     labels = (namesFromIndeces[cityAssignments == "Helsinki"])[meanOrderGoodCondSqm.Helsinki],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Helsinki"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Helsinki", 
      line = -2,
      cex.main = 1.5)


meanOrderGoodCondSqm.Espoo <- order(goodCondSqmCoefTable[(1:128)[cityAssignments == "Espoo"],1])
boxPlotData.Espoo <- groupEffectsSample[,((128+128+1):(128+128+128))[cityAssignments == "Espoo"]] 

boxplot(boxPlotData.Espoo[,meanOrderGoodCondSqm.Espoo], 
        horizontal = T, 
        outline=F, 
        ylim = goodCondSqmRange,
        axes=F)
axis(side = 1, 
     round(seq(from = goodCondSqmRange[1], to = goodCondSqmRange[2], length.out = 5),-1),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Espoo"]), 
     labels = (namesFromIndeces[cityAssignments == "Espoo"])[meanOrderGoodCondSqm.Espoo],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Espoo"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Espoo", 
      line = -2,
      cex.main = 1.5)

meanOrderGoodCondSqm.Vantaa <- order(goodCondSqmCoefTable[(1:128)[cityAssignments == "Vantaa"],1])
boxPlotData.Vantaa <- groupEffectsSample[,((128+128+1):(128+128+128))[cityAssignments == "Vantaa"]] 

boxplot(boxPlotData.Vantaa[,meanOrderGoodCondSqm.Vantaa], 
        horizontal = T, 
        outline=F, 
        ylim = goodCondSqmRange,
        axes=F)
axis(side = 1, 
     round(seq(from = goodCondSqmRange[1], to = goodCondSqmRange[2], length.out = 5),-1),
     cex.axis = 1.5)
axis(side = 2, 
     at = 1:length(namesFromIndeces[cityAssignments == "Vantaa"]), 
     labels = (namesFromIndeces[cityAssignments == "Vantaa"])[meanOrderGoodCondSqm.Vantaa],
     las = 2,
     cex.axis = 1.5)
abline(h = 1:length(namesFromIndeces[cityAssignments == "Vantaa"]), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.75))
title(main = "Vantaa", 
      line = -2,
      cex.main = 1.5)

par(mar = c(5,4,4,4)+0.1)
par(mfrow=c(1,1))
dev.off()


#########################################################################################################
# Bayesian R^2

# drawVariancePostSample <- function(postSample, likelihoodSigmaName, likelihoodNuName) {
#   sigmaPostSample <- postSample[,likelihoodSigmaName];
#   nuPostSample <- postSample[,likelihoodNuName];
# 
#   varSample <- (sigmaPostSample^2) * (nuPostSample/(nuPostSample-2));
# 
#   return(varSample);
# }
# 
# variancePostSample <- drawVariancePostSample(postSample = posteriorSamples.trueData,
#                                              likelihoodSigmaName = "sigma",
#                                              likelihoodNuName = "nu")
# hist(variancePostSample)
# summary(variancePostSample)
# 
# 
# getBayesianR2Draws <- function(postPredictiveDistDraws, residualVarianceDraws) {
#   # following (3) and appendix of http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
#   var_fit <- apply(X = postPredictiveDistDraws, MARGIN = 1, FUN = var)
#   return(var_fit/(var_fit + residualVarianceDraws))
# }
# 
# BayesianR2Draws <- getBayesianR2Draws(postPredictiveDistDraws = postPredDistDraws.estimation, residualVarianceDraws = variancePostSample);
# hist(BayesianR2Draws, xlim = c(0,1), nclass = 500);
# summary(BayesianR2Draws)
# 
# abline(v = median(BayesianR2Draws), lty = 2, col = 'red')
# 

