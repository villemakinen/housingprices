
# varying intercept model with t-distribution likelihood s.t. group means are dependent on ocean and road distances

library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/')

rm(list=ls()); gc(); 
# load(file = 'modelFit3.RData')
# looObj.trueData; plot(looObj.trueData); # EV p_loo ~147

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

# distribution of mean, median of replicated prices

scalingCoef <- 1.5;  
dir.create('./figures')

library(ggplot2)
png('./figures/model3replicatedMeans.png', width = 600*scalingCoef, height = 400*scalingCoef)
replicatedPrices.mean <- apply(dataReplications, 1, mean);
range.mean <- range(replicatedPrices.mean)
hist(replicatedPrices.mean, 
     xlim = c(250000, 280000),
     nclass = 500, 
     xlab = "Mean", 
     main = "Replicated means")
abline(v = mean(estimationSet$Price), col = 'red', lwd = 1, lty = 2)
abline(h = 0)
dev.off()

png('./figures/model3replicatedMedians.png', width = 600*scalingCoef, height = 400*scalingCoef)
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

estimationSetNeighborhoods <- as.character(combinedData.orig$NeighborhoodFinalized)[-testSetIndeces]

replicatedMeansPerNeighborhood <- apply(X = dataReplications, 
                                        MARGIN = 1, 
                                        FUN = function(rivi) tapply(X = rivi, INDEX = estimationSetNeighborhoods, mean))

estimationSetMeansPerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, mean)
sampleSizePerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, length)

# hack to add the uppercase letters back.. 
neighborhoodNames <- names(estimationSetMeansPerNeighborhood); 
substring(neighborhoodNames,1,1) <- toupper(substr(neighborhoodNames,1,1)) 
neighborhoodNames[neighborhoodNames == "Etu-töölö"] <- "Etu-Töölö";
neighborhoodNames[neighborhoodNames == "Pohjois-tapiola"] <- "Pohjois-Tapiola";
neighborhoodNames[neighborhoodNames == "Taka-töölö"] <- "Taka-Töölö";
neighborhoodNames[neighborhoodNames == "Itä-hakkila"] <- "Itä-Hakkila";

rownames(replicatedMeansPerNeighborhood) <- neighborhoodNames; 
names(estimationSetMeansPerNeighborhood) <- neighborhoodNames;

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

library(ggplot2)
png('./figures/model3replicatedMeansHelsinki.png', width = 600*scalingCoef, height = 400*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Helsinki), outline=F, ylim = c(100000, 750000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nHelsinki")
points(observerdMeans.Helsinki, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), labels = rownames(replicatedMeansPerNeighborhood.Helsinki), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 100000, to = 750000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

png('./figures/model3replicatedMeansEspoo.png', width = 600*scalingCoef, height = 400*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Espoo), outline=F, ylim = c(50000, 700000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nEspoo")
points(observerdMeans.Espoo, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Espoo), labels = rownames(replicatedMeansPerNeighborhood.Espoo), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Espoo), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 50000, to = 700000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

png('./figures/model3replicatedMeansVantaa.png', width = 600*scalingCoef, height = 400*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Vantaa), outline=F, ylim = c(-50000, 550000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nVantaa")
points(observerdMeans.Vantaa, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), labels = rownames(replicatedMeansPerNeighborhood.Vantaa), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
abline(h = 0)
axis(side = 2, seq(from = -50000, to = 550000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

############################################################################################################
# R-hats and effective samples sizes 

library(rstan)

rHatNEfftable <- summary(stanFit.trueData)$summary
rHatNEfftable <- rHatNEfftable[-grep("log_lik", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("lp__", rownames(rHatNEfftable)),]
rHatNEfftable <- rHatNEfftable[-grep("Intercept_offset", rownames(rHatNEfftable)),]

# se_mean = sd/sqrt(n_eff)
rHatNEfftable <- rHatNEfftable[,c("mean", "sd", "2.5%", "50%", "97.5%", "n_eff", "Rhat")] 

interceptCoefTable <- rHatNEfftable[grep("Intercept_coef", rownames(rHatNEfftable)),]; 
parameterTable <- rHatNEfftable[-grep("Intercept_coef", rownames(rHatNEfftable)),];

neighborhoodIndexHash <- data.frame(neighborhoodName = as.character(distanceData$identifier), 
                                    neighborhoodIndex = as.numeric(distanceData$identifier), 
                                    stringsAsFactors = F)


neighborhoodIndexHash <- unique(neighborhoodIndexHash)
neighborhoodIndexHash <- neighborhoodIndexHash[order(neighborhoodIndexHash$neighborhoodIndex),]


# hack to add the uppercase letters back.. 
neighborhoodNames <- neighborhoodIndexHash$neighborhoodName 
substring(neighborhoodNames,1,1) <- toupper(substr(neighborhoodNames,1,1)) 
neighborhoodNames[neighborhoodNames == "Etu-töölö"] <- "Etu-Töölö";
neighborhoodNames[neighborhoodNames == "Pohjois-tapiola"] <- "Pohjois-Tapiola";
neighborhoodNames[neighborhoodNames == "Taka-töölö"] <- "Taka-Töölö";
neighborhoodNames[neighborhoodNames == "Itä-hakkila"] <- "Itä-Hakkila";
neighborhoodNames[neighborhoodNames == "Vanha-nuuksio"] <- "Vanha-Nuuksio";

neighborhoodIndexHash.hash <- hash(keys = neighborhoodIndexHash$neighborhoodIndex, 
                                   values = neighborhoodNames)

indeces <- rownames(interceptCoefTable)
chopNfirst <- function(string, N) substring(text = string, first = N+1, last = nchar(string)) 
chopLast <- function(string) substring(text = string, first = 1, last = nchar(string)-1) 
indeces <- sapply(X = indeces, FUN = chopNfirst, N = 15)
indeces <- sapply(X = indeces, FUN = chopLast)
namesFromIndeces <- sapply(X = indeces, FUN = function(x) neighborhoodIndexHash.hash[[x]] )

interceptCoefTable <- cbind(data.frame(namesFromIndeces), interceptCoefTable)

rownames(interceptCoefTable) <- NULL;

library(xtable)
xtable(parameterTable)
xtable(interceptCoefTable)

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

png('./figures/model3EstimationSetPIT.png', width = 600*scalingCoef, height = 400*scalingCoef)
hist(PITsample.estimation, 
     xlab = "Probability Integral Transform", 
     main = "PIT histogram, estimation set, model 3",
     probability = T)
dev.off()

############################################################################################################
# sharpness box plots following Gneiting, Balabdaoui, Raftery 2007

# sharpness measured by the width of credible intervals with (5 %, 95 %)-cut 

credibleIntervalWidths.90.estimation <- apply(X = postPredDistDraws.estimation, 
                                              MARGIN = 2, 
                                              function(otos) quantile(x = otos, probs =  0.95) - quantile(x = otos, probs =  0.05));

png('./figures/model3EstimationSet90CredIntSharpnessHistogram.png', width = 600*scalingCoef, height = 400*scalingCoef);  
hist(credibleIntervalWidths.90.estimation, 
     xlab = "width", 
     main = "Sharpness histogram, 90 % credible interval width, model 3",
     #xlim = c(180000,215000),
     nclass = 30)
abline(h=0)
dev.off();
mean(credibleIntervalWidths.90.estimation)

############################################################################################################
# graphing mean prices 

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

png('./figures/model3EstimationSetMeanPredScatter.png', width = 600*scalingCoef, height = 400*scalingCoef);  
plot(estimationSet$Price, 
     predDistMean.estimation,
     xlab = "true price", 
     ylab = "mean of price predictive distribution",
     main = "True price vs. mean of predictive distributions\nestimation set")
abline(a = 0, b = 1, lty = 2, col = 'red')
dev.off();

1- sum((estimationSet$Price -predDistMean.estimation)^2)/sum((estimationSet$Price-mean(estimationSet$Price))^2) 

predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

png('./figures/model3TestSetMeanPredScatter.png', width = 600*scalingCoef, height = 400*scalingCoef);  
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

png('./figures/model3TestSetProblemObservations.png', width = 600*scalingCoef, height = 400*scalingCoef);  
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




groupEffectsSample <- posteriorSamples.trueData[,grep("Intercept_coef", colnames(posteriorSamples.trueData))]; 

neighborhoodIndexHash <- data.frame(neighborhoodName = as.character(distanceData$identifier), 
                                    neighborhoodIndex = as.numeric(distanceData$identifier), 
                                    stringsAsFactors = F)

neighborhoodIndexHash <- unique(neighborhoodIndexHash)
neighborhoodIndexHash <- neighborhoodIndexHash[order(neighborhoodIndexHash$neighborhoodIndex),]


# hack to add the uppercase letters back.. 
neighborhoodNames <- neighborhoodIndexHash$neighborhoodName 
substring(neighborhoodNames,1,1) <- toupper(substr(neighborhoodNames,1,1)) 
neighborhoodNames[neighborhoodNames == "Etu-töölö"] <- "Etu-Töölö";
neighborhoodNames[neighborhoodNames == "Pohjois-tapiola"] <- "Pohjois-Tapiola";
neighborhoodNames[neighborhoodNames == "Taka-töölö"] <- "Taka-Töölö";
neighborhoodNames[neighborhoodNames == "Itä-hakkila"] <- "Itä-Hakkila";
neighborhoodNames[neighborhoodNames == "Vanha-nuuksio"] <- "Vanha-Nuuksio";

neighborhoodIndexHash.hash <- hash(keys = neighborhoodIndexHash$neighborhoodIndex, 
                                   values = neighborhoodNames)

indeces <- colnames(groupEffectsSample)
chopNfirst <- function(string, N) substring(text = string, first = N+1, last = nchar(string)) 
chopLast <- function(string) substring(text = string, first = 1, last = nchar(string)-1) 
indeces <- sapply(X = indeces, FUN = chopNfirst, N = 15)
indeces <- sapply(X = indeces, FUN = chopLast)
namesFromIndeces <- sapply(X = indeces, FUN = function(x) neighborhoodIndexHash.hash[[x]] )



neighborhoodToCityHash <- hash(keys = c('Alppiharju','Askisto','Asola','Eira','Espoon keskus','Espoonlahti','Etu-Töölö','Haaga','Hakunila','Hämeenkylä','Hämevaara','Haukilahti','Havukoski','Henttaa','Hermanni','Herttoniemi','Hiekkaharju','Ilola','Itä-Hakkila','Järvenperä','Jokiniemi','Kaarela','Kaartinkaupunki','Kaitaa','Kaivoksela','Kallio','Kamppi','Käpylä','Karakallio','Karhusuo','Karvasmäki','Katajanokka','Kauklahti','Kaupunginkallio','Keimola','Kilo','Kivistö','Kluuvi','Koivuhaka','Koivukylä','Kolmperä','Konala','Korso','Koskela','Kruununhaka','Kulosaari','Kumpula','Kuninkaala','Kuninkaanmäki','Kuurinniitty','Laajalahti','Laajasalo','Laakso','Laaksolahti','Lahnus','Länsimäki','Länsisatama','Latokaski','Lauttasaari','Leppäkorpi','Leppävaara','Lintuvaara','Lippajärvi','Malmi','Mankkaa','Martinlaakso','Matari','Matinkylä','Meilahti','Mellunkylä','Metsola','Mikkola','Munkkiniemi','Muurala','Myyrmäki','Niipperi','Niittykumpu','Nikinmäki','Nöykkiö','Nupuri','Olari','Otaniemi','Oulunkylä','Päiväkumpu','Pakila','Pakkala','Pasila','Perusmäki','Piispankylä','Pitäjänmäki','Pohjois-Tapiola','Pukinmäki','Punavuori','Rajakylä','Rekola','Ruskeasanta','Ruskeasuo','Saunalahti','Sepänkylä','Simonkylä','Sörnäinen','Soukka','Suurmetsä','Suutarila','Taka-Töölö','Tammisalo','Tammisto','Tapaninkylä','Tapiola','Tikkurila','Toukola','Tuomarinkylä','Ullanlinna','Vaarala','Vallila','Vanhakaupunki','Vantaanlaakso','Vanttila','Vapaala','Varisto','Vartiokylä','Viertola','Vierumäki','Viherlaakso','Viikki','Vuosaari','Westend','Ylästö', c('Ämmässuo', 'Bodom', 'Espoonkartano', 'Gumböle', 'Hakkila', 'Helsingin pitäjän kirkonkylä', 'Högnäs', 'Jokivarsi', 'Kaivopuisto', 'Karhusaari', 'Kiila',   'Kunnarla', 'Kurttila',  'Länsisalmi', 'Lapinkylä', 'Lentokenttä', 'Linnainen', 'Luhtaanmäki', 'Luukki', 'Myllymäki', 'Nuuksio', 'Ojanko', 'Östersundom', 'Petikko', 'Riipilä', 'Röylä', 'Salmenkallio', 'Santahamina', 'Seutula', 'Siikajärvi', 'Sotunki', 'Suomenlinna', 'Suvisaaristo', 'Talosaari', 'Ultuna',   'Vallinoja', 'Vanha-Nuuksio', 'Vanhakartano', 'Vartiosaari', 'Velskola', 'Veromies', 'Vestra',  'Viinikkala', 'Villinki')),
                               values = c('Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Helsinki','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Espoo','Espoo','Espoo','Helsinki','Espoo','Espoo','Vantaa','Espoo','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Vantaa','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Vantaa','Espoo','Espoo','Helsinki','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Vantaa','Espoo','Espoo','Espoo','Helsinki','Espoo','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Vantaa','Espoo','Espoo','Vantaa','Espoo','Espoo','Espoo','Espoo','Helsinki','Vantaa','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Helsinki','Vantaa','Espoo','Vantaa','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Espoo','Vantaa',c('Espoo',    'Espoo', 'Espoo',         'Espoo',    'Vantaa', 'Vantaa',                        'Espoo', 'Vantaa',    'Helsinki',    'Helsinki',   'Vantaa', 'Espoo',     'Espoo',     'Vantaa',      'Vantaa',   'Vantaa',      'Vantaa',    'Vantaa',       'Espoo', 'Vantaa',    'Espoo',  'Vantaa', 'Helsinki',    'Vantaa',   'Vantaa', 'Espoo', 'Helsinki',      'Helsinki',     'Vantaa', 'Espoo',      'Vantaa',  'Helsinki',      'Espoo',      'Helsinki',  'Helsinki', 'Vantaa',    'Espoo',         'Espoo',        'Helsinki',   'Espoo',     'Vantaa',    'Vantaa', 'Vantaa',  'Helsinki'))) 


cityAssignments <- sapply(namesFromIndeces, function(x) neighborhoodToCityHash[[x]])

# intercepts - need to split city by city 
interceptRange <- quantile(x = groupEffectsSample[,(1:172)], probs = c(0.0001, 0.9999))

png('./figures/model3interceptsBoxplots.png', width = 1600, height = 1600)
par(mfrow=c(1,3))
par(mar = c(5,10.5,1,2)+0.1)

meanOrderIntercept.Helsinki <- order(interceptCoefTable[(1:172)[cityAssignments == "Helsinki"],1])
boxPlotData.Helsinki <- groupEffectsSample[,(1:172)[cityAssignments == "Helsinki"]] 

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


meanOrderIntercept.Espoo <- order(interceptCoefTable[(1:172)[cityAssignments == "Espoo"],1])
boxPlotData.Espoo <- groupEffectsSample[,(1:172)[cityAssignments == "Espoo"]] 

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

meanOrderIntercept.Vantaa <- order(interceptCoefTable[(1:172)[cityAssignments == "Vantaa"],1])
boxPlotData.Vantaa <- groupEffectsSample[,(1:172)[cityAssignments == "Vantaa"]] 

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




#################################################################################
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
# largDifIndeces.test <- order(abs(testSet$Price - predDistMean.test), decreasing = T);
# k <- 6; 
# targetIndex <- largDifIndeces.test[k];
# hist(postPredDistDraws.test[,targetIndex], nclass = 50)
# abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);
# 
# getBayesianR2Draws <- function(postPredictiveDistDraws, residualVarianceDraws) {
#   # following (3) and appendix of http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
#   var_fit <- apply(X = postPredictiveDistDraws, MARGIN = 1, FUN = var)
#   return(var_fit/(var_fit + residualVarianceDraws))
# }
# 
# BayesianR2Draws <- getBayesianR2Draws(postPredictiveDistDraws = postPredDistDraws.estimation, 
#                                       residualVarianceDraws = variancePostSample);
# hist(BayesianR2Draws);
# abline(v = median(BayesianR2Draws), lty = 2, col = 'red')
# summary(BayesianR2Draws);
# 
# 
# 
