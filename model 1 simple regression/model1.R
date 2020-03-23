
# simple model with t-distribution likelihood

library(extraDistr)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/')

rm(list=ls()); gc();
# load(file = 'modelFit1.RData')
# looObj.trueData; plot(looObj.trueData); # EV p_loo ~11.2

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

scalingCoef <- 1.5;  
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
# residual plot - difficult to produce a good figure..

# residuals  <- sapply(1:length(estimationSet$Price), function(k) dataReplications[,k] - estimationSet$Price[k])
# 
# residualPlottingData <- do.call(rbind, lapply(1:ncol(residuals), function(k) cbind(rep(k, length(residuals[,k])), residuals[,k])))
# 
# colnames(residualPlottingData) <- c("observation", "residual")
# 
# residualPlottingData <- data.frame(residualPlottingData)
# 
# library(ggplot2)
# 
# index <- 2600; 
# nObs <- 100; 
# ggplot(data = residualPlottingData[(index*8000):(index*8000 + nObs*8000),], mapping =  aes( x = as.factor(observation), y = residual)) + 
#          geom_boxplot(outlier.shape = NA) +
#          ylim(-300000, 300000)


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

png('./figures/model1replicatedMeansHelsinki.png', width = 600*scalingCoef, height = 400*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Helsinki), outline=F, ylim = c(100000, 750000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nHelsinki")
points(observerdMeans.Helsinki, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), labels = rownames(replicatedMeansPerNeighborhood.Helsinki), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 100000, to = 750000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()


png('./figures/model1replicatedMeansEspoo.png', width = 600*scalingCoef, height = 400*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Espoo), outline=F, ylim = c(0, 800000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nEspoo")
points(observerdMeans.Espoo, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Espoo), labels = rownames(replicatedMeansPerNeighborhood.Espoo), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Espoo), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, seq(from = 0, to = 800000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()


png('./figures/model1replicatedMeansVantaa.png', width = 600*scalingCoef, height = 400*scalingCoef)
par(mar = c(8.1, 5.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Vantaa), outline=F, ylim = c(-50000, 650000), axes=F, main = "Neighborhood average prices,\nreplicated data vs. realized value,\nVantaa")
points(observerdMeans.Vantaa, col = 'red', pch = 13);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), labels = rownames(replicatedMeansPerNeighborhood.Vantaa), las = 2, pch = 0.8)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
abline(h = 0)
axis(side = 2, seq(from = -50000, to = 650000, by = 50000), las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1))
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

png('./figures/model1EstimationSetPIT.png', width = 600*scalingCoef, height = 400*scalingCoef)
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
# png('./figures/model1EstimationSet90CredIntSharpnessBoxplot.png', width = 600*scalingCoef, height = 400*scalingCoef);  
# boxplot(credibleIntervalWidths.90.estimation, outline=F)
# dev.off();

png('./figures/model1EstimationSet90CredIntSharpnessHistogram.png', width = 600*scalingCoef, height = 400*scalingCoef);  
hist(credibleIntervalWidths.90.estimation, 
     xlab = "width", 
     main = "Sharpness histogram, 90 % credible interval width, model 1")
abline(h=0)
dev.off();
mean(credibleIntervalWidths.90.estimation)


# png('./figures/model1EstimationSet50CredIntSharpnessBoxplot.png', width = 600*scalingCoef, height = 400*scalingCoef); 
# boxplot(credibleIntervalWidths.50.estimation, outline=F)
# dev.off(); 

############################################################################################################
# graphing means  

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

png('./figures/model1EstimationSetMeanPredScatter.png', width = 600*scalingCoef, height = 400*scalingCoef);  
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

largDifIndeces.test <- order((testSet$Price - predDistMean.test)^2, decreasing = T);

png('./figures/model1TestSetMeanPredScatter.png', width = 600*scalingCoef, height = 400*scalingCoef);  
problematicObservations <- largDifIndeces.test[1:9]
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


# k <- 7;
# targetIndex <- largDifIndeces.test[k];
# hist(postPredDistDraws.test[,targetIndex], nclass = 50)
# abline(v = testSet$Price[targetIndex], col = 'red', lty = 2);

#########################################################################################
# difficult test set observations, predictive distributions histograms 

testSetDifficultObsIndeces <- c(259, 1290, 225, 118, 518, 443, 708, 200, 63)

png('./figures/model1TestSetProblemObservations.png', width = 600*scalingCoef, height = 400*scalingCoef);  
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

#########################################################################################
# Bayesian R^2(?)
# drawVariancePostSample <- function(postSample, likelihoodSigmaName, likelihoodNuName) {
#   sigmaPostSample <- postSample[,likelihoodSigmaName];
#   nuPostSample <- postSample[,likelihoodNuName];
#   
#   varSample <- (sigmaPostSample^2) * (nuPostSample/(nuPostSample-2));
#   
#   return(varSample); 
# }
# 
# variancePostSample <- drawVariancePostSample(postSample = posteriorSamples.trueData, likelihoodSigmaName = "sigma", likelihoodNuName = "nu")
# hist(variancePostSample)
# summary(variancePostSample)
# 
# # - LOO R^2, avehtari.github.io/bayes_R2/bayes_R2.html
# # http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
# 
# getBayesianR2Draws <- function(postPredictiveDistDraws, residualVarianceDraws) {
#   # following (3) and appendix of http://www.stat.columbia.edu/~gelman/research/unpublished/bayes_R2_v3.pdf
#   var_fit <- apply(X = postPredictiveDistDraws, MARGIN = 1, FUN = var)
#   return(var_fit/(var_fit + residualVarianceDraws))
# }
# 
# BayesianR2Draws <- getBayesianR2Draws(postPredictiveDistDraws = postPredDistDraws.estimation, residualVarianceDraws = variancePostSample);
# hist(BayesianR2Draws); 
# abline(v = median(BayesianR2Draws), lty = 2, col = 'red')
# 




