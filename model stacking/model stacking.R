
# model stacking with loo packagae (stacking of predictive distributions)

# approach from https://cran.r-project.org/web/packages/loo/vignettes/loo2-weights.html 

library(rstan)
library(loo)
library(LaplacesDemon)
library(parallel)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model stacking/')

rm(list=ls()); gc(); 
# load("stackingWeightsPostPredDrawsDone.RData")


###########
# retrieving loo

modelFitFilenames <- c('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/modelFit1.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 2 simple varying intercept model/modelFit2.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/modelFit3.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/modelFit4.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/PSISLOOfix.RData' # 
                       ); 

lpd_point <-NULL; 

counter <- 1; 

listOfPosteriorPredictiveDensities <- list(); 
listOfPosteriorSamples <- list(); 
listOfPosteriorPredictiveDistSamplingFunctions <- list();

for(filename in modelFitFilenames) {
  load(filename)
  
  lpd_point <- cbind(lpd_point, pointwiseElpdLooVectorForStacking);

  listOfPosteriorPredictiveDensities[[counter]] <- evaluatePosteriorPredictive;
  listOfPosteriorSamples[[counter]] <- posteriorSamples.trueData; 
  listOfPosteriorPredictiveDistSamplingFunctions[[counter]] <- getPosteriorPredictiveDraws;
  
  counter <- counter + 1;   
}

###########
# calculating stacking weights

weights <- stacking_weights(lpd_point)

library(xtable)

xtable(data.frame(t(as.numeric(weights))))


# checks for the stacking weights
# weightVector <- matrix(c(1,0,0,0,0),ncol = 1)
# # weightVector <- matrix(weights,ncol = 1)
# sum(log(exp(lpd_point) %*% weightVector))

# stacking  / -45431.49
# model 1   / -48249.87
# model 2   / -46294.43
# model 3   / -46285.67
# model 4   / -46334.23
# model 5   / -45451.76
# => ok, when calculated with the loo predictive distributions

###########
# hack to evaluate model 3 densities etc. correctly

load('/home/asdf/Desktop/gradu/git/housingprices/model 2 simple varying intercept model/modelFit2.RData')
hashTargets <- combinedData$NeighborhoodAssignment 
load('/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/modelFit3.RData')
hashStarts <- combinedData$NeighborhoodAssignment 
hashDf <- data.frame(start = as.character(hashTargets), to = as.character(hashStarts), stringsAsFactors = F)
hashDf <- unique(hashDf)
neighborhoodBetweenModelsHash <- hash(keys = hashDf$start, values = hashDf$to)

###########
# function for combined predictive distribution


evaluatePosteriorPredictive.combined <- function(Price.pointEvaluation,  
                                                 predictiveVariables, 
                                                 
                                                 posteriorPredictiveDensities_list, 
                                                 postSample_list,
                                                 
                                                 weights,
                                                 
                                                 likelihoodSigmaName, 
                                                 likelihoodNuName,
                                                 
                                                 # hack added on 27.10.2019 to guarantee that model 3 neighborhoods get evaluated correctly
                                                 # when calling this function with assignments from models 2,4,5
                                                 
                                                 neighborhoodBetweenModelsHash
                                                 
                                                 ) {
  
  evaluatedDensities <- rep(NA, length(weights)); 
  
  # evaluating each density
  for(k in 1:length(posteriorPredictiveDensities_list)) {
    singleDensity <- posteriorPredictiveDensities_list[[k]];
    singlePosteriorSample <- postSample_list[[k]];
    
    # hack for model 3 
    
    inputPredictiveVariables <- predictiveVariables;
    if(k == 3) {
      inputPredictiveVariables$NeighborhoodAssignment <- as.numeric(neighborhoodBetweenModelsHash[[as.character(inputPredictiveVariables$NeighborhoodAssignment)]])
    }
    
    evaluatedDensities[k] <- singleDensity(Price.pointEvaluation = Price.pointEvaluation, 
                                           predictiveVariables = inputPredictiveVariables, 
                                           postSample = singlePosteriorSample, 
                                           likelihoodSigmaName = likelihoodSigmaName, 
                                           likelihoodNuName = likelihoodNuName);
  }
  
 return(sum(weights*evaluatedDensities));
}



########### 
# sampling from the stacking distributon 


# first, samples from each individual models are generated 
modelFitFilenames <- c('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/modelFit1.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 2 simple varying intercept model/modelFit2.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/modelFit3.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/modelFit4.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/modelFit5.RData' # 
);


listOfPosteriorPredictiveDistributionsEstimationSet <- list();
listOfPosteriorPredictiveDistributionsTestSet <- list();


counter <- 1; 
for(filename in modelFitFilenames) {
  cat("handling", filename, "...\n"); 
  load(filename)
  
  # doing the predictions 
  set.seed(1234);
  # estimation set draws from predictive distribution
  postPredDistDraws.estimation.singleModel <- getPosteriorPredictiveDraws(dataSet = estimationSet,
                                                                          postSample = posteriorSamples.trueData,
                                                                          likelihoodSigmaName = "sigma",
                                                                          likelihoodNuName = "nu")
  
  listOfPosteriorPredictiveDistributionsEstimationSet[[counter]] <- postPredDistDraws.estimation.singleModel;
  
  # test set draws from predictive distribution
  postPredDistDraws.test.singleModel <- getPosteriorPredictiveDraws(dataSet = testSet,
                                                                    postSample = posteriorSamples.trueData,
                                                                    likelihoodSigmaName = "sigma",
                                                                    likelihoodNuName = "nu")
  
  listOfPosteriorPredictiveDistributionsTestSet[[counter]] <- postPredDistDraws.test.singleModel;
  
  counter <- counter + 1; 
}


# second, samples are used for sampling using the stacking weights 
nModels <- length(listOfPosteriorPredictiveDistributionsEstimationSet);

nDraws.estimationSet <- nrow(listOfPosteriorPredictiveDistributionsEstimationSet[[1]])
nObservations.estimationSet <- ncol(listOfPosteriorPredictiveDistributionsEstimationSet[[1]])

modelChoiceDraws <- sample(x = 1:nModels, 
                           nDraws.estimationSet*nObservations.estimationSet, 
                           replace = T, 
                           prob = weights);

modelChoiceDraws.estimationSet <- matrix(modelChoiceDraws, 
                                         ncol = nObservations.estimationSet, 
                                         nrow = nDraws.estimationSet)

### censoring the samples to express everything as sums 
postPredDistDraws.estimation <- listOfPosteriorPredictiveDistributionsEstimationSet[[1]]; 
postPredDistDraws.estimation[,] <- 0; 

for(k in 1:length(listOfPosteriorPredictiveDistributionsEstimationSet)) {
  drawsToAdd  <- listOfPosteriorPredictiveDistributionsEstimationSet[[k]]; 
  drawsToAdd[modelChoiceDraws.estimationSet != k] <- 0; 
  postPredDistDraws.estimation <- postPredDistDraws.estimation + drawsToAdd;
}



nModels <- length(listOfPosteriorPredictiveDistributionsTestSet);

nDraws.testSet <- nrow(listOfPosteriorPredictiveDistributionsTestSet[[1]])
nObservations.testSet <- ncol(listOfPosteriorPredictiveDistributionsTestSet[[1]])

modelChoiceDraws <- sample(x = 1:nModels, 
                           nDraws.testSet*nObservations.testSet, 
                           replace = T, 
                           prob = weights);

modelChoiceDraws.testSet <- matrix(modelChoiceDraws, 
                                   ncol = nObservations.testSet, 
                                   nrow = nDraws.testSet)

### censoring the samples to express everything as sums 
postPredDistDraws.test <- listOfPosteriorPredictiveDistributionsTestSet[[1]]; 
postPredDistDraws.test[,] <- 0; 

for(k in 1:length(listOfPosteriorPredictiveDistributionsTestSet)) {
  drawsToAdd  <- listOfPosteriorPredictiveDistributionsTestSet[[k]]; 
  drawsToAdd[modelChoiceDraws.testSet != k] <- 0; 
  postPredDistDraws.test <- postPredDistDraws.test + drawsToAdd;
}

# save.image("stackingWeightsPostPredDrawsDone.RData")

############################################################################################################
# replicated price histograms compared to true price histogram   

# distribution of mean, median of replicated prices

dataReplications <- postPredDistDraws.estimation;

scalingCoef <- 1.5;  
dir.create('./figures')

cexAxisConstant <- 2; 
cexLabConstant <- 2;
cexMainConstant <- 2; 


library(ggplot2)


png('./figures/stackingModelreplicatedMeans.png', width = 800*scalingCoef, height = 400*scalingCoef)
par(mar = c(5,10,6,2) + 0.1)
replicatedPrices.mean <- apply(dataReplications, 1, mean);
minPlottedValue <- 230000; 
maxPlottedValue <- 290000;
replicatedPrices.mean.censored <- replicatedPrices.mean;
replicatedPrices.mean.censored <- replicatedPrices.mean.censored[replicatedPrices.mean.censored >= minPlottedValue];
replicatedPrices.mean.censored <- replicatedPrices.mean.censored[replicatedPrices.mean.censored <= maxPlottedValue];
mainText <- paste("Replicated mean prices, stacking model, plotted with ", 
                  length(replicatedPrices.mean.censored), "/", length(replicatedPrices.mean)," replications",  sep ="")
hist(replicatedPrices.mean.censored, 
     xlim = c(minPlottedValue,maxPlottedValue),
     ylim = c(0, 3500),
     nclass = 15,
     main = "",
     cex.axis = cexAxisConstant, 
     cex.lab = cexLabConstant,
     cex.main = cexMainConstant,
     xlab = "",
     ylab = "",
     axes=F)
axis(side = 1,
     at = seq(from = minPlottedValue, to = maxPlottedValue, by = 10000),
     cex.axis = cexAxisConstant)
axis(side = 2,
     at = seq(from = 0, to = 3500, by = 500),
     cex.axis = cexAxisConstant,
     las = 2)
title(ylab = "Frequency", cex.lab = cexLabConstant, line = 7)
title(main = mainText, 
      line = 1,
      cex.main = cexMainConstant)
abline(v = mean(estimationSet$Price), col = 'red', lwd = 2.5, lty = 2)
par(mar = c(5,4,4,2) + 0.1)
dev.off()


png('./figures/stackingModelreplicatedMedians.png', width = 800*scalingCoef, height = 400*scalingCoef)
replicatedPrices.median <- apply(dataReplications, 1, median);
par(mar = c(5,10,6,2) + 0.1)
minPlottedValue <- 225000;
maxPlottedValue <- 265000;
replicatedPrices.median.censored <- replicatedPrices.median;
replicatedPrices.median.censored <- replicatedPrices.median.censored[replicatedPrices.median.censored >= minPlottedValue];
replicatedPrices.median.censored <- replicatedPrices.median.censored[replicatedPrices.median.censored <= maxPlottedValue];
mainText <- paste("Replicated median prices, stacking model, plotted with ", 
                  length(replicatedPrices.median.censored), "/", length(replicatedPrices.median)," replications",  sep ="")
hist(replicatedPrices.median.censored,
     xlab = "",
     ylab = "", 
     main = "",
     xlim = c(minPlottedValue,maxPlottedValue),
     ylim = c(0,2000),
     nclass = 10,
     cex.axis = cexAxisConstant, 
     cex.lab = cexLabConstant,
     cex.main = cexMainConstant,   
     axes=F)
abline(v = median(estimationSet$Price), col = 'red', lwd = 2.5, lty = 2)
axis(side = 1,
     at = seq(from = minPlottedValue, to = maxPlottedValue, by = 10000),
     cex.axis = cexAxisConstant)
axis(side = 2,
     at = seq(from = 0, to = 2000, by = 500),
     cex.axis = cexAxisConstant,
     las = 2)
title(ylab = "Frequency", cex.lab = cexLabConstant, line = 7)
title(main = mainText, 
      line = 1,
      cex.main = cexMainConstant)
par(mar = c(5,4,4,2) + 0.1)
dev.off()


############################################################################################################
# average price per neighborhood

estimationSetNeighborhoods <- combinedData.orig$NeighborhoodFinalized[-testSetIndeces]

replicatedMeansPerNeighborhood <- apply(X = dataReplications, 
                                        MARGIN = 1, 
                                        FUN = function(rivi) tapply(X = rivi, INDEX = estimationSetNeighborhoods, mean))

estimationSetMeansPerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, mean)

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

library(ggplot2)

cexAxisConstant <- 1.6; 
cexLabConstant <- 2;
cexMainConstant <- 1.8; 

png('./figures/stackingModelReplicatedMeansHelsinki.png', width = 800*scalingCoef, height = 400*scalingCoef)
par(mar = c(12.1, 8.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Helsinki), outline=F, ylim = c(100000, 750000), axes=F, main = "")
points(observerdMeans.Helsinki, col = 'red', pch = 4, lwd = 2);
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), labels = rownames(replicatedMeansPerNeighborhood.Helsinki), las = 2, pch = 0.8,cex.axis = cexAxisConstant)
title(main = "Neighborhood replicated mean prices for Helsinki", 
      line = 1,
      cex.main = cexMainConstant)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, round(seq(from = 100000, to = 750000, length.out=5),0), las = 2, cex.axis = cexAxisConstant)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

png('./figures/stackingModelReplicatedMeansEspoo.png', width = 800*scalingCoef, height = 400*scalingCoef)
par(mar = c(12.1, 8.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Espoo), outline=F, ylim = c(50000, 700000), axes=F, main = "")
points(observerdMeans.Espoo, col = 'red', pch = 4, lwd = 2);
title(main = "Neighborhood replicated mean prices for Espoo", 
      line = 1,
      cex.main = cexMainConstant)
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Espoo), labels = rownames(replicatedMeansPerNeighborhood.Espoo), las = 2, pch = 0.8, cex.axis = cexAxisConstant)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Espoo), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, round(seq(from = 50000, to = 700000, length.out = 5), 0), las = 2, cex.axis = cexAxisConstant)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

png('./figures/stackingModelReplicatedMeansVantaa.png', width = 800*scalingCoef, height = 400*scalingCoef)
par(mar = c(12.1, 8.1, 4.1, 2.1))
boxplot(t(replicatedMeansPerNeighborhood.Vantaa), outline=F, ylim = c(-10000, 600000), axes=F, main = "")
points(observerdMeans.Vantaa, col = 'red', pch = 4, lwd = 2);
title(main = "Neighborhood replicated mean prices for Vantaa", 
      line = 1,
      cex.main = cexMainConstant)
axis(side = 1, at = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), labels = rownames(replicatedMeansPerNeighborhood.Vantaa), las = 2, pch = 0.8, cex.axis = cexAxisConstant)
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), lty = 3, lwd = 0.5, col = alpha('gray', 0.95))
axis(side = 2, round(seq(from = -10000, to = 600000, length.out = 5), 0), las = 2, cex.axis = cexAxisConstant)
par(mar = c(5.1, 4.1, 4.1, 2.1))
dev.off()

############################################################################################################
# PIT histograms

PITsample.estimation <- sapply(1:nrow(estimationSet), function(k) mean(postPredDistDraws.estimation[,k] <= estimationSet$Price[k]))

png('./figures/stackingModelEstimationSetPIT.png', width = 800*scalingCoef, height = 400*scalingCoef)
par(mar = c(5.1, 8.1, 4.1, 2.1))
hist(PITsample.estimation, 
     xlab = "Probability Integral Transform", 
     ylab = "", 
     main = "PIT histogram, stacking model",
     nclass  = 20, 
     probability = T,
     cex.lab = cexLabConstant,
     cex.axis = cexAxisConstant,
     cex.main = cexMainConstant,
     axes = F)
abline(h = 1, lty = 2)
axis(side = 1,
     at = seq(from = 0, to = 1, length.out = 5),
     cex.axis = cexAxisConstant);
axis(side = 2,
     at = round(seq(from = 0, to = 1.25, length.out = 6), 2),
     las = 2,
     cex.axis = cexAxisConstant);
title(ylab = "Density", cex.lab = cexLabConstant, line = 5)
dev.off(); 
par(mar = c(5.1, 4.1, 4.1, 2.1))

############################################################################################################
# sharpness box plots following Gneiting, Balabdaoui, Raftery 2007

# sharpness measured by the width of credible intervals with (5 %, 95 %)-cut 

credibleIntervalWidths.90.estimation <- apply(X = postPredDistDraws.estimation, 
                                              MARGIN = 2, 
                                              function(otos) quantile(x = otos, probs =  0.95) - quantile(x = otos, probs =  0.05));



cexMainConstant <- 2; 
cexLabConstant <- 2; 
cexAxisConstant <- 2; 

png('./figures/stackingModelEstimationSet90CredIntSharpnessHistogram.png', width = 800*scalingCoef, height = 400*scalingCoef);  
par(mar = c(5,7,4,2) + 0.1)
hist(credibleIntervalWidths.90.estimation, 
     xlab = "90 % credible interval width",  
     main = "Sharpness histogram, 90 % credible interval width, stacking model",
     xlim = c(140000, 350000),
     nclass = 130,
     axes = F, 
     probability = T,
     cex.lab = cexLabConstant,
     cex.main = cexMainConstant,
     ylim = c(0, 0.000065));
axis(side = 1,
     at = seq(from = 140000, to = 350000, length.out = 5),
     cex.axis = cexAxisConstant);
axis(side = 2,
     at = c(0, 0.000065), #seq(from = 0, to = 0.000125, length.out = 5),
     labels = c("0", "6.5e-5"),
     las = 2,
     cex.axis = cexAxisConstant);
dev.off();
mean(credibleIntervalWidths.90.estimation)

par(mar = c(5,4,4,2) + 0.1)


############################################################################################################
# graphing mean prices 

predDistMean.estimation <- apply(postPredDistDraws.estimation, MARGIN = 2, mean)

cexMainConstant <- 1.8; 
cexLabConstant <- 1.7; 
cexAxisConstant <- 1.5; 

png('./figures/stackingModlEstimationSetMeanPredScatter.png', width = 800*scalingCoef, height = 400*scalingCoef); 
par(mar = c(5,10,4,2) + 0.1)
plot(estimationSet$Price, 
     predDistMean.estimation,
     xlab = "Observed price",
     ylab = "",
     main = "",
     ylim = c(0, 1800000),
     xlim = c(0, 2500000), 
     axes= F,
     cex.lab = cexLabConstant,
     )
axis(side = 1,
     at = seq(from = 0, to = 2500000, length.out = 6),
     cex.axis = cexAxisConstant);
axis(side = 2,
     at = seq(from = 0, to = 1800000, length.out = 7),
     las = 2,
     cex.axis = cexAxisConstant);
title(ylab = "Mean of predictive\ndistribution draws", cex.lab = cexLabConstant, line = 6.5)
title(main = "Estimation set price scatter plot, stacking model", 
      cex.main = cexMainConstant,
      line = 0.5)
abline(a = 0, b = 1, lty = 2, col = 'red')
dev.off();
par(mar = c(5,4,4,2) + 0.1)

1 - sum((estimationSet$Price -predDistMean.estimation)^2)/sum((estimationSet$Price-mean(estimationSet$Price))^2) 

predDistMean.test <- apply(postPredDistDraws.test, MARGIN = 2, mean)

png('./figures/stackingModelTestSetMeanPredScatter.png', width = 800*scalingCoef, height = 400*scalingCoef);  
problematicObservations <- c(259, 1290, 
                             #225, 118, 518, 443, 708, 
                             200, 63)
par(mar = c(5,10,4,2) + 0.1)
plot(testSet$Price, 
     predDistMean.test,
     xlab = "Observed price",
     ylab = "",
     main = "",
     ylim = c(0, 1500000),
     xlim = c(0, 2500000), 
     axes= F,
     cex.lab = cexLabConstant)
abline(a = 0, b = 1, lty = 2, col = 'red')
axis(side = 1,
     at = seq(from = 0, to = 2500000, length.out = 6),
     cex.axis = cexAxisConstant);
axis(side = 2,
     at = seq(from = 0, to = 1500000, length.out = 6),
     las = 2,
     cex.axis = cexAxisConstant);
title(ylab = "Mean of predictive\ndistribution draws", cex.lab = cexLabConstant, line = 6.5)
title(main = "Test set price scatter plot, stacking model", 
      cex.main = cexMainConstant,
      line = 0.5)

points(testSet$Price[problematicObservations], 
       predDistMean.test[problematicObservations],
       col = 'darkgreen',
       lwd = 1.1)

with(testSet[problematicObservations,], text(testSet$Price[problematicObservations], 
                                             predDistMean.test[problematicObservations],
                                             labels = rownames(testSet)[problematicObservations],
                                             pos = 1, 
                                             cex = 1.4, 
                                             col = 'darkgreen'))

par(mar = c(5,4,4,2) + 0.1)
dev.off();

1 - sum((testSet$Price - predDistMean.test)^2)/sum((testSet$Price-mean(testSet$Price))^2) 

#########################################################################################
# difficult test set observations, predictive distributions histograms 

testSetDifficultObsIndeces <- c(259, 1290, 
                                # 225, 118, 518, 443, 708, 
                                200, 63)

png('./figures/stackingModelTestSetChosenObservations.png', width = 800*scalingCoef, height = 400*scalingCoef);  

par(mfrow=c(2,2)); 
cexLabConstant <- 1.4; 
cexAxisConstant <- 1.5; 
cexMainConstant <- 1.8;
xLimitMatrix <- matrix(c(0, 2500000,
                         0, 1800000,
                         0, 1800000,
                         0, 1400000), nrow = 4, ncol = 2, byrow = T)
for(k in 1:length(testSetDifficultObsIndeces)) {
  testSetIndex <- testSetDifficultObsIndeces[k]
  valueVector <- postPredDistDraws.test[,testSetIndex];
  # removing 0.1 % extreme values from both sides
  valueVector <- valueVector[valueVector <= quantile(valueVector, probs = 1 - 0.001) & valueVector >= quantile(valueVector, probs = 0.001)]
  truePrice <- testSet$Price[testSetIndex]
  xlim.histogram <- xLimitMatrix[k,]; 
  par(mar = c(7,7,3,1) + 0.1)
  hist(valueVector, 
       xlim = xlim.histogram,
       ylim = c(0, 10e-6),
       main = "",
       nclass = 20, 
       probability = T,
       cex.lab = cexLabConstant,
       xlab = "",
       ylab = "", 
       axes = F);
  abline(v = truePrice, lty = 2, col = 'red', lwd = 2)
  axis(side = 2,
       at = c(0, 10e-6), #seq(from = 0, to = 0.000125, length.out = 5),
       labels = c("0", "1e-5"),
       las = 2,
       cex.axis = cexAxisConstant);
  axis(side = 1,
       at = round(seq(from = xlim.histogram[1], to = xlim.histogram[2], length.out = 5)),
       las = 2, 
       cex.axis = cexAxisConstant);
  abline(h=0);
  title(main = paste("Stacking model predictive distribution,\nobservation", rownames(testSet)[testSetIndex]), 
        cex.main = cexMainConstant,
        line = -2.5)
  title(xlab = "Price", cex.lab = cexLabConstant, line = 6)
  title(ylab = "Density", cex.lab = cexLabConstant, line = 2)
}
par(mfrow=c(1,1)); 
dev.off();


#########################################################################################
# log scores?  

# estimation set 

# weights_test <- weights
# 
# weights_test[1] <- 0
# weights_test[2] <- 0
# weights_test[3] <- 0
# weights_test[4] <- 1
# weights_test[5] <- 0
# 
# posteriorPredictiveDensities.estimationset <- sapply(1:nrow(estimationSet), function(k)
#   evaluatePosteriorPredictive.combined(Price.pointEvaluation = estimationSet$Price[k],
#                                        predictiveVariables = estimationSet[k,],
#                                        
#                                        posteriorPredictiveDensities_list = listOfPosteriorPredictiveDensities,
#                                        postSample_list = listOfPosteriorSamples,
#                                        
#                                        weights = weights_test,
#                                        
#                                        likelihoodSigmaName = "sigma",
#                                        likelihoodNuName = "nu",
#                                        neighborhoodBetweenModelsHash = neighborhoodBetweenModelsHash) )
# 
# sum(log(posteriorPredictiveDensities.estimationset))

# stacking  / -45130.67
# model 1   / -48238.64
# model 2   / -46131.51
# model 3   / -46138.79
# model 4   / -46787.16
# model 5   / -45118.65



# weights_test <- weights

weights_test[1] <- 0
weights_test[2] <- 0
weights_test[3] <- 0
weights_test[4] <- 0
weights_test[5] <- 1

posteriorPredictiveDensities.testSet <- sapply(1:nrow(testSet), function(k)
  evaluatePosteriorPredictive.combined(Price.pointEvaluation = testSet$Price[k],
                                       predictiveVariables = testSet[k,],

                                       posteriorPredictiveDensities_list = listOfPosteriorPredictiveDensities,
                                       postSample_list = listOfPosteriorSamples,

                                       weights = weights_test,

                                       likelihoodSigmaName = "sigma",
                                       likelihoodNuName = "nu",
                                       neighborhoodBetweenModelsHash = neighborhoodBetweenModelsHash) )

sum(log(posteriorPredictiveDensities.testSet))

# test set 
# stacking  / -19409.36
# model 1   / -20712.11
# model 2   / -19813.57
# model 3   / -19817.1
# model 4   / -20115.93 
# model 5   / -19405.49


###########################################
# checks.. 
# stackingPredictiveDistributionSample <- sapply(1:10000, 
#                                                function(x) sampleSingleDrawFromStackingDistribution(explanatoryVariableData = testSet[635,], 
#                                                                                                     stackingWeights = weights, 
#                                                                                                     listOfPosteriorPredictiveDistSamplingFunctions = listOfPosteriorPredictiveDistSamplingFunctions, 
#                                                                                                     listOfPosteriorSamples = listOfPosteriorSamples, likelihoodSigmaName = "sigma", 
#                                                                                                     likelihoodNuName = "nu" ))
# 
# 
# 
# par(mfrow=c(2,1)); 
# hist(stackingPredictiveDistributionSample)
# abline(v=testSet[635,]$Price, col = 'red')
# abline(v = mean(stackingPredictiveDistributionSample), col = 'orange', lwd = 2)
# par(mfrow=c(1,1));



