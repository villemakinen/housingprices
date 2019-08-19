
# model stacking with loo packagae (stacking of predictive distributions)

# approach from https://cran.r-project.org/web/packages/loo/vignettes/loo2-weights.html 

library(rstan)
library(loo)
library(LaplacesDemon)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model stacking/')

rm(list=ls()); gc(); 

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

###########
# function for combined predictive distribution

evaluatePosteriorPredictive.combined <- function(Price.pointEvaluation,  
                                                 predictiveVariables, 
                                                 
                                                 posteriorPredictiveDensities_list, 
                                                 postSample_list,
                                                 
                                                 weights,
                                                 
                                                 likelihoodSigmaName, 
                                                 likelihoodNuName) {
  
  evaluatedDensities <- rep(NA, length(weights)); 
  
  # evaluating each density
  for(k in 1:length(posteriorPredictiveDensities_list)) {
    singleDensity <- posteriorPredictiveDensities_list[[k]];
    singlePosteriorSample <- postSample_list[[k]];
    
    evaluatedDensities[k] <- singleDensity(Price.pointEvaluation = Price.pointEvaluation, 
                                           predictiveVariables = predictiveVariables, 
                                           postSample = singlePosteriorSample, 
                                           likelihoodSigmaName = likelihoodSigmaName, 
                                           likelihoodNuName = likelihoodNuName);
  }
  
 return(sum(weights*evaluatedDensities));
}

###########
# generating figures for the predictive densities 

# library(parallel)
# 
# #858
# for(k in 1:nrow(testSet)) { 
#   cat("plotting", k, "\n")
#   predictiveVariables <- testSet[k,] 
#   # predictiveVariables <-  combinedData[combinedData$NeighborhoodAssignment == 125,]
#   
#   priceGrid <- seq(predictiveVariables$Price*0.5, predictiveVariables$Price*1.5, by = 1000);
#   
#   densityValues <- mclapply(priceGrid, function(x) evaluatePosteriorPredictive.combined(Price.pointEvaluation = x, 
#                                                                                       predictiveVariables = predictiveVariables, 
#                                                                                       posteriorPredictiveDensities_list = listOfPosteriorPredictiveDensities, 
#                                                                                       postSample_list = listOfPosteriorSamples, 
#                                                                                       weights = weights, 
#                                                                                       likelihoodSigmaName = "sigma", 
#                                                                                       likelihoodNuName = "nu"),
#                             mc.cores = 4); 
#   
#   densityValues <- unlist(densityValues); 
#   
#   png(filename = paste('/home/asdf/Desktop/gradu/stacking densities/',k,'.png', sep=""), width = 1000, height = 500)
#   
#   plot(priceGrid, 
#        densityValues, 
#        type = 'l', 
#        main = paste("k: ", k, ", neighborhood: ", combinedData.orig$NeighborhoodRaw[as.numeric(rownames(predictiveVariables))], sep = ""))
#   text(x = 1.025*min(priceGrid), y = max(densityValues) , paste("Sqm = ", predictiveVariables$Sqm, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.95 , paste("CondGoodDummySqm = ", predictiveVariables$CondGoodDummySqm, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.90 , paste("Age = ", predictiveVariables$Age, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.85 , paste("TwoRoomsDummy = ", predictiveVariables$TwoRoomsDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.80 , paste("ThreeRoomsDummy = ", predictiveVariables$ThreeRoomsDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.75 , paste("FourRoomsOrMoreDummy = ", predictiveVariables$FourRoomsOrMoreDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.70 , paste("OwnFloor = ", predictiveVariables$OwnFloor, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.65 , paste("SaunaDummy = ", predictiveVariables$SaunaDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.60 , paste("HouseType = ", combinedData.orig$HouseType[as.numeric(rownames(predictiveVariables))], sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.55 , paste("ApartmentTypeRaw = ", combinedData.orig$ApartmentTypeRaw[as.numeric(rownames(predictiveVariables))], sep = ""), adj = 0)
#   
#   abline(v = predictiveVariables$Price, lwd = 2, lty = 2, col = 'red')
#   abline(v = priceGrid[which.max(densityValues)], lwd = 2, lty = 2, col = 'gray')
#   dev.off(); 
# }
# 
# 
# for(k in 1:nrow(testSet)) { 
#   cat("plotting", k, "\n")
#   predictiveVariables <- testSet[k,] 
#   # predictiveVariables <-  combinedData[combinedData$NeighborhoodAssignment == 125,]
#   
#   priceGrid <- seq(predictiveVariables$Price*0.5, predictiveVariables$Price*1.5, by = 1000);
#   
#   densityValues <- mclapply(priceGrid, function(x) evaluatePosteriorPredictive.combined(Price.pointEvaluation = x, 
#                                                                                         predictiveVariables = predictiveVariables, 
#                                                                                         posteriorPredictiveDensities_list = listOfPosteriorPredictiveDensities, 
#                                                                                         postSample_list = listOfPosteriorSamples, 
#                                                                                         weights = weights, 
#                                                                                         likelihoodSigmaName = "sigma", 
#                                                                                         likelihoodNuName = "nu"),
#                             mc.cores = 4); 
#   
#   densityValues <- unlist(densityValues); 
#   
#   png(filename = paste('/home/asdf/Desktop/gradu/stacking densities/test set/',k,'.png', sep=""), width = 1000, height = 500)
#   
#   plot(priceGrid, 
#        densityValues, 
#        type = 'l', 
#        main = paste("k: ", k, ", neighborhood: ", combinedData.orig$NeighborhoodRaw[as.numeric(rownames(predictiveVariables))], sep = ""))
#   text(x = 1.025*min(priceGrid), y = max(densityValues) , paste("Sqm = ", predictiveVariables$Sqm, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.95 , paste("CondGoodDummySqm = ", predictiveVariables$CondGoodDummySqm, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.90 , paste("Age = ", predictiveVariables$Age, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.85 , paste("TwoRoomsDummy = ", predictiveVariables$TwoRoomsDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.80 , paste("ThreeRoomsDummy = ", predictiveVariables$ThreeRoomsDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.75 , paste("FourRoomsOrMoreDummy = ", predictiveVariables$FourRoomsOrMoreDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.70 , paste("OwnFloor = ", predictiveVariables$OwnFloor, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.65 , paste("SaunaDummy = ", predictiveVariables$SaunaDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.60 , paste("HouseType = ", combinedData.orig$HouseType[as.numeric(rownames(predictiveVariables))], sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.55 , paste("ApartmentTypeRaw = ", combinedData.orig$ApartmentTypeRaw[as.numeric(rownames(predictiveVariables))], sep = ""), adj = 0)
#   
#   abline(v = predictiveVariables$Price, lwd = 2, lty = 2, col = 'red')
#   abline(v = priceGrid[which.max(densityValues)], lwd = 2, lty = 2, col = 'gray')
#   dev.off(); 
# }
# 
# 
# 
# for(k in 1:nrow(estimationSet)) { 
#   cat("plotting", k, "\n")
#   predictiveVariables <- estimationSet[k,] 
#   # predictiveVariables <-  combinedData[combinedData$NeighborhoodAssignment == 125,]
#   
#   priceGrid <- seq(predictiveVariables$Price*0.5, predictiveVariables$Price*1.5, by = 1000);
#   
#   densityValues <- mclapply(priceGrid, function(x) evaluatePosteriorPredictive.combined(Price.pointEvaluation = x, 
#                                                                                         predictiveVariables = predictiveVariables, 
#                                                                                         posteriorPredictiveDensities_list = listOfPosteriorPredictiveDensities, 
#                                                                                         postSample_list = listOfPosteriorSamples, 
#                                                                                         weights = weights, 
#                                                                                         likelihoodSigmaName = "sigma", 
#                                                                                         likelihoodNuName = "nu"),
#                             mc.cores = 4); 
#   
#   densityValues <- unlist(densityValues); 
#   
#   png(filename = paste('/home/asdf/Desktop/gradu/stacking densities/estimation set/',k,'.png', sep=""), width = 1000, height = 500)
#   
#   plot(priceGrid, 
#        densityValues, 
#        type = 'l', 
#        main = paste("k: ", k, ", neighborhood: ", combinedData.orig$NeighborhoodRaw[as.numeric(rownames(predictiveVariables))], sep = ""))
#   text(x = 1.025*min(priceGrid), y = max(densityValues) , paste("Sqm = ", predictiveVariables$Sqm, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.95 , paste("CondGoodDummySqm = ", predictiveVariables$CondGoodDummySqm, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.90 , paste("Age = ", predictiveVariables$Age, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.85 , paste("TwoRoomsDummy = ", predictiveVariables$TwoRoomsDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.80 , paste("ThreeRoomsDummy = ", predictiveVariables$ThreeRoomsDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.75 , paste("FourRoomsOrMoreDummy = ", predictiveVariables$FourRoomsOrMoreDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.70 , paste("OwnFloor = ", predictiveVariables$OwnFloor, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.65 , paste("SaunaDummy = ", predictiveVariables$SaunaDummy, sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.60 , paste("HouseType = ", combinedData.orig$HouseType[as.numeric(rownames(predictiveVariables))], sep = ""), adj = 0)
#   text(x = 1.025*min(priceGrid), y = max(densityValues)*0.55 , paste("ApartmentTypeRaw = ", combinedData.orig$ApartmentTypeRaw[as.numeric(rownames(predictiveVariables))], sep = ""), adj = 0)
#   
#   abline(v = predictiveVariables$Price, lwd = 2, lty = 2, col = 'red')
#   abline(v = priceGrid[which.max(densityValues)], lwd = 2, lty = 2, col = 'gray')
#   dev.off(); 
# }


########### 
# sampling from the stacking distributon 


grid <- seq(from = 250000, to = 900000, by = 1000)

densitiesEval <- sapply(grid, function(x) evaluatePosteriorPredictive.combined(Price.pointEvaluation = x, 
                                     predictiveVariables = testSet[635,], 
                                     posteriorPredictiveDensities_list = listOfPosteriorPredictiveDensities, 
                                     postSample_list =  listOfPosteriorSamples, 
                                     weights = weights, 
                                     likelihoodSigmaName = "sigma", 
                                     likelihoodNuName = "nu"))



sampleSingleDrawFromStackingDistribution <- function(explanatoryVariableData,
                                           
                                           stackingWeights, 
                                           listOfPosteriorPredictiveDistSamplingFunctions,
                                           listOfPosteriorSamples,
                                           
                                           likelihoodSigmaName,
                                           likelihoodNuName) {
  # choose distribution to draw from
  modelIndex <- sample(1:length(stackingWeights), size = 1, prob = stackingWeights);
  
  # draw from likelihood using the complete posterior sample   
  samplingFunction <- listOfPosteriorPredictiveDistSamplingFunctions[[modelIndex]];
  posteriorSample <- listOfPosteriorSamples[[modelIndex]];
  
  predictiveDistributionDraw <- samplingFunction(dataSet = explanatoryVariableData, 
                                                 postSample = posteriorSample, 
                                                 likelihoodSigmaName = likelihoodSigmaName, 
                                                 likelihoodNuName = likelihoodNuName)
  
  # choose one draw from predictiveDistributionDraw to "represent" a realization of the posterior distributions 
  # for the parameters which is then used for drawing from the likelihood distribution 
  realizedValue <- sample(x = predictiveDistributionDraw, size = 1)
  
  return(realizedValue)
}




stackingPredictiveDistributionSample <- sapply(1:10000, 
                                               function(x) sampleSingleDrawFromStackingDistribution(explanatoryVariableData = testSet[635,], 
                                                                                                    stackingWeights = weights, 
                                                                                                    listOfPosteriorPredictiveDistSamplingFunctions = listOfPosteriorPredictiveDistSamplingFunctions, 
                                                                                                    listOfPosteriorSamples = listOfPosteriorSamples, likelihoodSigmaName = "sigma", 
                                                                                                    likelihoodNuName = "nu" ))



par(mfrow=c(2,1)); 
plot(grid, densitiesEval)
abline(v=testSet[635,]$Price, col = 'red')

hist(stackingPredictiveDistributionSample, nclass = 500, xlim = range(grid))
abline(v=testSet[635,]$Price, col = 'red')
abline(v = mean(stackingPredictiveDistributionSample), col = 'orange', lwd = 2)
par(mfrow=c(1,1));



