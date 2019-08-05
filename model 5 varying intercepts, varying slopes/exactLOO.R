
# exact LOO for model 5 for cases with k >= 0.7 

library(rstan)
library(loo)

setwd('/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/')

rm(list=ls()); gc(); 
load(file = 'modelFit5.RData')

# getting the indeces for the problematic data points 

# exact loo performed for each index with k > 0.7 
problemIndexes <- pareto_k_ids(looObj.trueData, threshold = 0.7);

library(LaplacesDemon) # for the dst-function 

calculateLPDContribution <- function(stanFit, leftOutObservation) {
  posteriorSamples <- as.matrix(stanFit)
  
  # parameters determining the EV - group variant
  VaryingSlopes_postSample <- posteriorSamples[,grep("VaryingSlopes", colnames(posteriorSamples), value=T)];
  VaryingSlopes_postSample <- VaryingSlopes_postSample[,-grep("Mu", colnames(VaryingSlopes_postSample))];
  
  Intercept_coef_postSample <- VaryingSlopes_postSample[,grep("1\\]", colnames(VaryingSlopes_postSample), value = T)];
  Sqm_coef_postSample <- VaryingSlopes_postSample[,grep("2\\]", colnames(VaryingSlopes_postSample), value = T)];
  CondGoodSqm_coef_postSample <- VaryingSlopes_postSample[,grep("3\\]", colnames(VaryingSlopes_postSample), value = T)];
  
  # parameters determining the EV - group invariant  
  Age_coef_postSample <- posteriorSamples[,grep("Age_coef", colnames(posteriorSamples), value=T)];
  TwoRoomsDummy_coef_postSample <- posteriorSamples[,grep("TwoRoomsDummy_coef", colnames(posteriorSamples), value=T)];
  ThreeRoomsDummy_coef_postSample <- posteriorSamples[,grep("ThreeRoomsDummy_coef", colnames(posteriorSamples), value=T)];
  FourRoomsOrMoreDummy_coef_postSample <- posteriorSamples[,grep("FourRoomsOrMoreDummy_coef", colnames(posteriorSamples), value=T)];
  SaunaDummy_coef_postSample <- posteriorSamples[,grep("SaunaDummy_coef", colnames(posteriorSamples), value=T)];
  OwnFloor_coef_postSample <- posteriorSamples[,grep("OwnFloor_coef", colnames(posteriorSamples), value=T)];
  
  # variance parameters
  sigma_postSample <- posteriorSamples[,grep("sigma$", colnames(posteriorSamples), value=T, perl = T)];
  nu_postSample <- posteriorSamples[,grep("nu", colnames(posteriorSamples), value=T)];
  
  EV <- Intercept_coef_postSample[,leftOutObservation$NeighborhoodAssignment] +
          Sqm_coef_postSample[,leftOutObservation$NeighborhoodAssignment]*leftOutObservation$Sqm +
          CondGoodSqm_coef_postSample[,leftOutObservation$NeighborhoodAssignment]*leftOutObservation$CondGoodDummySqm +
          Age_coef_postSample*leftOutObservation$Age + 
          TwoRoomsDummy_coef_postSample*leftOutObservation$TwoRoomsDummy + 
          ThreeRoomsDummy_coef_postSample*leftOutObservation$ThreeRoomsDummy +
          FourRoomsOrMoreDummy_coef_postSample*leftOutObservation$FourRoomsOrMoreDummy +
          SaunaDummy_coef_postSample*leftOutObservation$SaunaDummy + 
          OwnFloor_coef_postSample*leftOutObservation$OwnFloor;
  
  return(log((1/length(EV))*sum(dst(x = leftOutObservation$Price, mu = EV, sigma = sigma_postSample, nu = nu_postSample))));
}

k <- 1; 
resultList <- list(); 

for(singleIndex in problemIndexes) {
  message("*********** k ", k, " of ", length(problemIndexes))
  
  # creating the data
  
  estimationSet.minusI <- estimationSet[-singleIndex,]; 
  
  # building a separate model 
  stanFit.trueData.minusI <- sampling(object = model5.stanObj, 
                                           data = list(N = nrow(estimationSet.minusI), 
                                                       N_neighborhood = max(combinedData$NeighborhoodAssignment),
                                                       Price = estimationSet.minusI$Price, 
                                                       Sqm = estimationSet.minusI$Sqm,
                                                       CondGoodDummySqm = estimationSet.minusI$CondGoodDummySqm,
                                                       Age = estimationSet.minusI$Age,
                                                       TwoRoomsDummy = estimationSet.minusI$TwoRoomsDummy,
                                                       ThreeRoomsDummy = estimationSet.minusI$ThreeRoomsDummy, 
                                                       FourRoomsOrMoreDummy = estimationSet.minusI$FourRoomsOrMoreDummy,
                                                       OwnFloor = estimationSet.minusI$OwnFloor,
                                                       SaunaDummy = estimationSet.minusI$SaunaDummy, 
                                                       NeighborhoodAssignment = estimationSet.minusI$NeighborhoodAssignment), 
                                           iter = 4000,
                                           cores = 4,
                                           seed = 1234);
  
  # calculating the elpd contribution for the problematic points 
  calculated_lpd_loo <- calculateLPDContribution(stanFit = stanFit.trueData.minusI, 
                                                 leftOutObservation = estimationSet[singleIndex,]);
  
  # saving the results 
  resultList[[k]] <- calculated_lpd_loo;

  k <- k + 1; 
}


# calculating the PSIS-LOO+-version of elpd for model comparison etc. 

looObj.trueData

sum(looObj.trueData$pointwise[,"elpd_loo"])

psisLooPlus.pointwiseELPD <- looObj.trueData$pointwise[,"elpd_loo"]

psisLooPlus.pointwiseELPD[problemIndexes] <- unlist(resultList)

sum(psisLooPlus.pointwiseELPD)

# used for stacking later 
pointwiseElpdLooVectorForStacking <- psisLooPlus.pointwiseELPD; 

save.image("PSISLOOfix.RData");
