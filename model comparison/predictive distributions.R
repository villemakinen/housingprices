
#######################
# checks for individual distributions 

rm(list=ls()); gc()

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



# control for choosen model... 
weights <- c(0,0,0.008,0.072, 0.92); 



##################
# estimation set 

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



# calculating the mass at observedPrice +- 10000 for the predictive distribution samples

massNearObsVal.estimationSet <- sapply(1:ncol(postPredDistDraws.estimation), 
       function(k) {
         # fetch observed price
         obsPrice <- estimationSet$Price[k]; 
         
         # calculate ratio 
         postPredSample <- postPredDistDraws.estimation[,k]; 
         
         ratio <- sum(abs(postPredSample - obsPrice) < 10000)/length(postPredSample)
         return(ratio); 
       }
  ); 

# cut offs for "good" and "bad" predictions
cutOff.estimationSet <- quantile(massNearObsVal.estimationSet, probs = c(0.05, 0.95))

difficultObservations.estimationSet <- (1:ncol(postPredDistDraws.estimation))[massNearObsVal.estimationSet < cutOff.estimationSet[1]]

for(k in difficultObservations.estimationSet) {
  cat("cp ../", k,".png .\n", sep="")
} 

easyObservations.estimationSet <- (1:ncol(postPredDistDraws.estimation))[massNearObsVal.estimationSet > cutOff.estimationSet[2]]


table(combinedData.orig$HouseType)


for(k in easyObservations.estimationSet) {
  cat("cp ../", k,".png .\n", sep="")
} 


###############
# test set 

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


massNearObsVal.testSet <- sapply(1:ncol(postPredDistDraws.test), 
                                       function(k) {
                                         # fetch observed price
                                         obsPrice <- testSet$Price[k]; 
                                         
                                         # calculate ratio 
                                         postPredSample <- postPredDistDraws.test[,k]; 
                                         
                                         ratio <- sum(abs(postPredSample - obsPrice) < 10000)/length(postPredSample)
                                         return(ratio); 
                                       }
); 

# cut offs for "good" and "bad" predictions
cutOff.testSet <- quantile(massNearObsVal.testSet, probs = c(0.05, 0.95))

difficultObservations.testSet <- (1:ncol(postPredDistDraws.test))[massNearObsVal.testSet < cutOff.testSet[1]]

for(k in difficultObservations.testSet) {
  cat("cp ../", k,".png .\n", sep="")
} 

easyObservations.testSet <- (1:ncol(postPredDistDraws.test))[massNearObsVal.testSet > cutOff.testSet[2]]

for(k in easyObservations.testSet) {
  cat("cp ../", k,".png .\n", sep="")
} 


##############################################
# subset checks => mitä nämä oikeasti kertoisivat?




