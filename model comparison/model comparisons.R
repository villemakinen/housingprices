

# model comparisons

# LOO-PIT?
# elpd-plotting
# Pareto k ploting 

library(rstan)
library(loo)
library(hash); 

setwd('/home/asdf/Desktop/gradu/git/housingprices/model comparison/')

rm(list=ls()); gc(); 

############################################
# plotting large figures to be used for model comparisons

modelFitFilenames <- c('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/modelFit1.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 2 simple varying intercept model/modelFit2.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/modelFit3.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/modelFit4.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/modelFit5.RData' # 
); 

listOfReplicatedMeansData <- list(); 
listOfReplicatedMediansData <- list(); 

listOfReplicatedMeansHelsinkiNeighborhoodData <- list();
listOfReplicatedMeansEspooNeighborhoodData <- list();
listOfReplicatedMeansVantaaNeighborhoodData <- list();

listOfPIThistogramsData <- list(); 
listOfPredictiveDistributionWidthsData <- list(); 

counter <- 1; 
for(filename in modelFitFilenames) {
  cat("handling", filename, "...\n"); 
  load(filename)
  
  set.seed(123); 
  
  dataReplications <- getPosteriorPredictiveDraws(dataSet = estimationSet, 
                                                  postSample = posteriorSamples.trueData, 
                                                  likelihoodSigmaName = "sigma", 
                                                  likelihoodNuName = "nu")
  
  # replicated means for estimation set
  replicatedPrices.mean <- apply(dataReplications, 1, mean);
  listOfReplicatedMeansData[[counter]] <- replicatedPrices.mean;

  # replicated medians for estimation set
  replicatedPrices.median <- apply(dataReplications, 1, median);
  listOfReplicatedMediansData[[counter]] <- replicatedPrices.median;
  

  # replicated means for each city
  estimationSetNeighborhoods <- combinedData.orig$NeighborhoodFinalized[-testSetIndeces]
  
  replicatedMeansPerNeighborhood <- apply(X = dataReplications, 
                                          MARGIN = 1, 
                                          FUN = function(rivi) tapply(X = rivi, INDEX = estimationSetNeighborhoods, mean))
  
  estimationSetMeansPerNeighborhood <- tapply(X = estimationSet$Price, INDEX = estimationSetNeighborhoods, mean);
  
  neighborhoodNames <- names(estimationSetMeansPerNeighborhood);
  
  if(counter == 3) {
    # a hack to fix the neighborhood names for model 3
    substring(neighborhoodNames,1,1) <- toupper(substr(neighborhoodNames,1,1)) 
    neighborhoodNames[neighborhoodNames == "Etu-töölö"] <- "Etu-Töölö";
    neighborhoodNames[neighborhoodNames == "Pohjois-tapiola"] <- "Pohjois-Tapiola";
    neighborhoodNames[neighborhoodNames == "Taka-töölö"] <- "Taka-Töölö";
    neighborhoodNames[neighborhoodNames == "Itä-hakkila"] <- "Itä-Hakkila";
    
    rownames(replicatedMeansPerNeighborhood) <- neighborhoodNames; 
    names(estimationSetMeansPerNeighborhood) <- neighborhoodNames;
  }
  
  neighborhoodToCityHash <- hash(keys = c('Alppiharju','Askisto','Asola','Eira','Espoon keskus','Espoonlahti','Etu-Töölö','Haaga','Hakunila','Hämeenkylä','Hämevaara','Haukilahti','Havukoski','Henttaa','Hermanni','Herttoniemi','Hiekkaharju','Ilola','Itä-Hakkila','Järvenperä','Jokiniemi','Kaarela','Kaartinkaupunki','Kaitaa','Kaivoksela','Kallio','Kamppi','Käpylä','Karakallio','Karhusuo','Karvasmäki','Katajanokka','Kauklahti','Kaupunginkallio','Keimola','Kilo','Kivistö','Kluuvi','Koivuhaka','Koivukylä','Kolmperä','Konala','Korso','Koskela','Kruununhaka','Kulosaari','Kumpula','Kuninkaala','Kuninkaanmäki','Kuurinniitty','Laajalahti','Laajasalo','Laakso','Laaksolahti','Lahnus','Länsimäki','Länsisatama','Latokaski','Lauttasaari','Leppäkorpi','Leppävaara','Lintuvaara','Lippajärvi','Malmi','Mankkaa','Martinlaakso','Matari','Matinkylä','Meilahti','Mellunkylä','Metsola','Mikkola','Munkkiniemi','Muurala','Myyrmäki','Niipperi','Niittykumpu','Nikinmäki','Nöykkiö','Nupuri','Olari','Otaniemi','Oulunkylä','Päiväkumpu','Pakila','Pakkala','Pasila','Perusmäki','Piispankylä','Pitäjänmäki','Pohjois-Tapiola','Pukinmäki','Punavuori','Rajakylä','Rekola','Ruskeasanta','Ruskeasuo','Saunalahti','Sepänkylä','Simonkylä','Sörnäinen','Soukka','Suurmetsä','Suutarila','Taka-Töölö','Tammisalo','Tammisto','Tapaninkylä','Tapiola','Tikkurila','Toukola','Tuomarinkylä','Ullanlinna','Vaarala','Vallila','Vanhakaupunki','Vantaanlaakso','Vanttila','Vapaala','Varisto','Vartiokylä','Viertola','Vierumäki','Viherlaakso','Viikki','Vuosaari','Westend','Ylästö'),
                                 values = c('Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Helsinki','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Espoo','Espoo','Espoo','Helsinki','Espoo','Espoo','Vantaa','Espoo','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Vantaa','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Vantaa','Espoo','Espoo','Helsinki','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Vantaa','Espoo','Espoo','Espoo','Helsinki','Espoo','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Vantaa','Espoo','Espoo','Vantaa','Espoo','Espoo','Espoo','Espoo','Helsinki','Vantaa','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Helsinki','Vantaa','Espoo','Vantaa','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Espoo','Vantaa')) 
  
  cityAssignments <- sapply(rownames(replicatedMeansPerNeighborhood), function(x) neighborhoodToCityHash[[x]])
  
  replicatedMeansPerNeighborhood.Helsinki <- replicatedMeansPerNeighborhood[cityAssignments == "Helsinki",]
  replicatedMeansPerNeighborhood.Espoo <- replicatedMeansPerNeighborhood[cityAssignments == "Espoo",]
  replicatedMeansPerNeighborhood.Vantaa <- replicatedMeansPerNeighborhood[cityAssignments == "Vantaa",]
  
  listOfReplicatedMeansHelsinkiNeighborhoodData[[counter]] <- replicatedMeansPerNeighborhood.Helsinki;
  listOfReplicatedMeansEspooNeighborhoodData[[counter]] <- replicatedMeansPerNeighborhood.Espoo;
  listOfReplicatedMeansVantaaNeighborhoodData[[counter]] <- replicatedMeansPerNeighborhood.Vantaa;
  
  # estimation set draws from predictive distribution
  set.seed(123); 
  postPredDistDraws.estimation <- getPosteriorPredictiveDraws(dataSet = estimationSet,
                                                              postSample = posteriorSamples.trueData,
                                                              likelihoodSigmaName = "sigma",
                                                              likelihoodNuName = "nu")
  
  # PIT histograms
  PITsample.estimation <- sapply(1:nrow(estimationSet), function(k) mean(postPredDistDraws.estimation[,k] <= estimationSet$Price[k]))
  listOfPIThistogramsData[[counter]] <- PITsample.estimation;
  
  # predictive distribution widths
  credibleIntervalWidths.90.estimation <- apply(X = postPredDistDraws.estimation, 
                                                MARGIN = 2, 
                                                function(otos) quantile(x = otos, probs =  0.95) - quantile(x = otos, probs =  0.05));
  listOfPredictiveDistributionWidthsData[[counter]] <- credibleIntervalWidths.90.estimation; 
  
  counter <- counter + 1;   
}

# joint figure for the replicated means over whole data
cexAxisConstant <- 2; 
cexLabConstant <- 2;
cexMainConstant <- 2; 

png('./figures/replicatedMeansAllModels.png', width = 1200, height = 1200)
par(mfrow=c(5,1));
par(mar = c(5,10,4,2) + 0.1)
for(k in 1:length(listOfReplicatedMeansData)) {
  singleDataSet <- listOfReplicatedMeansData[[k]];
  minPlottedValue <- 230000; 
  maxPlottedValue <- 290000;
  
  singleDataSet.censored <- singleDataSet;
  singleDataSet.censored <- singleDataSet.censored[singleDataSet.censored >= minPlottedValue];
  singleDataSet.censored <- singleDataSet.censored[singleDataSet.censored <= maxPlottedValue];
  
  mainText <- paste("Replicated means, model ", k, 
                    ", plotted with ", length(singleDataSet.censored), "/", length(singleDataSet)," replications",  sep ="")
  hist(singleDataSet.censored, 
       xlim = c(minPlottedValue,maxPlottedValue),
       ylim = c(0,3000),
       xlab = "",
       ylab = "",
       main = mainText,
       nclass=42,
       cex.axis = cexAxisConstant, 
       cex.lab = cexLabConstant,
       cex.main = cexMainConstant,
       axes=F)
  axis(side = 1,
       at = seq(from = minPlottedValue, to = maxPlottedValue, by = 10000),
       cex.axis = cexAxisConstant)
  axis(side = 2,
       at = seq(from = 0, to = 3000, by = 500),
       cex.axis = cexAxisConstant,
       las = 2)
  title(ylab = "Frequency", cex.lab = cexLabConstant, line = 7)
  abline(v = mean(estimationSet$Price), col = 'red', lwd = 2.5, lty = 2)
  #abline(h = 0)
}
par(mfrow=c(1,1));
par(mar = c(5,4,4,2) + 0.1)
dev.off()


# joint figure for the replicated medians over whole data
cexAxisConstant <- 2; 
cexLabConstant <- 2;
cexMainConstant <- 2; 

png('./figures/replicatedMediansAllModels.png', width = 1200, height = 1200)
par(mfrow=c(5,1));
par(mar = c(5,10,4,2) + 0.1)
for(k in 1:length(listOfReplicatedMediansData)) {
  singleDataSet <- listOfReplicatedMediansData[[k]];
  minPlottedValue <- 225000;
  maxPlottedValue <- 265000;
  
  singleDataSet.censored <- singleDataSet;
  singleDataSet.censored <- singleDataSet.censored[singleDataSet.censored >= minPlottedValue];
  singleDataSet.censored <- singleDataSet.censored[singleDataSet.censored <= maxPlottedValue];
  
  mainText <- paste("Replicated medians, model ", k, 
                    ", plotted with ", length(singleDataSet.censored), "/", length(singleDataSet)," replications",  sep ="")
  hist(singleDataSet.censored, 
       xlim = c(minPlottedValue,maxPlottedValue),
       ylim = c(0,1000),
       xlab = "",
       ylab = "",
       main = mainText,
       nclass=30,
       cex.axis = cexAxisConstant, 
       cex.lab = cexLabConstant,
       cex.main = cexMainConstant
       ,   axes=F
       )
  axis(side = 1,
       at = seq(from = minPlottedValue, to = maxPlottedValue, by = 5000),
       cex.axis = cexAxisConstant)
  axis(side = 2,
       at = seq(from = 0, to = 1000, by = 250),
       cex.axis = cexAxisConstant,
       las = 2)
  title(ylab = "Frequency", cex.lab = cexLabConstant, line = 7)
  abline(v = median(estimationSet$Price), col = 'red', lwd = 2.5, lty = 2)
  #abline(h = 0)
}
par(mfrow=c(1,1));
par(mar = c(5,4,4,2) + 0.1)
dev.off()


# graphs for mean prices per neighborhood

observerdMeans.Helsinki <- estimationSetMeansPerNeighborhood[names(estimationSetMeansPerNeighborhood) %in% rownames(replicatedMeansPerNeighborhood.Helsinki)]
observerdMeans.Espoo <- estimationSetMeansPerNeighborhood[names(estimationSetMeansPerNeighborhood) %in% rownames(replicatedMeansPerNeighborhood.Espoo)]
observerdMeans.Vantaa <- estimationSetMeansPerNeighborhood[names(estimationSetMeansPerNeighborhood) %in% rownames(replicatedMeansPerNeighborhood.Vantaa)]

cexAxisConstant <- 2; 
cexLabConstant <- 2;
cexMainConstant <- 1.8; 

def.par <- par(no.readonly = TRUE) # save default, for resetting...

#### Helsinki
png('./figures/replicatedMeanPricesHelsinkiAllModels.png', width = 1200, height = 1200)

nf <- layout(mat = matrix(1:5, ncol=1), 
             widths = c(1,1,1,1,1),
             heights = c(1,1,1,1,1.6))

par(mar = c(0.25, # bottom
            10.1, #left
            0.25, 0.8))

for(k in 1:(length(listOfReplicatedMeansHelsinkiNeighborhoodData)-1)) {
  singleDataSet <- listOfReplicatedMeansHelsinkiNeighborhoodData[[k]]; 
  boxplot(t(singleDataSet), 
          outline=F, 
          ylim = c(100000, 750000), axes=F,
          main = "",
          cex.main = cexMainConstant)
  title(main = paste("Neighborhood replicated mean price box plots for Helsinki, model ",k, sep=""), 
        line = -2,
        cex.main = cexMainConstant)
  points(observerdMeans.Helsinki, col = 'red', pch = 4, cex = 1.5, lwd = 2);
  
  axis(side = 2, 
       round(seq(from = 100000, to = 750000, length.out = 5)), 
       las = 2,
       cex.axis = cexAxisConstant)
  
  abline(v = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), 
         lty = 3, 
         lwd = 0.85, 
         col = alpha('gray', 0.95))
  # 
  # abline(h = 0, 
  #        lty = 2, 
  #        lwd = 0.85, 
  #        col = 'black')
}

par(mar = c(13.1, # bottom 
            10.1, #left
            0.25, 0.8))

# last model added separately to add the x-axis labels, layout guarantees that the 

singleDataSet <- listOfReplicatedMeansHelsinkiNeighborhoodData[[k+1]]; 
boxplot(t(singleDataSet), 
        outline=F, 
        ylim = c(100000, 750000), axes=F,
        main = "",
        cex.main = cexMainConstant)
title(main = paste("Neighborhood replicated mean price box plots for Helsinki, model ",k+1, sep=""), 
      line = -2,
      cex.main = cexMainConstant)
points(observerdMeans.Helsinki, col = 'red', pch = 4, cex = 1.5, lwd = 2);

axis(side = 1,
     at = 1:nrow(replicatedMeansPerNeighborhood.Helsinki),
     labels = rownames(replicatedMeansPerNeighborhood.Helsinki),
     las = 2,
     pch = 0.8,
     cex.axis = cexAxisConstant)
axis(side = 2, 
     round(seq(from = 100000, to = 750000, length.out = 5)), 
     las = 2,
     cex.axis = cexAxisConstant)

abline(v = 1:nrow(replicatedMeansPerNeighborhood.Helsinki), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.95))
# abline(h = 0, 
#        lty = 2, 
#        lwd = 0.85, 
#        col = 'black')
dev.off()

par(def.par)

#### Espoo
png('./figures/replicatedMeanPricesEspooAllModels.png', width = 1200, height = 1200)

nf <- layout(mat = matrix(1:5, ncol=1), 
             widths = c(1,1,1,1,1),
             heights = c(1,1,1,1,1.6))

par(mar = c(0.25, # bottom
            10.1, #left
            0.25, 0.8))

for(k in 1:(length(listOfReplicatedMeansEspooNeighborhoodData)-1)) {
  singleDataSet <- listOfReplicatedMeansEspooNeighborhoodData[[k]]; 
  boxplot(t(singleDataSet), 
          outline=F, 
          ylim = c(25000, 750000), axes=F,
          main = "",
          cex.main = cexMainConstant)
  title(main = paste("Neighborhood replicated mean price box plots for Espoo, model ",k, sep=""), 
        line = -2, 
        cex.main = cexMainConstant)
  points(observerdMeans.Espoo, col = 'red', pch = 4, cex = 1.5, lwd = 2);
  
  axis(side = 2, 
       round(seq(from = 50000, to = 750000, length.out = 5)), 
       las = 2,
       cex.axis = cexAxisConstant)
  # abline(h = 0, 
  #        lty = 2, 
  #        lwd = 0.85, 
  #        col = 'black')
  abline(v = 1:nrow(replicatedMeansPerNeighborhood.Espoo), 
         lty = 3, 
         lwd = 0.85, 
         col = alpha('gray', 0.95))
}

par(mar = c(13.1, # bottom 
            10.1, #left
            0.25, 0.8))

# last model added separately to add the x-axis labels, layout guarantees that the 

singleDataSet <- listOfReplicatedMeansEspooNeighborhoodData[[k+1]]; 
boxplot(t(singleDataSet), 
        outline=F, 
        ylim = c(25000, 750000), axes=F,
        main = "",
        cex.main = cexMainConstant)
title(main = paste("Neighborhood replicated mean price box plots for Espoo, model ", k + 1, sep=""), 
      line = -2,
      cex.main = cexMainConstant)
points(observerdMeans.Espoo, col = 'red', pch = 4, cex = 1.5, lwd = 2);

axis(side = 1,
     at = 1:nrow(replicatedMeansPerNeighborhood.Espoo),
     labels = rownames(replicatedMeansPerNeighborhood.Espoo),
     las = 2,
     pch = 0.8,
     cex.axis = cexAxisConstant)
axis(side = 2, 
     round(seq(from = 50000, to = 750000, length.out = 5)), 
     las = 2,
     cex.axis = cexAxisConstant)
# abline(h = 0, 
#        lty = 2, 
#        lwd = 0.85, 
#        col = 'black')
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Espoo), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.95))

dev.off()

par(def.par)


#### Vantaa
png('./figures/replicatedMeanPricesVantaaAllModels.png', width = 1200, height = 1200)

nf <- layout(mat = matrix(1:5, ncol=1), 
             widths = c(1,1,1,1,1),
             heights = c(1,1,1,1,1.6))

par(mar = c(0.25, # bottom
            10.1, #left
            0.25, 1.8))

for(k in 1:(length(listOfReplicatedMeansVantaaNeighborhoodData)-1)) {
  singleDataSet <- listOfReplicatedMeansVantaaNeighborhoodData[[k]]; 
  boxplot(t(singleDataSet), 
          outline=F, 
          ylim = c(-10000, 600000), axes=F,
          main = "",
          cex.main = cexMainConstant)
  title(main = paste("Neighborhood replicated mean price box plots for Vantaa, model ",k, sep=""), 
        line = -2, 
        cex.main = cexMainConstant)
  points(observerdMeans.Vantaa, col = 'red', pch = 4, cex = 1.5, lwd = 2);
  
  axis(side = 2, 
       round(seq(from = -10000, to = 600000, length.out = 5)), 
       las = 2,
       cex.axis = cexAxisConstant)
  axis(side = 4, 
       0, 
       las = 2,
       cex.axis = cexAxisConstant/2)
  abline(h = 0, 
         lty = 2, 
         lwd = 0.85, 
         col = 'black')
  abline(v = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), 
         lty = 3, 
         lwd = 0.85, 
         col = alpha('gray', 0.95))
}

par(mar = c(13.1, # bottom 
            10.1, #left
            0.25, 1.8))

# last model added separately to add the x-axis labels, layout guarantees that the 

singleDataSet <- listOfReplicatedMeansVantaaNeighborhoodData[[k+1]]; 
boxplot(t(singleDataSet), 
        outline=F, 
        ylim = c(-10000, 600000), axes=F,
        main = "",
        cex.main = cexMainConstant)
title(main = paste("Neighborhood replicated mean price box plots for Vantaa, model ",k+1, sep=""), 
      line =-2, 
      cex.main = cexMainConstant)
points(observerdMeans.Vantaa, col = 'red', pch = 4, cex = 1.5, lwd = 2);

axis(side = 1,
     at = 1:nrow(replicatedMeansPerNeighborhood.Vantaa),
     labels = rownames(replicatedMeansPerNeighborhood.Vantaa),
     las = 2,
     pch = 0.8,
     cex.axis = cexAxisConstant)
axis(side = 2, 
     round(seq(from = -10000, to = 600000, length.out = 5)), 
     las = 2,
     cex.axis = cexAxisConstant)
axis(side = 4, 
     0, 
     las = 2,
     cex.axis = cexAxisConstant/2)
abline(h = 0, 
       lty = 2, 
       lwd = 0.85, 
       col = 'black')
abline(v = 1:nrow(replicatedMeansPerNeighborhood.Vantaa), 
       lty = 3, 
       lwd = 0.85, 
       col = alpha('gray', 0.95))

dev.off()

par(def.par)


# joint figure for the PIT histograms

png('./figures/PITHistograms.png', width = 1200, height = 1200)

cexMainConstant <- 2; 
cexLabConstant <- 2; 
cexAxisConstant <- 2; 
par(mfrow=c(5,1));
par(mar = c(5,10,4,2) + 0.1)
for(k in 1:length(listOfPIThistogramsData)) {
  singleDataSet <- listOfPIThistogramsData[[k]]; 
  
  helpHistogram <- hist(singleDataSet, 
                        nclass = 20,
                        plot = F)
  
  hist(singleDataSet, 
       xlab = "Probability Integral Transform", 
       ylab = "",
       main = "",
       nclass = 20,
       probability = T,
       axes = F,
       ylim = c(0,max(helpHistogram$density)),
       cex.lab = cexLabConstant
  )
  axis(side = 1,
       at = seq(from = 0, to = 1, length.out = 5),
       cex.axis = cexAxisConstant);
  
  axis(side = 2,
       at = round(seq(from = 0, to = max(helpHistogram$density), length.out = 5), 2),
       las = 2,
       cex.axis = cexAxisConstant);
  axis(side = 4,
       at = 1,
       las = 2,
       cex.axis = cexAxisConstant*0.8);
  abline(h = 1, lty = 2)
  title(ylab = "Density", cex.lab = cexLabConstant, line = 6)
  title(main = paste("PIT histogram, model", k), 
        cex.main = cexMainConstant,
        line = 0.1)
}
par(mfrow=c(1,1));
par(mar = c(5,4,4,2) + 0.1)

dev.off()

# joint figure for the predictive distribution widths

cexMainConstant <- 2; 
cexLabConstant <- 2; 
cexAxisConstant <- 2; 

png('./figures/SharpnessHistograms.png', width = 1200, height = 1200)

par(mfrow=c(5,1));
par(mar = c(5,10,4,2) + 0.1)

nClassVec <- c(20, 50, 40, 40, 60);
for(k in 1:length(listOfPredictiveDistributionWidthsData)) {
  singleDataSet <- listOfPredictiveDistributionWidthsData[[k]]; 
  
  helpHistogram <- hist(singleDataSet, 
                        nclass = nClassVec[k],
                        plot = F)
  
  hist(singleDataSet,
       nclass = nClassVec[k], 
       xlim = c(140000,350000),
       xlab = "90 % credible interval width", 
       ylab = "", 
       main = "", 
       axes = F, 
       probability = T,
       ylim = c(0,0.000125),
       cex.lab = cexLabConstant);
  
  axis(side = 1,
       at = seq(from = 140000, to = 350000, length.out = 5),
       cex.axis = cexAxisConstant);
  
  axis(side = 2,
       at = c(0, 0.000125), #seq(from = 0, to = 0.000125, length.out = 5),
       labels = c("0", "1.25e-4"),
       las = 2,
       cex.axis = cexAxisConstant);
  
  title(ylab = "Density", cex.lab = cexLabConstant, line = 6)
  title(main = paste("Sharpness histogram, 90 % credible interval widths, model", k), 
        cex.main = cexMainConstant,
        line = -1.5)
}
par(mfrow=c(1,1));
par(mar = c(5,4,4,2) + 0.1)

dev.off()


############################################
# plotting large figures to be used for model comparisons - scatter plots and predictive distributions 

# rajoitetaan "vaikeat" testijoukon havaintoihin, joiden nimet 3084/tapahtuu parannusta, 2748/tapahtuu parannusta, 2753/ei parannusta, 3089/ei parannusta



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
  set.seed(123);
  # estimation set draws from predictive distribution
  postPredDistDraws.estimation <- getPosteriorPredictiveDraws(dataSet = estimationSet,
                                                              postSample = posteriorSamples.trueData,
                                                              likelihoodSigmaName = "sigma",
                                                              likelihoodNuName = "nu")
  
  listOfPosteriorPredictiveDistributionsEstimationSet[[counter]] <- postPredDistDraws.estimation;
  
  # test set draws from predictive distribution
  postPredDistDraws.test <- getPosteriorPredictiveDraws(dataSet = testSet,
                                                        postSample = posteriorSamples.trueData,
                                                        likelihoodSigmaName = "sigma",
                                                        likelihoodNuName = "nu")
  
  listOfPosteriorPredictiveDistributionsTestSet[[counter]] <- postPredDistDraws.test;
  
  counter <- counter + 1; 
}

# joint figures for estimation set scatters 

cexMainConstant <- 1.8; 
cexLabConstant <- 1.7; 
cexAxisConstant <- 1.5; 

png('./figures/EstimationSetScatters.png', width = 1200, height = 1200)

#par(mfrow=c(5,1));

nf <- layout(mat = matrix(c(1,1,2,2,
                            3,3,4,4,
                            0,5,5,0), ncol=4, nrow = 3, byrow=T), 
             widths = c(1,1),
             heights = c(1,1,1))

par(mar = c(5,10,4,2) + 0.1)
for(k in 1:length(listOfPosteriorPredictiveDistributionsEstimationSet)) {
  singleDataSet <- apply(listOfPosteriorPredictiveDistributionsEstimationSet[[k]], 2, mean) 
  
  plot(estimationSet$Price, 
       singleDataSet,
       xlab = "Observed price",
       #xlab = "", 
       #ylab = "mean of price predictive distribution",
       ylab = "",
       #main = "True price vs. mean of predictive distributions\nestimation set"
       main = "",
       ylim = c(0, 1800000),
       xlim = c(0, 2500000), 
       axes= F,
       cex.lab = cexLabConstant)
  
  axis(side = 1,
       at = seq(from = 0, to = 2500000, length.out = 6),
       cex.axis = cexAxisConstant);
  
  axis(side = 2,
       at = seq(from = 0, to = 1800000, length.out = 7),
       las = 2,
       cex.axis = cexAxisConstant);
  
  title(ylab = "Mean of predictive\ndistribution draws", cex.lab = cexLabConstant, line = 6.5)
  
  title(main = paste("Estimation set price scatter plot, model", k), 
        cex.main = cexMainConstant,
        line = 0.5)

  abline(a = 0, b = 1, lty = 2, col = 'red')
}
par(mfrow=c(1,1));
par(mar = c(5,4,4,2) + 0.1)

dev.off()


# joint figures for test set scatter plot

png('./figures/TestSetScatters.png', width = 1200, height = 1200)

nf <- layout(mat = matrix(c(1,1,2,2,
                            3,3,4,4,
                            0,5,5,0), ncol=4, nrow = 3, byrow=T), 
             widths = c(1,1),
             heights = c(1,1,1))
for(k in 1:length(listOfPosteriorPredictiveDistributionsTestSet)) {
  predictiveDistributionDraw.all <- listOfPosteriorPredictiveDistributionsTestSet[[k]];
  for(l  in 1:length(problematicObservations)) {
    targetObs <- problematicObservations[l]; 
    predictiveDistributionDraw.target <- predictiveDistributionDraw.all[,targetObs]
    
    # removing 0.1 % extreme values from both sides
    valueVector <- predictiveDistributionDraw.target[predictiveDistributionDraw.target <= quantile(predictiveDistributionDraw.target, probs = 1 - 0.001) & predictiveDistributionDraw.target >= quantile(predictiveDistributionDraw.target, probs = 0.001)]
    
    
    truePrice <- testSet$Price[targetObs]
    
    hist(valueVector, 
         xlim = xLimitMatrix[l,],
         ylim = c(0, 10e-6),
         main = paste("model", k, "predictive distribution", "\nobservation", rownames(testSet)[targetObs]),
         probability = T,
         nclass = nClassMatrix[k,l],
         xlab = "Price");
    abline(v = truePrice, lty = 2, col = 'red')
    abline(h=0);
  }  
  
par(mar = c(5,10,4,2) + 0.1)
for(k in 1:length(listOfPosteriorPredictiveDistributionsTestSet)) {
  singleDataSet <- apply(listOfPosteriorPredictiveDistributionsTestSet[[k]], 2, mean) 
  
  plot(testSet$Price, 
       singleDataSet,
       xlab = "Observed price",
       #xlab = "", 
       #ylab = "mean of price predictive distribution",
       ylab = "",
       #main = "True price vs. mean of predictive distributions\nestimation set"
       main = "",
       ylim = c(0, 1500000),
       xlim = c(0, 2500000), 
       axes= F,
       cex.lab = cexLabConstant)
  
  axis(side = 1,
       at = seq(from = 0, to = 2500000, length.out = 6),
       cex.axis = cexAxisConstant);
  
  axis(side = 2,
       at = seq(from = 0, to = 1500000, length.out = 6),
       las = 2,
       cex.axis = cexAxisConstant);
  
  title(ylab = "Mean of predictive\ndistribution draws", cex.lab = cexLabConstant, line = 6.5)
  
  title(main = paste("Test set price scatter plot, model", k), 
        cex.main = cexMainConstant,
        line = 0.5)
  
  abline(a = 0, b = 1, lty = 2, col = 'red')
  
  problematicObservations <- c(259, 1290, 200, 63)
  
  points(testSet$Price[problematicObservations], 
         singleDataSet[problematicObservations],
         col = 'darkgreen',
         lwd = 1.1)
  
  with(testSet[problematicObservations,], text(testSet$Price[problematicObservations], 
                                               singleDataSet[problematicObservations],
                                               labels = rownames(testSet)[problematicObservations],
                                               pos = 1, 
                                               cex = 1.4, 
                                               col = 'darkgreen'))
}
par(mfrow=c(1,1));
par(mar = c(5,4,4,2) + 0.1)

dev.off()

### predictive distributions per model per highlighted observations 


png('./figures/PredictiveDistributionsChosenObs.png', width = 1200, height = 1200)

cexLabConstant <- 1.5; 
cexAxisConstant <- 1.5; 
cexMainConstant <- 1.8;

problematicObservations <- c(259, 1290, 200, 63);

xLimitMatrix <- matrix(c(0, 2500000,
                         0, 1800000,
                         0, 1800000,
                         0, 1400000), nrow = 4, ncol = 2, byrow = T)

yLimitMatrix <- matrix(c(0, 7e-6,
                         0, 10e-6,
                         0, 8e-6,
                         0, 9e-6), nrow = 4, ncol = 2, byrow = T)



nClassMatrix <- matrix(c(25,25, 25, 25,
                         25,25, 25, 25,
                         25,20, 20, 20,
                         30,35, 35, 30,
                         20,20, 20, 20), nrow = 5, ncol = 4, byrow = T)


#par(mfrow=c(5,4)); 


nf <- layout(mat = matrix(c(1,2,3,4,
                            5,6,7,8,
                            9,10,11,12,
                            13,14,15,16,
                            17,18,19,20), ncol=4, nrow = 5, byrow=T), 
             widths = c(1.5,1,1,1),
             heights = c(1,1,1,1,1.5))


for(k in 1:length(listOfPosteriorPredictiveDistributionsTestSet)) {
  predictiveDistributionDraw.all <- listOfPosteriorPredictiveDistributionsTestSet[[k]];
  for(l  in 1:length(problematicObservations)) {
    targetObs <- problematicObservations[l]; 
    predictiveDistributionDraw.target <- predictiveDistributionDraw.all[,targetObs]
    
    # removing 0.1 % extreme values from both sides
    valueVector <- predictiveDistributionDraw.target[predictiveDistributionDraw.target <= quantile(predictiveDistributionDraw.target, probs = 1 - 0.001) & predictiveDistributionDraw.target >= quantile(predictiveDistributionDraw.target, probs = 0.001)]
    
    
    truePrice <- testSet$Price[targetObs]
    
    otherMargin <- 2.5; 
    bottomMargin <- 12;
    leftMargin <- 9; 
    
    if(l == 1 & k == 5) {
      # lower left => margins for bottom and left
      par(mar = c(bottomMargin, # bottom 
                  leftMargin, #left
                  otherMargin, otherMargin))
    } else if(l == 1 & k != 5) {
      # other left side => margins on left
      par(mar = c(otherMargin, # bottom 
                  leftMargin, #left
                  otherMargin, otherMargin))
    } else if(l != 1 & k == 5) {
      # other bottom => margins on bottom
      par(mar = c(bottomMargin, # bottom 
                  otherMargin, #left
                  otherMargin, otherMargin))
    } else {
      # internal => minimal margins 
      par(mar = c(otherMargin,otherMargin,otherMargin,otherMargin))
    }
    
    
    hist(valueVector, 
         xlim = xLimitMatrix[l,],
         ylim = c(0, 10e-6),
         main = "",
         probability = T,
         nclass = nClassMatrix[k,l],
         xlab = "",
         cex.lab = cexLabConstant,
         
         axes=F);
    #true price
    abline(v = truePrice, lty = 2, col = 'red', lwd = 2)
    
    title(main = paste("Model", k, "predictive distribution,", 
                       "\nobservation", rownames(testSet)[targetObs]), 
          cex.main = cexMainConstant,
          line = -2.5)
    
    title(xlab = "Price", cex.lab = cexLabConstant, line = 8)
    
    # only left
    if(l == 1) {
      axis(side = 2,
           at = c(0, 10e-6), #seq(from = 0, to = 0.000125, length.out = 5),
           labels = c("0", "1e-5"),
           las = 2,
           cex.axis = cexAxisConstant);
    }
    
    
    # only bottom
    if(k == 5) {
      axis(side = 1,
           at = round(seq(from = xLimitMatrix[l,1], to = xLimitMatrix[l,2], length.out = 5)),
           las = 2, 
           cex.axis = cexAxisConstant);
    }
    
    abline(h=0);
  }  
}
par(mfrow=c(1,1)); 
par(mar = c(5.1, 4.1, 4.1, 2.1))

dev.off()
















##############################################################################
# model comparison statistics 

modelFitFilenames <- c('/home/asdf/Desktop/gradu/git/housingprices/model 1 simple regression/modelFit1.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 2 simple varying intercept model/modelFit2.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 3 varying intercept model with ocean, road distance/modelFit3.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 4 gaussian process intercepts/modelFit4.RData',
                       '/home/asdf/Desktop/gradu/git/housingprices/model 5 varying intercepts, varying slopes/PSISLOOfix.RData' # 
);

listOfElpdLoo <- list();
listOfP_loo <- list();


counter <- 1; 
for(filename in modelFitFilenames) {
  cat("handling", filename, "...\n"); 
  load(filename)
  
  posteriorSamples.trueData <- as.matrix(stanFit.trueData)
  logLikSamples <- posteriorSamples.trueData[,grep("log_lik", colnames(posteriorSamples.trueData))];
  
  likSamples <- exp(logLikSamples)
  
  meanLogLikelihood.lppd <- colMeans(likSamples)
  
  lppd <- sum(log(meanLogLikelihood.lppd))
  
  # fix for the PSIS-LOO for k > 0.7 - only relevant for model 5... 
  elpd_loo_manual <- sum(pointwiseElpdLooVectorForStacking); 
  p_loo_manual <- lppd - elpd_loo_manual;
  
  listOfElpdLoo[[counter]] <- elpd_loo_manual;
  listOfP_loo[[counter]] <- p_loo_manual;
  
  counter <- counter + 1; 
}

elpdVec <- do.call(c, listOfElpdLoo);
p_looVec <- do.call(c, listOfP_loo);

library(xtable)
xtable(data.frame(t(elpdVec)))
xtable(data.frame(t(p_looVec)))

