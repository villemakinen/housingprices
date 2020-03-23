
# kaupunginosien luonnostelua 11.11.

nimet <- interceptCoefTable$namesFromIndeces

skaalattuVektori.Intercept <- interceptCoefTable$mean/(max(interceptCoefTable$mean)); 
skaalattuVektori.Sqm <- sqmCoefTable$mean/(max(sqmCoefTable$mean)); 
skaalattuVektori.GoodCondSqm <- goodCondSqmCoefTable$mean/(max(goodCondSqmCoefTable$mean)); 

skaalattuVektori.Intercept <- skaalattuVektori.Intercept[order(interceptCoefTable$mean)]; 
skaalattuVektori.Sqm <- skaalattuVektori.Sqm[order(sqmCoefTable$mean)]
skaalattuVektori.GoodCondSqm <- skaalattuVektori.GoodCondSqm[order(goodCondSqmCoefTable$mean)]

nimet.InterceptJarj <- nimet[order(interceptCoefTable$mean)]; 
nimet.SqmJarj <- nimet[order(sqmCoefTable$mean)];
nimet.GoodCondSqmJarj <- nimet[order(goodCondSqmCoefTable$mean)]; 


arvoVektoriLista <- list(skaalattuVektori.Intercept, 
                         skaalattuVektori.Sqm, 
                         skaalattuVektori.GoodCondSqm)

vertailuDf <- data.frame(nimet.InterceptJarj, nimet.SqmJarj, nimet.GoodCondSqmJarj)


plottausApu <- function(k, arvoVektoriLista) {
  
  nimi <- as.character(nimet[k])

  cat("handling", nimi, "\n")
  
  segments(x0 = 1, y0 = arvoVektoriLista[[1]][nimet.InterceptJarj == nimi], x1 = 2, y1 = arvoVektoriLista[[2]][nimet.SqmJarj == nimi])
  segments(x0 = 2, y0 = arvoVektoriLista[[2]][nimet.SqmJarj == nimi], x1 = 3, y1 = arvoVektoriLista[[3]][nimet.GoodCondSqmJarj == nimi])
}

library(hash)
neighborhoodToCityHash <- hash::hash(keys = c('Alppiharju','Askisto','Asola','Eira','Espoon keskus','Espoonlahti','Etu-Töölö','Haaga','Hakunila','Hämeenkylä','Hämevaara','Haukilahti','Havukoski','Henttaa','Hermanni','Herttoniemi','Hiekkaharju','Ilola','Itä-Hakkila','Järvenperä','Jokiniemi','Kaarela','Kaartinkaupunki','Kaitaa','Kaivoksela','Kallio','Kamppi','Käpylä','Karakallio','Karhusuo','Karvasmäki','Katajanokka','Kauklahti','Kaupunginkallio','Keimola','Kilo','Kivistö','Kluuvi','Koivuhaka','Koivukylä','Kolmperä','Konala','Korso','Koskela','Kruununhaka','Kulosaari','Kumpula','Kuninkaala','Kuninkaanmäki','Kuurinniitty','Laajalahti','Laajasalo','Laakso','Laaksolahti','Lahnus','Länsimäki','Länsisatama','Latokaski','Lauttasaari','Leppäkorpi','Leppävaara','Lintuvaara','Lippajärvi','Malmi','Mankkaa','Martinlaakso','Matari','Matinkylä','Meilahti','Mellunkylä','Metsola','Mikkola','Munkkiniemi','Muurala','Myyrmäki','Niipperi','Niittykumpu','Nikinmäki','Nöykkiö','Nupuri','Olari','Otaniemi','Oulunkylä','Päiväkumpu','Pakila','Pakkala','Pasila','Perusmäki','Piispankylä','Pitäjänmäki','Pohjois-Tapiola','Pukinmäki','Punavuori','Rajakylä','Rekola','Ruskeasanta','Ruskeasuo','Saunalahti','Sepänkylä','Simonkylä','Sörnäinen','Soukka','Suurmetsä','Suutarila','Taka-Töölö','Tammisalo','Tammisto','Tapaninkylä','Tapiola','Tikkurila','Toukola','Tuomarinkylä','Ullanlinna','Vaarala','Vallila','Vanhakaupunki','Vantaanlaakso','Vanttila','Vapaala','Varisto','Vartiokylä','Viertola','Vierumäki','Viherlaakso','Viikki','Vuosaari','Westend','Ylästö'),
                               values = c('Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Espoo','Vantaa','Helsinki','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Espoo','Espoo','Espoo','Helsinki','Espoo','Espoo','Vantaa','Espoo','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Vantaa','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Vantaa','Espoo','Espoo','Helsinki','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Vantaa','Espoo','Espoo','Espoo','Helsinki','Espoo','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Helsinki','Espoo','Vantaa','Espoo','Espoo','Vantaa','Espoo','Espoo','Espoo','Espoo','Helsinki','Vantaa','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Vantaa','Vantaa','Vantaa','Helsinki','Espoo','Espoo','Vantaa','Helsinki','Espoo','Helsinki','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Espoo','Vantaa','Helsinki','Helsinki','Helsinki','Vantaa','Helsinki','Helsinki','Vantaa','Espoo','Vantaa','Vantaa','Helsinki','Vantaa','Vantaa','Espoo','Helsinki','Helsinki','Espoo','Vantaa')) 


nimet <- as.character(nimet)
kaupungit <- sapply(X = nimet, FUN = function(nimi) neighborhoodToCityHash[[nimi]])

indeksiVektori.Helsinki <- (1:128)[kaupungit == "Helsinki"]
indeksiVektori.Espoo <- (1:128)[kaupungit == "Espoo"]
indeksiVektori.Vantaa <- (1:128)[kaupungit == "Vantaa"]


par(mfrow=c(1,3)); 

plot(rep(1, 128), arvoVektoriLista[[1]], xlim = c(0, 4), ylim = c(0, 1.1), cex = 0.35, axes  = F, ylab = "", xlab = "", main = "Helsinki")
axis(side = 1, at = 1:3, labels = c("intercept", "sqm", "good cond sqm"), pch = 0.8)
points(rep(2, 128), arvoVektoriLista[[2]], cex = 0.35)
points(rep(3, 128), arvoVektoriLista[[3]], cex = 0.35)

for(k in indeksiVektori.Helsinki) {
  plottausApu(k, arvoVektoriLista = arvoVektoriLista)
}


plot(rep(1, 128), arvoVektoriLista[[1]], xlim = c(0, 4), ylim = c(0, 1.1), cex = 0.35, axes  = F, ylab = "", xlab = "", main = "Espoo")
axis(side = 1, at = 1:3, labels = c("intercept", "sqm", "good cond sqm"), pch = 0.8)
points(rep(2, 128), arvoVektoriLista[[2]], cex = 0.35)
points(rep(3, 128), arvoVektoriLista[[3]], cex = 0.35)

for(k in indeksiVektori.Espoo) {
  plottausApu(k, arvoVektoriLista = arvoVektoriLista)
}


plot(rep(1, 128), arvoVektoriLista[[1]], xlim = c(0, 4), ylim = c(0, 1.1),  cex = 0.35, axes  = F, ylab = "", xlab = "", main = "Vantaa")
axis(side = 1, at = 1:3, labels = c("intercept", "sqm", "good cond sqm"), pch = 0.8)
points(rep(2, 128), arvoVektoriLista[[2]], cex = 0.35)
points(rep(3, 128), arvoVektoriLista[[3]], cex = 0.35)

for(k in indeksiVektori.Vantaa) {
  plottausApu(k, arvoVektoriLista)
}


par(mfrow=c(1,1));




nimet <- as.character(interceptCoefTable$namesFromIndeces)
nimet <- nimet[order(interceptCoefTable$mean)]
kaupungit <- sapply(X = nimet, FUN = function(nimi) neighborhoodToCityHash[[nimi]])
plot(skaalattuVektori.Intercept, col = as.numeric(as.factor(kaupungit)), ylim = c(0,1))
legend("top", legend = levels(as.factor(kaupungit)), fill = 1:3)



nimet <- as.character(interceptCoefTable$namesFromIndeces)
nimet <- nimet[order(sqmCoefTable$mean)]
kaupungit <- sapply(X = nimet, FUN = function(nimi) neighborhoodToCityHash[[nimi]])
plot(skaalattuVektori.Sqm, col = as.numeric(as.factor(kaupungit)), ylim = c(0,1))
legend("top", legend = levels(as.factor(kaupungit)), fill = 1:3)





nimet <- as.character(interceptCoefTable$namesFromIndeces)
nimet <- nimet[order(goodCondSqmCoefTable$mean)]
kaupungit <- sapply(X = nimet, FUN = function(nimi) neighborhoodToCityHash[[nimi]])
plot(skaalattuVektori.GoodCondSqm, col = as.numeric(as.factor(kaupungit)), ylim = c(0,1))
legend("top", legend = levels(as.factor(kaupungit)), fill = 1:3)



     