

rm(list=ls()); gc(); dev.off(); 

library(rvest)    

setwd('/home/asdf/Desktop/gradu/')

#############################
# fetching the data

cityNames <- c('Helsinki', 'Espoo', 'Vantaa'); 

cityData <- list()

# fetching commented out

# run on 23.12.2018
# for(city in cityNames) {
#   startPartURL <- paste('http://asuntojen.hintatiedot.fi/haku/?c=', city, '&cr=1&t=3&l=0&z=', sep = "")
#   endPartURL <- '&search=1&sf=0&so=a'
#   
#   rawPages <- list(); 
#   pageDataFrames <- list(); 
#   k <- 1; 
#   while(TRUE) {
#     cat("k:", k, "\n"); 
#     listObjectToAdd <- list(); 
#     
#     fullURL <- paste(startPartURL, k, endPartURL, sep ="")
#     cat('    **** getting', fullURL, '...\n')
#     
#     retrievedPage <- read_html(fullURL); 
#     
#     rawPages[[k]] <- retrievedPage; 
#     
#     oddClass <- retrievedPage %>% html_nodes('.odd')
#     
#     # no observations found => breaking, updating 
#     if(length(oddClass) == 1) {
#       cat('    **** no observations retrieved (length(oddClass) == 1) \n')
#       break; 
#     }
#     
#     singleObs <- oddClass[2] %>%  html_nodes('tr') %>% html_text()  
#     
#     # removing non-observations
#     singleObs <- singleObs[nchar(singleObs) > 100]; 
#     
#     singleObs <- gsub(x = singleObs, pattern =  "\"", replacement = "")
#     singleObs <- gsub(x = singleObs, pattern =  '"', replacement = '')
#   
#     pageDataFrames[[k]] <- data.frame(t(sapply(singleObs, function(x) unlist(strsplit(x, '\n\t\t\t\t\t\t\t')))), row.names = NULL); 
#     
#     evenClass <- retrievedPage %>% html_nodes('.even')
#     
#     if(length(evenClass) > 0) {
#       singleObs <- evenClass %>%  html_nodes('tr') %>% html_text();
#       
#       # removing non-observations
#       singleObs <- singleObs[nchar(singleObs) > 100]; 
#       
#       pageDataFrames[[k]] <- rbind(pageDataFrames[[k]], 
#                                    data.frame(t(sapply(singleObs, function(x) unlist(strsplit(x, '\n\t\t\t\t\t\t\t')))), row.names = NULL));
#     }
#     
#     k <- k + 1; 
#   }
#   cityData[[city]] <- list(rawPages, pageDataFrames)
# }
# 
# # combining the data by dumping everything to a .csv-file
# for(city in cityNames) {
#   singleCityData <- cityData[[city]]; 
#   dataFrameList <- singleCityData[[2]]
#   
#   fileName <- paste(city, '.csv', sep=""); 
#     
#   for(k in 1:length(dataFrameList)) {
#     dfToWrite <- dataFrameList[[k]]; 
#     
#     write.table(x = dfToWrite, 
#                 file = fileName, 
#                 append = T, 
#                 row.names = F, 
#                 col.names = F, 
#                 sep = ";")
#   }
# }


# combining the data for each city
helsinkiData <- read.csv2("./asuntojen hintatiedot - gradu aineisto/Helsinki.csv", header = F);
vantaaData <- read.csv2("./asuntojen hintatiedot - gradu aineisto/Vantaa.csv", header = F);
espooData <- read.csv2("./asuntojen hintatiedot - gradu aineisto/Espoo.csv", header = F);

combinedData <- rbind(helsinkiData, vantaaData, espooData)

colnames(combinedData) <- c("NeighborhoodRaw", 
                            "ApartmentTypeRaw", 
                            "HouseType", 
                            "SquareMeters", 
                            "Price",
                            "PricePerSquareMeters",
                            "YearBuilt", 
                            "FloorRaw", 
                            "ElevatorRaw",
                            "ConditionRaw", 
                            "EnergyClassificationRaw")

#############################
# cleaning the data 
#   - these steps have to be modified if run later on  


# neighborhood identificator

library(hash)

write.csv2(sort(unique(tolower(as.character(combinedData$NeighborhoodRaw)))), "neigh.csv")
table(tolower(as.character(combinedData$NeighborhoodRaw)))

neighborHoodKey <- hash(keys = c('not recorded',
                                 'ala-malmi',
                                 'alppiharju',
                                 'alppila',
                                 'arabia',
                                 'arabianranta',
                                 'askisto',
                                 'asola',
                                 'aurinkolahti',
                                 'bemböle',
                                 'brunak�rr',
                                 'eestinlaakso',
                                 'eira',
                                 'eiranranta',
                                 'espoon keskus',
                                 'espoonlahti',
                                 'etelä-haaga',
                                 'etel�-haaga',
                                 'etu-töölö',
                                 'fallpakka',
                                 'friisilä',
                                 'haaga',
                                 'hakaniemi',
                                 'hakunila',
                                 'hakuninmaa',
                                 'hämeenkylä',
                                 'hämevaara',
                                 'hannus',
                                 'hannusjärvi',
                                 'harju',
                                 'haukilahti',
                                 'havukoski',
                                 'heikinlaakso',
                                 'helsinki',
                                 'henttaa',
                                 'hermanni',
                                 'hernesaari, eira',
                                 'herttoniemenranta',
                                 'herttoniemi',
                                 'hevossalmi',
                                 'hiekkaharju',
                                 'hietalahti',
                                 'hietalahti, kamppi',
                                 'honkasuo',
                                 'hyljelahti',
                                 'iirislahti',
                                 'iivisniemi',
                                 'ilmala',
                                 'ilola',
                                 'itä-hakkila',
                                 'itäkeskus',
                                 'itä-pakila',
                                 'itä-pasila',
                                 'jakomäki',
                                 'järvenperä',
                                 'jätkäsaari',
                                 'jokiniemi',
                                 'jollas',
                                 'joupinmäki',
                                 'jouppi',
                                 'jupperi',
                                 'kaarela',
                                 'kaartinkaupunki',
                                 'kaisaniemi',
                                 'kaitaa',
                                 'kaivoksela',
                                 'kalajärvi',
                                 'kalasatama',
                                 'kallahti',
                                 'kallio',
                                 'kallio, harju',
                                 'kallio, torkkelinmäki',
                                 'kamppi',
                                 'kamppi, bulevardi',
                                 'kamppi hietalahti',
                                 'kannelmäki',
                                 'kannelm�ki',
                                 'kannisto',
                                 'käpylä',
                                 'karakallio',
                                 'kartanonkoski',
                                 'katajanokka',
                                 'kauklahti',
                                 'kaupunginkallio',
                                 'kavallinmäki',
                                 'keimolanmäki',
                                 'keski-töölö',
                                 'keskusta',
                                 'kilo',
                                 'kirkkojärvi',
                                 'kirkkoj�rvi',
                                 'kirstinharju',
                                 'kirstinmäki',
                                 'kivenlahti',
                                 'kivihaka',
                                 'kivikko',
                                 'kivimäki',
                                 'kivist�',
                                 'kivistö',
                                 'kluuvi',
                                 'koivuhaka',
                                 'koivuhovi',
                                 'koivukylä',
                                 'kolmperä',
                                 'konala',
                                 'konepaja, vallila',
                                 'kontula',
                                 'korso',
                                 'koskela',
                                 'koukkuniemi',
                                 'kruununhaka',
                                 'kuitinmäki',
                                 'kukkumäki',
                                 'kulomäki',
                                 'kulosaari',
                                 'kulotorppa',
                                 'kumpula',
                                 'kuninkaanmäki',
                                 'kuninkaantammi',
                                 'kuninkainen',
                                 'kurkimäki',
                                 'kuurinniitty',
                                 'kuusikko',
                                 'kuusisaari',
                                 'laajalahti',
                                 'laajasalo',
                                 'laakso',
                                 'laaksolahti',
                                 'lähderanta',
                                 'lahnus',
                                 'lansa',
                                 'lansankallio',
                                 'länsimäki',
                                 'länsi-pakila',
                                 'länsi-pasila',
                                 'lassila',
                                 'latokaski',
                                 'laurinlahti',
                                 'lauttaaari',
                                 'lauttasaari',
                                 'lauttasaari, vattuniemi',
                                 'lehtisaari',
                                 'leinelä',
                                 'leppäkorpi',
                                 'leppäsilta, leppävaara ',
                                 'leppävaara',
                                 'leppävaara / vallikallio',
                                 'lepp�vaara',
                                 'lepp�vaara, puustellinm�ki',
                                 'lintulahti',
                                 'lintuvaara',
                                 'lippajärvi',
                                 'louhela',
                                 'lystimäki',
                                 'maarinkunnas',
                                 'maarukka',
                                 'mäkkylä',
                                 'mäkkylä/ puustellinmäki',
                                 'malmi',
                                 'malminiitty',
                                 'malminkartano',
                                 'mankkaa',
                                 'marjaniemi',
                                 'martinlaakso',
                                 'matari',
                                 'matinkylä',
                                 'matinkylä ',
                                 'maunula',
                                 'maununneva',
                                 'meilahti',
                                 'mellunkylä',
                                 'mellunmäki',
                                 'merihaka',
                                 'meri-kivenlahti',
                                 'meri-rastila',
                                 'metsälä',
                                 'metsola',
                                 'miilukorpi',
                                 'mikkelä',
                                 'mikkola',
                                 'munkkiniemi',
                                 'munkkisaari',
                                 'munkkivuori',
                                 'muurala',
                                 'myllykylä',
                                 'myllypuro',
                                 'myyrmäki',
                                 'näkinpuisto',
                                 'niemenmäki',
                                 'niemenm�ki',
                                 'niipperi',
                                 'niittykumpu',
                                 'niittymaa',
                                 'nikinmäki',
                                 'nissas',
                                 'nöykkiö',
                                 'nuottalahti',
                                 'nuottaniemi',
                                 'nupuri',
                                 'olari',
                                 'otaniemi',
                                 'oulunkylä',
                                 'oulunkylä/veräjälaakso',
                                 'pähkinärinne',
                                 'painiitty',
                                 'päiväkumpu',
                                 'pajamäki',
                                 'pakila',
                                 'pakkala',
                                 'pakkalanrinne',
                                 'paloheinä',
                                 'pasila',
                                 'patola',
                                 'perkkaa',
                                 'perusmäki',
                                 'pihlajamäki',
                                 'pihlajisto',
                                 'piispankallio',
                                 'piispanmäki',
                                 'piispanpiha',
                                 'piispansilta',
                                 'pikku-huopalahti',
                                 'pisa',
                                 'pitäjänmäki',
                                 'pitkäniitty',
                                 'pohjois-haaga',
                                 'pohjois-leppävaara',
                                 'pohjois-tapiola',
                                 'puistola',
                                 'pukinmäki',
                                 'pukinm�ki',
                                 'punavuori',
                                 'puotila',
                                 'puotinharju',
                                 'rajakylä',
                                 'rajatorppa',
                                 'ramsinranta',
                                 'rastaala',
                                 'rastaspuisto',
                                 'rastila',
                                 'reimarla',
                                 'rekola',
                                 'roihuvuori',
                                 'ruoholahti',
                                 'ruskeasanta',
                                 'ruskeasuo',
                                 'saarnilaakso',
                                 'satomäki',
                                 'saunalahti',
                                 'saunaniemi',
                                 'savela',
                                 'sepänkylä',
                                 'sepänmäki',
                                 'siltamäki',
                                 'siltasaari',
                                 'silvola',
                                 'simonkallio',
                                 'simonkylä',
                                 'simonlaakso',
                                 'simonmetsä',
                                 'simonsilta',
                                 'sokinvuori',
                                 'sörnäinen',
                                 'soukanranta',
                                 'soukka',
                                 'suna',
                                 'suurmetsä',
                                 'suurpelto',
                                 'suutarila',
                                 'suvela',
                                 'taka-t��l�',
                                 'taka töölö',
                                 'taka-töölö',
                                 'tali',
                                 'tammisalo',
                                 'tammisto',
                                 'tapanila',
                                 'tapaninkylä',
                                 'tapaninvainio',
                                 'tapiola',
                                 'tapulikaupunki',
                                 'tiistilä',
                                 'tikkurila',
                                 'tikkurila / viertola',
                                 'tillinmäki',
                                 'tontunmäki',
                                 'töölö',
                                 'torkkelinmäki',
                                 'torpparinmäki',
                                 'toukola',
                                 'töyrynummi',
                                 'tuomarila',
                                 'ullanlinna',
                                 'ulrikanpuisto',
                                 'uusmäki',
                                 'vaarala',
                                 'vallikallio',
                                 'vallila',
                                 'vallila, hermanni',
                                 'vanhakaupunki',
                                 'vanha koivukylä',
                                 'vanhankaupunginkoski',
                                 'vanhankaupunginkoski, viikinranta',
                                 'vantaanlaakso',
                                 'vantaanpuisto',
                                 'vantaapuisto',
                                 'vanttila',
                                 'vapaala',
                                 'varisto',
                                 'vartioharju',
                                 'vartiokylä',
                                 'veräjälaakso',
                                 'veräjämäki',
                                 'vermo',
                                 'vesala',
                                 'vesirattaanmäki',
                                 'viertola',
                                 'vierumäki',
                                 'viherkallio',
                                 'viherlaakso',
                                 'viikinmäki',
                                 'viikki',
                                 'viikki, latokartano',
                                 'vuosaari',
                                 'vuosaari / rastilankallio',
                                 'westend',
                                 'ylä-malmi',
                                 'ylästö',
                                 'yliskylä',
                                 'ymmersta'),
                        values = c('not recorded',
                                   'Malmi',
                                   'Alppiharju',
                                   'Alppila',
                                   'Toukola',
                                   'Toukola',
                                   'Askisto',
                                   'Asola',
                                   'Vuosaari',
                                   'Karvasmäki',
                                   'Ruskeasuo',
                                   'Nöykkiö',
                                   'Eira',
                                   'Länsisatama',
                                   'Espoon keskus',
                                   'Espoonlahti',
                                   'Haaga',
                                   'Haaga',
                                   'Etu-Töölö',
                                   'Mellunkylä',
                                   'Olari',
                                   'Haaga',
                                   'Kallio',
                                   'Hakunila',
                                   'Kaarela',
                                   'Hämeenkylä',
                                   'Hämevaara',
                                   'Kaitaa',
                                   'Kaitaa',
                                   'Alppiharju',
                                   'Haukilahti',
                                   'Havukoski',
                                   'Suurmetsä',
                                   'Helsinki',
                                   'Henttaa',
                                   'Hermanni',
                                   'Länsisatama',
                                   'Herttoniemi',
                                   'Herttoniemi',
                                   'Laajasalo',
                                   'Hiekkaharju',
                                   'Kamppi',
                                   'Kamppi',
                                   'Kaarela',
                                   'Kaitaa',
                                   'Matinkylä',
                                   'Kaitaa',
                                   'Pasila',
                                   'Ilola',
                                   'Itä-Hakkila',
                                   'Vartiokylä',
                                   'Pakila',
                                   'Pasila',
                                   'Suurmetsä',
                                   'Järvenperä',
                                   'Länsisatama',
                                   'Jokiniemi',
                                   'Laajasalo',
                                   'Espoon keskus',
                                   'Espoon keskus',
                                   'Laaksolahti',
                                   'Kaarela',
                                   'Kaartinkaupunki',
                                   'Kluuvi',
                                   'Kaitaa',
                                   'Kaivoksela',
                                   'Kalajärvi',
                                   'Sörnäinen',
                                   'Vuosaari',
                                   'Kallio',
                                   'Kallio',
                                   'Kallio',
                                   'Kamppi',
                                   'Kamppi',
                                   'Kamppi',
                                   'Kaarela',
                                   'Kaarela',
                                   'Kivistö',
                                   'Käpylä',
                                   'Karakallio',
                                   'Pakkala',
                                   'Katajanokka',
                                   'Kauklahti',
                                   'Kaupunginkallio',
                                   'Viherlaakso',
                                   'Keimola',
                                   'Etu-Töölö',
                                   'Kamppi',
                                   'Kilo',
                                   'Espoon keskus',
                                   'Espoon keskus',
                                   'Espoon keskus',
                                   'Espoon keskus',
                                   'Espoonlahti',
                                   'Haaga',
                                   'Mellunkylä',
                                   'Martinlaakso',
                                   'Kivistö',
                                   'Kivistö',
                                   'Kluuvi',
                                   'Koivuhaka',
                                   'Espoon keskus',
                                   'Koivukylä',
                                   'Kolmperä',
                                   'Konala',
                                   'Vallila',
                                   'Mellunkylä',
                                   'Korso',
                                   'Koskela',
                                   'Matinkylä',
                                   'Kruununhaka',
                                   'Olari',
                                   'Latokaski',
                                   'Korso',
                                   'Kulosaari',
                                   'Espoon keskus',
                                   'Kumpula',
                                   'Kuninkaanmäki',
                                   'Kaarela',
                                   'Kilo',
                                   'Mellunkylä',
                                   'Kuurinniitty',
                                   'Kuninkaala',
                                   'Munkkiniemi',
                                   'Laajalahti',
                                   'Laajasalo',
                                   'Laakso',
                                   'Laaksolahti',
                                   'Laaksolahti',
                                   'Lahnus',
                                   'Kilo',
                                   'Kilo',
                                   'Länsimäki',
                                   'Pakila',
                                   'Pasila',
                                   'Haaga',
                                   'Latokaski',
                                   'Espoonlahti',
                                   'Lauttasaari',
                                   'Lauttasaari',
                                   'Lauttasaari',
                                   'Munkkiniemi',
                                   'Koivukylä',
                                   'Leppäkorpi',
                                   'Leppävaara',
                                   'Leppävaara',
                                   'Leppävaara',
                                   'Leppävaara',
                                   'Leppävaara',
                                   'Sörnäinen',
                                   'Lintuvaara',
                                   'Lippajärvi',
                                   'Myyrmäki',
                                   'Olari',
                                   'Jokiniemi',
                                   'Mikkola',
                                   'Leppävaara',
                                   'Leppävaara',
                                   'Malmi',
                                   'Simonkylä',
                                   'Kaarela',
                                   'Mankkaa',
                                   'Vartiokylä',
                                   'Martinlaakso',
                                   'Matari',
                                   'Matinkylä',
                                   'Matinkylä',
                                   'Oulunkylä',
                                   'Kaarela',
                                   'Meilahti',
                                   'Mellunkylä',
                                   'Mellunkylä',
                                   'Sörnäinen',
                                   'Espoonlahti',
                                   'Vuosaari',
                                   'Oulunkylä',
                                   'Metsola',
                                   'Karhusuo',
                                   'Muurala',
                                   'Mikkola',
                                   'Munkkiniemi',
                                   'Länsisatama',
                                   'Munkkiniemi',
                                   'Muurala',
                                   'Vartiokylä',
                                   'Vartiokylä',
                                   'Myyrmäki',
                                   'Sörnäinen',
                                   'Munkkiniemi',
                                   'Munkkiniemi',
                                   'Niipperi',
                                   'Niittykumpu',
                                   'Niittykumpu',
                                   'Nikinmäki',
                                   'Itä-Hakkila',
                                   'Nöykkiö',
                                   'Matinkylä',
                                   'Matinkylä',
                                   'Nupuri',
                                   'Olari',
                                   'Otaniemi',
                                   'Oulunkylä',
                                   'Oulunkylä',
                                   'Hämeenkylä',
                                   'Lintuvaara',
                                   'Päiväkumpu',
                                   'Pitäjänmäki',
                                   'Pakila',
                                   'Pakkala',
                                   'Pakkala',
                                   'Tuomarinkylä',
                                   'Pasila',
                                   'Oulunkylä',
                                   'Leppävaara',
                                   'Perusmäki',
                                   'Malmi',
                                   'Malmi',
                                   'Olari',
                                   'Olari',
                                   'Olari',
                                   'Matinkylä',
                                   'Ruskeasuo',
                                   'Nöykkiö',
                                   'Pitäjänmäki',
                                   'Karhusuo',
                                   'Haaga',
                                   'Leppävaara',
                                   'Pohjois-Tapiola',
                                   'Suurmetsä',
                                   'Pukinmäki',
                                   'Pukinmäki',
                                   'Punavuori',
                                   'Vartiokylä',
                                   'Vartiokylä',
                                   'Rajakylä',
                                   'Vapaala',
                                   'Vuosaari',
                                   'Karakallio',
                                   'Karakallio',
                                   'Vuosaari',
                                   'Pitäjänmäki',
                                   'Rekola',
                                   'Herttoniemi',
                                   'Länsisatama',
                                   'Ruskeasanta',
                                   'Ruskeasuo',
                                   'Espoon keskus',
                                   'Jokiniemi',
                                   'Saunalahti',
                                   'Saunalahti',
                                   'Pukinmäki',
                                   'Sepänkylä',
                                   'Malmi',
                                   'Suutarila',
                                   'Kallio',
                                   'Kaivoksela',
                                   'Simonkylä',
                                   'Simonkylä',
                                   'Simonkylä',
                                   'Simonkylä',
                                   'Simonkylä',
                                   'Espoon keskus',
                                   'Sörnäinen',
                                   'Soukka',
                                   'Soukka',
                                   'Espoon keskus',
                                   'Suurmetsä',
                                   'Henttaa',
                                   'Suutarila',
                                   'Espoon keskus',
                                   'Taka-Töölö',
                                   'Taka-Töölö',
                                   'Taka-Töölö',
                                   'Pitäjänmäki',
                                   'Tammisalo',
                                   'Tammisto',
                                   'Tapaninkylä',
                                   'Tapaninkylä',
                                   'Tapaninkylä',
                                   'Tapiola',
                                   'Suutarila',
                                   'Matinkylä',
                                   'Tikkurila',
                                   'Tikkurila',
                                   'Saunalahti',
                                   'Niittykumpu',
                                   'Etu-Töölö',
                                   'Kallio',
                                   'Tuomarinkylä',
                                   'Toukola',
                                   'Suutarila',
                                   'Espoon keskus',
                                   'Ullanlinna',
                                   'Rekola',
                                   'Lintuvaara',
                                   'Vaarala',
                                   'Leppävaara',
                                   'Vallila',
                                   'Vallila',
                                   'Vanhakaupunki',
                                   'Koivukylä',
                                   'Vanhakaupunki',
                                   'Vanhakaupunki',
                                   'Vantaanlaakso',
                                   'Piispankylä',
                                   'Piispankylä',
                                   'Vanttila',
                                   'Vapaala',
                                   'Varisto',
                                   'Vartiokylä',
                                   'Vartiokylä',
                                   'Oulunkylä',
                                   'Oulunkylä',
                                   'Leppävaara',
                                   'Mellunkylä',
                                   'Karhusuo',
                                   'Viertola',
                                   'Vierumäki',
                                   'Viherlaakso',
                                   'Viherlaakso',
                                   'Viikki',
                                   'Viikki',
                                   'Viikki',
                                   'Vuosaari',
                                   'Vuosaari',
                                   'Westend',
                                   'Malmi',
                                   'Ylästö',
                                   'Laajasalo',
                                   'Sepänkylä'));

combinedData$NeighborhoodKeys <- tolower(as.character(combinedData$NeighborhoodRaw));
combinedData$NeighborhoodKeys[combinedData$NeighborhoodKeys == ''] <- 'not recorded';

combinedData$NeighborhoodFinalized <- sapply(X = combinedData$NeighborhoodKeys, FUN = function(x) neighborHoodKey[[x]])
combinedData$NeighborhoodFinalized <- factor(combinedData$NeighborhoodFinalized)


# retrieving the apartment type information

combinedData$ApartmentTypeUniformized <- combinedData$ApartmentTypeRaw;
combinedData$ApartmentTypeUniformized <- as.character(combinedData$ApartmentTypeUniformized)
combinedData$ApartmentTypeUniformized <- tolower(combinedData$ApartmentTypeUniformized) 
combinedData$ApartmentTypeUniformized <- gsub(" ", "", combinedData$ApartmentTypeUniformized)
combinedData$ApartmentTypeUniformized <- gsub(",", "+", combinedData$ApartmentTypeUniformized)

library(stringr)

chop <- function(string) substr(string, 1, nchar(string)-1);
chopFirst <- function(string) substr(string, 2, nchar(string)); 

combinedData$NumberOfRooms <- as.numeric(chop(str_extract(combinedData$ApartmentTypeUniformized, '([0-9]+)h')))
combinedData$SaunaDummy <- 1*(!is.na(str_extract(combinedData$ApartmentTypeUniformized, '\\+s')))

# retrieving the floor information

combinedData$ownFloor <- as.numeric(chop(str_extract(combinedData$FloorRaw, '.+/')))
combinedData$maxFloor <- as.numeric(chopFirst(str_extract(combinedData$FloorRaw, '/.+')))

# elevator dummy construction
combinedData$ElevatorDummy <- 1*(combinedData$ElevatorRaw == "on")

# condition contrasts dummy construction
combinedData$ConditionUnrecordedDummy <- 1*(combinedData$ConditionRaw == " ")
combinedData$ConditionAdequateDummy <- 1*(combinedData$ConditionRaw == "tyyd.")
combinedData$ConditionGoodDummy <- 1*(combinedData$ConditionRaw == "hyvä")

# age of the building
combinedData$AgeOfTheBuilding <- 2018 - combinedData$YearBuilt; 

# energy classification contrasts dummy construction
combinedData$EnergyClassificationRaw <- gsub("\\n\\t\\t\\t\\t\\t\\t", "", combinedData$EnergyClassificationRaw)

getFirstLetter <- function(string) substr(string, 1, 1); 

combinedData$EnergyClassificationLetterClass <- sapply(combinedData$EnergyClassificationRaw, FUN = getFirstLetter)  

combinedData$EnergyClassificationADummy <- 1*(combinedData$EnergyClassificationLetterClass == 'A'); 
combinedData$EnergyClassificationBDummy <- 1*(combinedData$EnergyClassificationLetterClass == 'B'); 
combinedData$EnergyClassificationCDummy <- 1*(combinedData$EnergyClassificationLetterClass == 'C'); 
combinedData$EnergyClassificationDDummy <- 1*(combinedData$EnergyClassificationLetterClass == 'D'); 
combinedData$EnergyClassificationEDummy <- 1*(combinedData$EnergyClassificationLetterClass == 'E'); 
combinedData$EnergyClassificationFDummy <- 1*(combinedData$EnergyClassificationLetterClass == 'F');
combinedData$EnergyClassificationUnrecordedDummy <- 1*(combinedData$EnergyClassificationLetterClass == ''); 

# ad hoc fix for ownFloor

combinedData$ownFloor.fixed <- combinedData$ownFloor; 
combinedData$ownFloor.fixed[combinedData$ownFloor.fixed < 1 & !is.na(combinedData$ownFloor.fixed)] <- combinedData$maxFloor[combinedData$ownFloor.fixed < 1 & !is.na(combinedData$ownFloor.fixed)]; 

# dummies for apartment types

combinedData$TwoRoomsDummy <- 1*(combinedData$NumberOfRooms == 2);
combinedData$ThreeRoomsDummy <- 1*(combinedData$NumberOfRooms == 3);
combinedData$FourRoomsOrMoreDummy <- 1*(combinedData$NumberOfRooms >= 4); 

##################################################
# filters for missing data 
#     - a EM-type solution to utilize this data as well?

# requirement that the # of rooms is available 
combinedData <- combinedData[!is.na(combinedData$NumberOfRooms),];

# requirement that the floor is recorded => NA's dropped
combinedData <- combinedData[!is.na(combinedData$ownFloor.fixed),]

# requirement that the neighborhood is known
combinedData <- combinedData[combinedData$NeighborhoodFinalized != "not recorded",]
combinedData <- combinedData[combinedData$NeighborhoodFinalized != "Helsinki",]

combinedData$NeighborhoodFinalized <- factor(combinedData$NeighborhoodFinalized);

##################################################
# plotting for finalized data

# sauna
plot(combinedData$SaunaDummy, combinedData$Price)

par(mfrow=c(1,2))
hist(combinedData$Price[combinedData$SaunaDummy == 0])
abline(v = mean(combinedData$Price[combinedData$SaunaDummy == 0]), col = 'red')
hist(combinedData$Price[combinedData$SaunaDummy == 1])
abline(v = mean(combinedData$Price[combinedData$SaunaDummy == 1]), col = 'red')
par(mfrow=c(1,1))


# sauna
plot(combinedData$ElevatorDummy, combinedData$Price)

par(mfrow=c(1,2))
hist(combinedData$Price[combinedData$ElevatorDummy == 0])
abline(v = mean(combinedData$Price[combinedData$ElevatorDummy == 0]), col = 'red')
hist(combinedData$Price[combinedData$ElevatorDummy == 1])
abline(v = mean(combinedData$Price[combinedData$ElevatorDummy == 1]), col = 'red')
par(mfrow=c(1,1))


# floor 
plot(combinedData$ownFloor.fixed, combinedData$Price)
plot(combinedData$ownFloor.fixed, combinedData$Price, ylim = c(0, 1e6))
plot(combinedData$ownFloor.fixed, combinedData$Price, col = abs(combinedData$ElevatorDummy-1))
#   => there's apartments with floor > 6 with no elevator => most likely erroneous data entry 

plot(combinedData$AgeOfTheBuilding, combinedData$Price)

# scatters for all 
plotScatters <- function(k) {
  par(mfrow=c(2,2))
  
  neighborhood <- levels(combinedData$NeighborhoodFinalized)[k]; 
  plot(combinedData$AgeOfTheBuilding[combinedData$NeighborhoodFinalized == neighborhood], 
       combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood], 
       col = as.numeric(combinedData$ConditionRaw[combinedData$NeighborhoodFinalized == neighborhood]),
       main = "")
  
  x <- combinedData$AgeOfTheBuilding[combinedData$NeighborhoodFinalized == neighborhood]
  y <- combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood];
  
  
  regData <- data.frame(y, x)
  
  regMalli <- lm(y ~ x, data = regData)
  
  a <- coef(regMalli)[1]
  b <- coef(regMalli)[2]
  
  if(!is.infinite(a) & !is.infinite(b) & !is.na(a) & !is.na(b))
    abline(a = a, b = b, lty = 2, lwd = 0.5, col = 'gray'); 
  
  title(paste(neighborhood, ", age to price\n", "a = ", a, ", b = ", b, sep =""))
  
  ################################################
  
  plot(combinedData$SquareMeters[combinedData$NeighborhoodFinalized == neighborhood], 
       combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood], 
       col = as.numeric(combinedData$ConditionRaw[combinedData$NeighborhoodFinalized == neighborhood]),
       main = "")
  
  x <- combinedData$SquareMeters[combinedData$NeighborhoodFinalized == neighborhood];
  y <- combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood]
  
  regData <- data.frame(y, x)
  
  regMalli <- lm(y ~ x, data = regData)
  
  a <- coef(regMalli)[1]
  b <- coef(regMalli)[2]
  
  if(!is.infinite(a) & !is.infinite(b) & !is.na(a) & !is.na(b))
    abline(a = a, b = b, lty = 2, lwd = 0.5, col = 'gray'); 
  
  title(paste(neighborhood, ", sqm to price\n", "a = ", a, ", b = ", b, sep =""))
  
  ###############################################
  
  plot(combinedData$ownFloor.fixed[combinedData$NeighborhoodFinalized == neighborhood], 
       combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood], 
       col = as.numeric(combinedData$ConditionRaw[combinedData$NeighborhoodFinalized == neighborhood]),
       main = "")
  
  x <- combinedData$ownFloor.fixed[combinedData$NeighborhoodFinalized == neighborhood];
  y <- combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood]
  
  regData <- data.frame(y, x)
  
  regMalli <- lm(y ~ x, data = regData)
  
  a <- coef(regMalli)[1]
  b <- coef(regMalli)[2]
  
  if(!is.infinite(a) & !is.infinite(b) & !is.na(a) & !is.na(b))
    abline(a = a, b = b, lty = 2, lwd = 0.5, col = 'gray'); 
  
  title(paste(neighborhood, ", floor to price\n", "a = ", a, ", b = ", b, sep =""))
  
  ###############################################
  
  plot(combinedData$NumberOfRooms[combinedData$NeighborhoodFinalized == neighborhood], 
       combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood], 
       col = as.numeric(combinedData$ConditionRaw[combinedData$NeighborhoodFinalized == neighborhood]),
       main = "")
  
  x <- combinedData$NumberOfRooms[combinedData$NeighborhoodFinalized == neighborhood];
  y <- combinedData$Price[combinedData$NeighborhoodFinalized == neighborhood]
  
  regData <- data.frame(y, x)
  
  regMalli <- lm(y ~ x, data = regData)
  
  a <- coef(regMalli)[1]
  b <- coef(regMalli)[2]
  
  if(!is.infinite(a) & !is.infinite(b) & !is.na(a) & !is.na(b))
    abline(a = a, b = b, lty = 2, lwd = 0.5, col = 'gray'); 
  
  title(paste(neighborhood, ", number of rooms to price\n", "a = ", a, ", b = ", b, sep =""))
  
  
  par(mfrow=c(1,1))
}

for(k in 1:length(levels(combinedData$NeighborhoodFinalized))) {
  cat("k:", k, "\n")
  plotScatters(k);
  Sys.sleep(6);
}






##################################################
# linear spline variables

# generateSplineVariable <- function(breakpoint, 
#                                    originalVariable) {
#   shiftedVariable <- originalVariable - breakpoint; 
#   shiftedVariable[shiftedVariable < 0] <- 0; 
#   return(shiftedVariable)
# }
# 
# # square meters-variable
# knots.sqMeters <- quantile(x = combinedData$SquareMeters, probs = seq(0.05, 0.95, 0.05))
# splineVars.sqMeters <- sapply(X = knots.sqMeters, FUN = generateSplineVariable, originalVariable = combinedData$SquareMeters)
# colnames(splineVars.sqMeters) <- paste("SquareMetersSpline", colnames(splineVars.sqMeters), sep = "")
# 
# # age of the building
# knots.age <- quantile(x = combinedData$AgeOfTheBuilding, probs = seq(0.05, 0.95, 0.05))
# splineVars.age <- sapply(X = knots.age, FUN = generateSplineVariable, originalVariable = combinedData$AgeOfTheBuilding)
# colnames(splineVars.age) <- paste("AgeOfTheBuildingSpline", colnames(splineVars.age), sep = "")
# 
# # floor 
# # knots.floor <- quantile(x = combinedData$ownFloor.fixed, probs = seq(0.05, 0.95, 0.05))
# # knots.floor <- unique(knots.floor)
# 
# # knots for the floor variable chosen manually 
# knots.floor <- c(3, 4, 5, 6)
# splineVars.floor <- sapply(X = knots.floor, FUN = generateSplineVariable, originalVariable = combinedData$ownFloor.fixed)
# colnames(splineVars.floor) <- paste("ownFloor.fixedSpline", colnames(splineVars.floor), sep = "")
# 



##################################################
# saving the data

write.csv2(combinedData, "finalizedData29122018.csv")

