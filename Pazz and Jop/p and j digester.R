setwd("C:/Users/Seth/Documents/bandatablog/Pazz and Jop")

#year <- as.character(as.numeric(year)+1)
#year <- "1994"
#load data
#pj <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
#                       header=F, stringsAsFactors=F)

pj <- list()
pjM <- data.frame()
year <- "1973"
for (i in 1:34) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    pj[[i]] <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
                           header=F, stringsAsFactors=F)
    #deal with asterisks
    pj[[i]] <- pj[[i]][,1:4]
    #add year
    pj[[i]]$year <- year
    pjM <- rbind(pjM, pj[[i]])
}
#write.csv(wnoM, "weekly number ones.csv", row.names=F)

#for (i in 1:length(pj)) {
#    pjM <- rbind(pjM, pj[[i]])
#}

#get rid of rogue asterisks
pjM <- pjM[!pjM$V1=="*", ]
pjM <- pjM[!pjM$V1=="**", ]

#fix rows that we screwed up by asterisks
for (i in 1:nrow(pjM)) {
    if(grepl("^[^0-9+\\.]", pjM[i,1])) {
        pjM[i,4] <- pjM[i,3]
        pjM[i,3] <- pjM[i,2]
        pjM[i,2] <- pjM[i,1]
        pjM[i,1] <- as.character(as.numeric(pjM[i-1,1])+1)
    }
}

#split artist and album columns
albums <- character()
for (i in 1:nrow(pjM)) {
    if(grepl(":", pjM[i,2])) {
        info <- unlist(strsplit(pjM[i,2], ": "))
        pjM[i,2] <- info[1]
        albums[i] <- info[2]
    } else {
        albums[i] <- pjM[i,2]
    } 
}

pjM <- data.frame(rank=pjM$V1, 
                  artist=pjM$V2, 
                  album=albums,
                  points=pjM$V3,
                  mentions=pjM$V4,
                  year=pjM$year,
                  stringsAsFactors=F)

#make rank numeric
pjM$rank <- as.numeric(pjM$rank)

#makes the year Date format, but just gives it whatever day it is currently
#pjM$year <- as.Date(pjM$year, "%Y")

#write.csv(pjM, "pazz and jop 1974-2007.csv", row.names=F)