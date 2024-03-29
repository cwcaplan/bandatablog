setwd("C:/Users/Seth/Documents/bandatablog/Pazz and Jop/albums")

#got info from

#2008-present
#http://www.villagevoice.com/pazznjop/albums/2008
#http://www.villagevoice.com/pazznjop/singles/2008

#year <- as.character(as.numeric(year)+1)
#year <- "1994"
#load data
#pj <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
#                       header=F, stringsAsFactors=F)

### 1974-2007 (the rest is below)
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
    #add to master list
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

#get rid of label
for (i in 1:nrow(pjM)) {
    pjM$label[i] <- unlist(strsplit(pjM$album[i], " \\("))[2]
    pjM$album[i] <- unlist(strsplit(pjM$album[i], " \\("))[1]
}
pjM$label <- gsub("\\)", "", pjM$label)

#makes the year Date format, but just gives it whatever day it is currently
#pjM$year <- as.Date(pjM$year, "%Y")

#write.csv(pjM, "pazz and jop 1974-2007.csv", row.names=F)

#creating album index
pjM$index <- seq(1, nrow(pjM))

## 2008-2014
year <- "2007"
for (i in 35:41) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    pjN <- read.delim(paste("pazz and jop ", year, ".txt", sep=""), 
                      header=F, stringsAsFactors=F)
    #split every other line 
    pj1 <- pjN[seq(1,79,2),]
    pj2 <- pjN[seq(2,80,2),]
    #split artist and album
    pj1[,2] <- sub(", ", "~", pj1[,2])
    trim.trailing <- function (x) sub("\\s+$", "", x)
    for (j in 1:nrow(pj1)) {
        both <- unlist(strsplit(pj1[j,2], "~"))
        pj1[j,2] <- trim.trailing(both[1])
        pj1[j,3] <- trim.trailing(both[2])
    }
    # build data frame for year
    pj[[i]] <- data.frame(rank=pj1$V1, 
                          artist=pj1$V2, 
                          album=pj1$V3,
                          points=pj2$V2,
                          mentions=pj2$V3,
                          year=rep(year, nrow(pj1)),
                          label=pj2$V1,
                          index=seq(nrow(pjM)+1, by=1, length.out=40),
                          stringsAsFactors=F)
    # attach to master
    pjM <- rbind(pjM, pj[[i]])
}

#fixing duplicate indices
dups <- pjM[duplicated(pjM$album),]
dupsalb <- dups$album

#manually doing it

#look at them individually with this
#pjM[pjM$album==dupsalb[1],]

###we could do this automatically, but there might be two different albums with the same name
###and we wouldn't want to accidentally give them the same index
### pjM[pjM$album==dupsalb[13],] for instance
pjM[202,8] <- pjM[108,8]
pjM[204,8] <- pjM[169,8]
pjM[294,8] <- pjM[271,8]
pjM[313,8] <- pjM[286,8]
pjM[326,8] <- pjM[300,8]
pjM[337,8] <- pjM[277,8]
pjM[446,8] <- pjM[417,8]
pjM[454,8] <- pjM[432,8]
pjM[481,8] <- pjM[465,8]
pjM[534,8] <- pjM[504,8]
pjM[606,8] <- pjM[575,8]
pjM[1021,8] <- pjM[990,8]
pjM[1288,8] <- pjM[1269,8]

#write.csv(pjM, "pazz and jop 1974-2014.csv", row.names=F)