setwd("C:/Users/Seth/Documents/bandatablog/Pazz and Jop/singles")

#got info from
#1979-2007
#http://www.robertchristgau.com/xg/pnj/index.php
#2008-present
#http://www.villagevoice.com/pazznjop/singles/2008

### 1974-2007 (the rest is below)
pj <- list()
pjsM <- data.frame()
year <- "1978"
for (i in 1:29) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    pj[[i]] <- read.delim(paste("pj singles ", year, ".txt", sep=""), 
                           header=F, stringsAsFactors=F)
    #deal with erroneous bump overs
    for (k in 1:nrow(pj[[i]])) {
        if(grepl("[^0-9\\.]", pj[[i]][k,1])) {
            pj[[i]][k,4] <- pj[[i]][k,3]
            pj[[i]][k,3] <- pj[[i]][k,2]
            pj[[i]][k,2] <- pj[[i]][k,1]
            pj[[i]][k,1] <- as.character(as.numeric(pj[[i]][k-1,1]))
        }
    }
    #deal with ties
    for (j in 1:nrow(pj[[i]])) {
        if(!grepl("[0-9]", pj[[i]][j,1])) {
            pj[[i]][j,4] <- pj[[i]][j,3]
            pj[[i]][j,3] <- pj[[i]][j,2]
            pj[[i]][j,2] <- pj[[i]][j,1]
            pj[[i]][j,1] <- pj[[i]][j-1,1]
        }
    }
    #deal with asterisks
    pj[[i]] <- pj[[i]][,1:3]
    #add year
    pj[[i]]$year <- year
    #add to master list
    pjsM <- rbind(pjsM, pj[[i]])
}
#get rid of rogue asterisks
pjsM[571,2] <- "Nelly: (Hot Shit) Country Grammar (Universal)"
pjsM <- pjsM[!grepl("\\*", pjsM$V2),]

#split artist and song columns
songs <- character()
for (i in 1:nrow(pjsM)) {
    if(grepl(":", pjsM[i,2])) {
        info <- unlist(strsplit(pjsM[i,2], ": "))
        pjsM[i,2] <- info[1]
        songs[i] <- info[2]
    } else {
        songs[i] <- pjsM[i,2]
    } 
}

pjsM <- data.frame(rank=as.numeric(pjsM$V1), 
                  artist=pjsM$V2, 
                  song=songs,
                  votes=pjsM$V3,
                  year=pjsM$year,
                  stringsAsFactors=F)


#deal with of label
for (i in 1:nrow(pjsM)) {
    boss <- unlist(strsplit(pjsM$song[i], " \\("))
    if(length(boss)==2) {
        pjsM$song[i] <- boss[1] 
        pjsM$label[i] <- boss[2]
    } else if (length(boss)>2) {
        pjsM$song[i] <- paste(boss[1:length(boss)-1], collapse= " (")
        pjsM$label[i] <- boss[length(boss)]
    }
}
pjsM$label <- gsub("\\)", "", pjsM$label)


#makes the year Date format, but just gives it whatever day it is currently
#pjsM$year <- as.Date(pjsM$year, "%Y")

#write.csv(pjsM, "pazz and jop 1974-2007.csv", row.names=F)

## 2008-2014
year <- "2007"
for (i in 35:41) {
    #set year
    year <- as.character(as.numeric(year)+1)
    #load data
    pjN <- read.delim(paste("pj singles ", year, ".txt", sep=""), 
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
                          song=pj1$V3,
                          votes=pj2$V2,
                          year=rep(year, nrow(pj1)),
                          label=pj2$V1,
                          #index=seq(nrow(pjsM)+1, by=1, length.out=40),
                          stringsAsFactors=F)
    # attach to master
    pjsM <- rbind(pjsM, pj[[i]])
}

####GOT TO HERE

#creating album index
pjsM$index <- seq(1, nrow(pjsM))

#get rid of NA's
pjsM <- pjsM[!is.na(pjsM[,3]),]

#fixing duplicate indices
dups <- pjsM[duplicated(pjsM$song),]
dupssong <- dups$song

#manually doing it

#look at them individually with this
#pjsM[pjsM$song==dupssong[1],]

###we could do this automatically, but there might be two different albums with the same name
###and we wouldn't want to accidentally give them the same index
### pjsM[pjsM$album==dupsalb[13],] for instance
pjsM[83,7] <- pjsM[60,7]
pjsM[158,7] <- pjsM[142,7]
pjsM[263,7] <- pjsM[258,7]
pjsM[320,7] <- pjsM[305,7]
pjsM[343,7] <- pjsM[330,7]
pjsM[448,7] <- pjsM[430,7]
pjsM[491,7] <- pjsM[462,7]
pjsM[596,7] <- pjsM[563,7]
pjsM[699,7] <- pjsM[640,7]
pjsM[721,7] <- pjsM[690,7]
pjsM[735,7] <- pjsM[703,7]
pjsM[753,7] <- pjsM[713,7]
pjsM[754,7] <- pjsM[714,7]
pjsM[783,7] <- pjsM[720,7]
pjsM[843,7] <- pjsM[819,7]
pjsM[866,7] <- pjsM[809,7]
pjsM[875,7] <- pjsM[840,7]
pjsM[1082,7] <- pjsM[1049,7]
pjsM[1141,7] <- pjsM[1109,7]


#setwd("C:/Users/Seth/Documents/bandatablog/data")
#write.csv(pjsM, "pj singles 1979-2014.csv", row.names=F)
